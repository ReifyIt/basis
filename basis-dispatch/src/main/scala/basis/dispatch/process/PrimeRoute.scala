/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch
package process

import basis.collections._
import basis.concurrent._
import basis.containers._

import scala.runtime.AbstractFunction1

trait PrimeRoute extends AsyncRoute { async: Async =>
  import MetaPrime._
  
  class Route extends RouteApi {
    private[process] final var control: RouteState = RouteState(this)
    
    final override def state: RouteState = control
    
    final override val queue: AtomicQueue[() => _] = new AtomicQueue[() => _]
    
    /** Returns the target number of active threads. Configure the target
      * parallelism by setting the `basis.dispatch.async.parallelism` system
      * property to a natural number or a multiple of the available processors
      * (`x2`, `x4`, etc.)–defaults to the number of available processors (`x1`).
      * @group Classifying */
    override val parallelism: Int = {
      def parse(s: String): Int = {
        var i = 0
        val n = if (s != null) s.length else 0
        val k = if (0 < n && s.charAt(0) == 'x') { i = 1; Runtime.getRuntime.availableProcessors } else 1
        var p = 0
        while (i < n) {
          val c = s.charAt(i)
          if (c >= '0' && c <= '9') { p = 10 * p + (c - '0'); i += 1 }
          else { p = 0; i = n }
        }
        if (p > 0) k * p else Runtime.getRuntime.availableProcessors
      }
      parse(System.getProperty("basis.dispatch.async.parallelism"))
    }
    
    final override def rouse() {
      var c = null: RouteState
      do c = control
      while (!adapt(c, c.accelerate(parallelism)))
    }
    
    final override def adapt(oldState: RouteState, newState: RouteState): Boolean = {
      (oldState eq newState) || {
        var c = null: RouteState
        do c = control
        while ((c eq oldState) && !Unsafe.compareAndSwapObject(this, RouteControlOffset, c, newState))
        (c eq oldState) && { newState.signal(); true }
      }
    }
    
    override def uncaughtException(thread: Thread, exception: Throwable): Unit = exception.printStackTrace()
    
    override def apply[A](expr: => A): Relay[A] = {
      val to = new React[A](this)
      exec(new Thunk.Eval(expr, to))
      to
    }
    
    override def block[A](expr: => A): A = {
      val thread = Thread.currentThread
      if (thread.isInstanceOf[TrackApi]) thread.asInstanceOf[TrackApi].block(expr)
      else expr
    }
    
    override def exec[U](thunk: () => U) {
      val thread = Thread.currentThread
      if (thread.isInstanceOf[TrackApi]) thread.asInstanceOf[TrackApi].queue.push(thunk)
      else queue.push(thunk)
      rouse()
    }
    
    override def relay[A](thunk: () => A): Relay[A] = {
      val to = new React[A](this)
      exec(new Thunk.Exec(thunk, to))
      to
    }
    
    override def relayAll[A](thunks: Enumerator[() => A]): Relay[Batch[A]] = {
      val to = new Tally.JoinAll[A](this)
      thunks traverse new TallyAll(to, {
        val thread = Thread.currentThread
        if (thread.isInstanceOf[TrackApi]) thread.asInstanceOf[TrackApi].queue else queue
      })
      to.commit()
      rouse()
      to
    }
    
    override def relayAny[A](thunks: Enumerator[() => A]): Relay[A] = {
      val to = new Tally.JoinAny[A](this)
      thunks traverse new TallyAny(to, {
        val thread = Thread.currentThread
        if (thread.isInstanceOf[TrackApi]) thread.asInstanceOf[TrackApi].queue else queue
      })
      to.commit()
      rouse()
      to
    }
    
    override def relayFirst[A](thunks: Enumerator[() => A])(p: A => Boolean): Relay[A] = {
      val to = new Tally.JoinFirst(p)(this)
      thunks traverse new TallyAny(to, {
        val thread = Thread.currentThread
        if (thread.isInstanceOf[TrackApi]) thread.asInstanceOf[TrackApi].queue else queue
      })
      to.commit()
      rouse()
      to
    }
    
    override def reduce[A](thunks: Enumerator[() => A])(op: (A, A) => A): Relay[A] = {
      val to = new Tally.Reduce(null.asInstanceOf[A])(op)(this)
      thunks traverse new TallyAll(to, {
        val thread = Thread.currentThread
        if (thread.isInstanceOf[TrackApi]) thread.asInstanceOf[TrackApi].queue else queue
      })
      to.commit()
      rouse()
      to
    }
    
    private final class TallyAll[-A](tally: Tally[A, _], queue: AtomicQueue[() => _])
      extends AbstractFunction1[() => A, Unit] {
      override def apply(thunk: () => A) {
        tally.limit -= 1
        queue push new Tally.PutAll(thunk, tally)
      }
    }
    
    private final class TallyAny[-A](tally: Tally[A, _], queue: AtomicQueue[() => _])
      extends AbstractFunction1[() => A, Unit] {
      override def apply(thunk: () => A) {
        if (!tally.isSet) {
          tally.limit -= 1
          queue push new Tally.PutAny(thunk, tally)
        }
      }
    }
  }
}
