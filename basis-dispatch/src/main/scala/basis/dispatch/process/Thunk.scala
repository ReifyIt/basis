/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch
package process

import basis.containers._
import basis.control._

import scala.runtime.AbstractFunction0
import scala.runtime.AbstractFunction1

/** A latch that applies itself to the result with which it is set.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * 
  * @groupprio  Evaluating  1
  */
abstract class Thunk[-A](protected val trace: Trace) extends AbstractFunction0[Unit] with Latch[A] {
  @volatile private[this] final var state: Try[A] = _
  
  final override def isSet: Boolean = state != null
  
  final override def set(result: Try[A]): Boolean = {
    if (result == null) throw new NullPointerException
    state == null && { state = result; trace exec this; true }
  }
  
  /** Triggers the immediate evaluation of this thunk.
    * @group Evaluating */
  final override def apply() {
    val result = state
    if (result == null) throw new IllegalStateException
    apply(result)
  }
  
  /** Effects this thunk with its received result.
    * @group Evaluating */
  def apply(result: Try[A]): Unit
}

private[basis] object Thunk {
  private[basis] final class Eval[-A](expr: => A, to: Latch[A]) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      to set (try Bind(expr) catch { case e: Throwable => Trap.NonFatal(e) })
  }
  
  private[basis] final class Exec[-A](thunk: () => A, to: Latch[A]) extends AbstractFunction0[Unit] {
    override def apply(): Unit =
      to set (try Bind(thunk()) catch { case e: Throwable => Trap.NonFatal(e) })
  }
  
  private[basis] final class Run[-A](f: Try[A] => _)(trace: Trace) extends Thunk[A](trace) {
    override def apply(result: Try[A]): Unit = f(result)
  }
  
  private[basis] final class Foreach[-A](f: A => _)(trace: Trace) extends Thunk[A](trace) {
    override def apply(result: Try[A]): Unit = if (result.canBind) f(result.bind)
  }
  
  private[basis] final class AndThen[-A](q: PartialFunction[Try[A], _], to: Latch[A])(trace: Trace) extends Thunk[A](trace) {
    override def apply(result: Try[A]) {
      try if (q.isDefinedAt(result)) q(result)
      finally to set result
    }
  }
  
  private[basis] final class Choose[-A, +B](q: PartialFunction[A, B], to: Latch[B])(trace: Trace) extends Thunk[A](trace) {
    override def apply(result: Try[A]) {
      to set {
        try {
          if (result.canBind) {
            val value = result.bind
            if (q.isDefinedAt(value)) Bind(q(value)) else Trap
          }
          else result.asInstanceOf[Nothing Else Throwable]
        }
        catch { case e: Throwable => Trap.NonFatal(e) }
      }
    }
  }
  
  private[basis] final class Map[-A, +B](f: A => B, to: Latch[B])(trace: Trace) extends Thunk[A](trace) {
    override def apply(result: Try[A]) {
      to set (try if (result.canBind) Bind(f(result.bind)) else result.asInstanceOf[Nothing Else Throwable]
              catch { case e: Throwable => Trap.NonFatal(e) })
    }
  }
  
  private[basis] final class FlatMap[-A, +B](f: A => Relay[B], to: Latch[B])(trace: Trace) extends Thunk[A](trace) {
    override def apply(result: Try[A]) {
      try if (result.canBind) f(result.bind) forward to else to set result.asInstanceOf[Nothing Else Throwable]
      catch { case e: Throwable => to set Trap.NonFatal(e) }
    }
  }
  
  private[basis] final class Recover[-A](q: PartialFunction[Throwable, A], to: Latch[A])(trace: Trace) extends Thunk[A](trace) {
    override def apply(result: Try[A]) {
      to set {
        try {
          if (result.canSafelyTrap) {
            val exception = result.trap
            if (q.isDefinedAt(exception)) Bind(q(exception)) else result
          }
          else result
        }
        catch { case e: Throwable => Trap.NonFatal(e) }
      }
    }
  }
  
  private[basis] final class RecoverWith[-A](q: PartialFunction[Throwable, Relay[A]], to: Latch[A])(trace: Trace) extends Thunk[A](trace) {
    override def apply(result: Try[A]) {
      try {
        if (result.canSafelyTrap) {
          val exception = result.trap
          if (q.isDefinedAt(exception)) q(exception) forward to else to set result
        }
        else to set result
      }
      catch { case e: Throwable => to set Trap.NonFatal(e) }
    }
  }
  
  private[basis] final class Filter[-A](p: A => Boolean, to: Latch[A])(trace: Trace) extends Thunk[A](trace) {
    override def apply(result: Try[A]) {
      to set (try if (result.canTrap || p(result.bind)) result else Trap
              catch { case e: Throwable => Trap.NonFatal(e) })
    }
  }
  
  private[basis] final class Signal[+A](result: Try[A]) extends AbstractFunction1[Latch[A], Unit] {
    override def apply(latch: Latch[A]): Unit = latch set result
  }
}
