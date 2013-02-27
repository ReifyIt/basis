/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch
package process

import basis.concurrent._
import basis.containers._

import java.util.concurrent.locks.LockSupport

import scala.annotation.tailrec

trait PrimeTrack extends AsyncTrack { async: Async =>
  class Track(final override val route: Route) extends Thread with TrackApi {
    final override val queue: AtomicQueue[() => _] = new AtomicQueue[() => _]
    
    private[process] final var seed: Int = System.currentTimeMillis.toInt
    
    private[process] final var running: Boolean = false
    
    private[process] final var working: Boolean = false
    
    @volatile private[process] final var waiting: Boolean = false
    
    private[process] final var blocked: Boolean = false
    
    @volatile private[process] final var ceasing: Boolean = false
    
    @volatile private[process] final var parking: Boolean = false
    
    setDaemon(true)
    
    final override def isRunning: Boolean = running
    
    final override def isWorking: Boolean = working
    
    final override def isWaiting: Boolean = waiting
    
    final override def isBlocked: Boolean = blocked
    
    final override def isCeasing: Boolean = ceasing
    
    final override def rouse() {
      waiting = false
      if (parking) LockSupport.unpark(this)
    }
    
    final override def cease() {
      ceasing = true
      waiting = false
      if (parking) LockSupport.unpark(this)
    }
    
    final override def run() {
      running = true
      working = true
      try work()
      finally {
        var state = null: RouteState
        do state = route.state
        while (!route.adapt(state, state.cease(this)))
        working = false
        running = false
      }
    }
    
    @tailrec private[process] final def work() {
      val thunk = steal()
      if (thunk != null) exec(thunk) else slow()
      if (!ceasing) work()
    }
    
    private[process] final def exec(thunk: () => _) {
      try thunk()
      catch { case e: Exception => route.uncaughtException(this, e) }
    }
    
    private[process] final def slow() {
      working = false
      waiting = true
      var state = null: RouteState
      do state = route.state
      while (!route.adapt(state, state.setWaiting(this)))
      if (waiting) idle()
      waiting = false
      working = true
    }
    
    private[process] final def idle() {
      scan()
      if (waiting) {
        val startTime = System.currentTimeMillis
        parking = true
        if (waiting) {
          Thread.interrupted()
          LockSupport.parkNanos(route, 10000000000L)
        }
        parking = false
        if (System.currentTimeMillis - startTime >= 8000) {
          val state = route.state
          route.adapt(state, state.decelerate)
        }
      }
      if (waiting) idle()
    }
    
    @tailrec private[process] final def scan() {
      if (!queue.isEmpty) route.rouse()
      else {
        val state = route.state
        val n = state.runningCount
        var i = 0
        while (i < n && state(i).queue.isEmpty) i += 1
        if (i < n) route.rouse()
        else {
          if (!route.queue.isEmpty) route.rouse()
          else if (waiting && (state ne route.state)) scan()
        }
      }
    }
    
    @tailrec private[process] final def steal(): () => _ = {
      var thunk = queue.pollOrNull()
      if (thunk == null) {
        val state = route.state
        val n = state.runningCount
        var i = -n
        while (thunk == null && i < n) {
          var r = seed
          val q = state(java.lang.Math.abs(r) % n).queue
          thunk = q.pollOrNull()
          if (!q.isEmpty) route.rouse()
          r = r ^ r << 13; r = r ^ r >>> 17; seed = r ^ r << 5
          i += 1
        }
        i = 0
        while (thunk == null && i < n) {
          val q = state(i).queue
          thunk = q.pollOrNull()
          if (!q.isEmpty) route.rouse()
          i += 1
        }
        if (thunk == null) {
          thunk = route.queue.pollOrNull()
          if (thunk != null || (state eq route.state)) thunk
          else steal()
        }
        else thunk
      }
      else thunk
    }
    
    final override def block[A](expr: => A): A = {
      var state = null: RouteState
      do state = route.state
      while (!route.adapt(state, state.setBlocked(this)))
      val result = expr
      do state = route.state
      while (!route.adapt(state, state.setWorking(this)))
      result
    }
  }
  
  override def Track(route: Route): Track = new Track(route)
}
