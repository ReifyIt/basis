/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch
package process

import basis.containers._

import scala.runtime.AbstractFunction0
import scala.runtime.AbstractFunction2

trait PrimeState extends AsyncState { async: Async =>
  final class RouteState private[process] (
      val route: Route,
      private[process] val lanes: Array[TrackApi],
      private[process] val traffic: HashMap[Track, TrackState],
      override val runningCount: Int,
      override val workingCount: Int,
      override val waitingCount: Int,
      override val blockedCount: Int,
      override val ceasingCount: Int,
      private[process] val signals: List[() => Unit])
    extends RouteStateApi {
    
    override def apply(index: Int): Track = lanes(index).asInstanceOf[Track]
    
    override def apply(track: Track): TrackState = traffic(track)
    
    override def rouse: RouteState = {
      if (waitingCount > 0) {
        val f = new Accelerate
        traffic traverse f
        f.commit
      }
      else this
    }
    
    override def rouse(track: Track): RouteState = {
      val trackState = traffic(track)
      if (trackState.isWaiting)
        new RouteState(
          route,
          lanes,
          traffic + (track, trackState.working),
          runningCount,
          workingCount + 1,
          waitingCount - 1,
          blockedCount,
          ceasingCount,
          new RouseTrackSignal(track) :: Nil)
      else this
    }
    
    override def accelerate(parallelism: Int): RouteState = {
      if (waitingCount > 0 || workingCount < parallelism) {
        val f = new Accelerate
        traffic traverse f
        f.commit(parallelism)
      }
      else this
    }
    
    override def decelerate: RouteState = {
      if (waitingCount > 0) {
        val f = new Decelerate
        traffic traverse f
        f.commit
      }
      else this
    }
    
    override def setWorking(track: Track): RouteState = {
      val trackState = traffic(track)
      if (trackState.isWaiting)
        new RouteState(
          route,
          lanes,
          traffic + (track, trackState.working),
          runningCount,
          workingCount + 1,
          waitingCount - 1,
          blockedCount,
          ceasingCount,
          new RouseTrackSignal(track) :: Nil)
      else if (trackState.isBlocked)
        new RouteState(
          route,
          lanes,
          traffic + (track, trackState.working),
          runningCount,
          workingCount + 1,
          waitingCount,
          blockedCount - 1,
          ceasingCount,
          Nil)
      else this
    }
    
    override def setWaiting(track: Track): RouteState = {
      val trackState = traffic(track)
      if (trackState.isWorking)
        new RouteState(
          route,
          lanes,
          traffic + (track, trackState.waiting),
          runningCount,
          workingCount - 1,
          waitingCount + 1,
          blockedCount,
          ceasingCount,
          Nil)
      else this
    }
    
    override def setBlocked(track: Track): RouteState = {
      val trackState = traffic(track)
      if (trackState.isWorking)
        new RouteState(
          route,
          lanes,
          traffic + (track, trackState.blocked),
          runningCount,
          workingCount - 1,
          waitingCount,
          blockedCount + 1,
          ceasingCount,
          Nil)
      else this
    }
    
    override def setCeasing(track: Track): RouteState = {
      val trackState = traffic(track)
      if (trackState.isWaiting)
        new RouteState(
          route,
          lanes,
          traffic + (track, trackState.ceasing),
          runningCount,
          workingCount,
          waitingCount - 1,
          blockedCount,
          ceasingCount + 1,
          new CeaseTrackSignal(track) :: Nil)
      else this
    }
    
    override def cease(track: Track): RouteState = {
      val trackState = traffic(track)
      if (trackState.isCeasing) {
        val traffic = this.traffic - track
        new RouteState(
          route,
          Array.coerce(traffic.keys),
          traffic,
          runningCount - 1,
          workingCount,
          waitingCount,
          blockedCount,
          ceasingCount - 1,
          Nil)
      }
      else this
    }
    
    override def signal() {
      var signals = this.signals
      while (!signals.isEmpty) {
        signals.head()
        signals = signals.tail
      }
    }
    
    private[process] final class Accelerate extends AbstractFunction2[Track, TrackState, Unit] {
      private[process] var lanes: Array[TrackApi] = RouteState.this.lanes
      private[process] var traffic: HashMap[Track, TrackState] = RouteState.this.traffic
      private[process] var runningCount: Int = RouteState.this.runningCount
      private[process] var workingCount: Int = RouteState.this.workingCount
      private[process] var waitingCount: Int = RouteState.this.waitingCount
      private[process] var signals: List[() => Unit] = Nil
      private[process] var changed: Boolean = false
      
      override def apply(track: Track, trackState: TrackState) {
        if (trackState.isWaiting) {
          traffic += (track, trackState.working)
          workingCount += 1
          waitingCount -= 1
          signals = new RouseTrackSignal(track) :: signals
          changed = true
        }
      }
      
      def commit(parallelism: Int): RouteState = {
        if (workingCount < parallelism) {
          while (workingCount < parallelism) {
            val track = Track(route)
            traffic += (track, WorkingTrackState)
            runningCount += 1
            workingCount += 1
            signals = new StartTrackSignal(track) :: signals
          }
          lanes = Array.coerce(traffic.keys)
          changed = true
        }
        commit
      }
      
      def commit: RouteState = {
        if (changed)
          new RouteState(
            route,
            lanes,
            traffic,
            runningCount,
            workingCount,
            waitingCount,
            blockedCount,
            ceasingCount,
            signals)
        else RouteState.this
      }
    }
    
    private[process] final class Decelerate extends AbstractFunction2[Track, TrackState, Unit] {
      private[process] var traffic: HashMap[Track, TrackState] = RouteState.this.traffic
      private[process] var waitingCount: Int = RouteState.this.waitingCount
      private[process] var ceasingCount: Int = RouteState.this.ceasingCount
      private[process] var signals: List[() => Unit] = Nil
      private[process] var changed: Boolean = false
      
      override def apply(track: Track, trackState: TrackState) {
        if (trackState.isWaiting) {
          traffic += (track, trackState.ceasing)
          waitingCount -= 1
          ceasingCount += 1
          signals = new CeaseTrackSignal(track) :: signals
          changed = true
        }
      }
      
      def commit: RouteState = {
        if (changed)
          new RouteState(
            route,
            lanes,
            traffic,
            runningCount,
            workingCount,
            waitingCount,
            blockedCount,
            ceasingCount,
            signals)
        else RouteState.this
      }
    }
    
    override def toString: String = {
      val s = new java.lang.StringBuilder("RouteState")
      s.append('(')
      s.append("runningCount").append(" = ").append(runningCount)
      s.append(", ").append("workingCount").append(" = ").append(workingCount)
      s.append(", ").append("waitingCount").append(" = ").append(waitingCount)
      s.append(", ").append("blockedCount").append(" = ").append(blockedCount)
      if (ceasingCount > 0) s.append(", ").append("ceasingCount").append(" = ").append(ceasingCount)
      s.append(')')
      s.toString
    }
  }
  
  override def RouteState(route: Route): RouteState =
    new RouteState(route, new Array[TrackApi](0), HashMap.empty[Track, TrackState], 0, 0, 0, 0, 0, Nil)
  
  sealed abstract class TrackState extends TrackStateApi {
    override def isWorking: Boolean
    override def isWaiting: Boolean
    override def isBlocked: Boolean
    override def isCeasing: Boolean
    override def working: TrackState = WorkingTrackState
    override def waiting: TrackState = WaitingTrackState
    override def blocked: TrackState = BlockedTrackState
    override def ceasing: TrackState = CeasingTrackState
  }
  
  private[process] final class WorkingTrackState extends TrackState {
    override def isWorking: Boolean = true
    override def isWaiting: Boolean = false
    override def isBlocked: Boolean = false
    override def isCeasing: Boolean = false
    override def toString: String = "TrackState.working"
  }
  
  private[process] final class WaitingTrackState extends TrackState {
    override def isWorking: Boolean = false
    override def isWaiting: Boolean = true
    override def isBlocked: Boolean = false
    override def isCeasing: Boolean = false
    override def toString: String = "TrackState.waiting"
  }
  
  private[process] final class BlockedTrackState extends TrackState {
    override def isWorking: Boolean = false
    override def isWaiting: Boolean = false
    override def isBlocked: Boolean = true
    override def isCeasing: Boolean = false
    override def toString: String = "TrackState.blocked"
  }
  
  private[process] final class CeasingTrackState extends TrackState {
    override def isWorking: Boolean = false
    override def isWaiting: Boolean = false
    override def isBlocked: Boolean = false
    override def isCeasing: Boolean = true
    override def toString: String = "TrackState.ceasing"
  }
  
  private[process] val WorkingTrackState: TrackState = new WorkingTrackState
  
  private[process] val WaitingTrackState: TrackState = new WaitingTrackState
  
  private[process] val BlockedTrackState: TrackState = new BlockedTrackState
  
  private[process] val CeasingTrackState: TrackState = new CeasingTrackState
  
  private[process] final class StartTrackSignal(track: Track) extends AbstractFunction0[Unit] {
    override def apply(): Unit = track.start()
  }
  
  private[process] final class RouseTrackSignal(track: Track) extends AbstractFunction0[Unit] {
    override def apply(): Unit = track.rouse()
  }
  
  private[process] final class CeaseTrackSignal(track: Track) extends AbstractFunction0[Unit] {
    override def apply(): Unit = track.cease()
  }
}
