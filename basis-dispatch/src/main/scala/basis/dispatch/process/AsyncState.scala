/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch
package process

trait AsyncState { async: Async =>
  type RouteState >: Null <: RouteStateApi
  
  trait RouteStateApi { this: RouteState =>
    def runningCount: Int
    
    def workingCount: Int
    
    def waitingCount: Int
    
    def blockedCount: Int
    
    def ceasingCount: Int
    
    def apply(index: Int): Track
    
    def apply(track: Track): TrackState
    
    def rouse(track: Track): RouteState
    
    def rouse: RouteState
    
    def accelerate(parallelism: Int): RouteState
    
    def decelerate: RouteState
    
    def setWorking(track: Track): RouteState
    
    def setWaiting(track: Track): RouteState
    
    def setBlocked(track: Track): RouteState
    
    def setCeasing(track: Track): RouteState
    
    def cease(track: Track): RouteState
    
    def signal(): Unit
  }
  
  def RouteState(route: Route): RouteState
  
  type TrackState >: Null <: TrackStateApi
  
  trait TrackStateApi { this: TrackState =>
    def isWorking: Boolean
    
    def isWaiting: Boolean
    
    def isBlocked: Boolean
    
    def isCeasing: Boolean
    
    def working: TrackState
    
    def waiting: TrackState
    
    def blocked: TrackState
    
    def ceasing: TrackState
  }
}
