/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch
package process

import basis.concurrent._

trait AsyncTrack { async: Async =>
  type Track >: Null <: TrackApi
  
  trait TrackApi { this: Track =>
    def route: Route
    
    def queue: AtomicQueue[() => _]
    
    def isRunning: Boolean
    
    def isWorking: Boolean
    
    def isWaiting: Boolean
    
    def isBlocked: Boolean
    
    def isCeasing: Boolean
    
    def start(): Unit
    
    def rouse(): Unit
    
    def cease(): Unit
    
    def block[A](expr: => A): A
  }
  
  def Track(route: Route): Track
}
