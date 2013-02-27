/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch
package process

import basis.concurrent._

trait AsyncRoute { async: Async =>
  type Route >: Null <: RouteApi
  
  trait RouteApi extends Trace { this: Route =>
    def state: RouteState
    
    def queue: AtomicQueue[() => _]
    
    def parallelism: Int
    
    def rouse(): Unit
    
    def adapt(oldState: RouteState, newState: RouteState): Boolean
    
    def uncaughtException(thread: Thread, exception: Throwable): Unit
  }
}
