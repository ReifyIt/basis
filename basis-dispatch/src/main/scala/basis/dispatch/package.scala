/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis

import basis.control._

/** Concurrent execution. */
package object dispatch {
  /** The default async implementation. */
  val async: Async = Async.Main
  
  /** Returns a new latch, and the relay it triggers. */
  def defer[A]: (Latch[A], Relay[A]) = {
    val thunk = new Thunk[A]
    (thunk, thunk)
  }
}
