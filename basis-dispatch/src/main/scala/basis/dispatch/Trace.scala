/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch

/** A coordinator of future computations.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * 
  * @groupprio  Evaluating  1
  */
trait Trace {
  /** Returns a relay triggered by the completion of an asynchronous computation.
    * @group Evaluating */
  def apply[A](expr: => A): Relay[A]
  
  /** Evaluates and compensates for a potentially blocking expression.
    * @group Evaluating */
  def block[A](expr: => A): A
  
  /** Asynchronously executes a thunk.
    * @group Evaluating */
  def exec[U](thunk: () => U): Unit
}
