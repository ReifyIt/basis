/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch

import basis.collections._
import basis.containers._

/** A future computation coordinator.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * 
  * @groupprio  Evaluating  1
  */
trait Trace {
  /** Returns a relay triggered with the evaluation of an asynchronous expression.
    * @group Evaluating */
  def apply[A](expr: => A): Relay[A]
  
  /** Evaluates and compensates for a potentially blocking expression.
    * @group Evaluating */
  def block[A](expr: => A): A
  
  /** Asynchronously executes a thunk.
    * @group Evaluating */
  def exec[U](thunk: () => U): Unit
  
  /** Returns a relay triggered with the result of an asynchronous computation.
    * @group Evaluating */
  def relay[A](thunk: () => A): Relay[A]
  
  /** Returns a relay triggered with the successful results of all the given
    * asynchronous computations, or with the failure of any one of the computations.
    * @group Evaluating */
  def relayAll[A](thunks: Enumerator[() => A]): Relay[Enumerator[A]]
  
  /** Returns a relay triggered with the result of the first asynchronous
    * computation to successfully complete, or a trap if none succeeds.
    * @note   may not execute all the given computations.
    * @group  Evaluating */
  def relayAny[A](thunks: Enumerator[() => A]): Relay[A]
  
  /** Returns a relay triggered with the result of the first asynchronous
    * computation to successfully complete that satisfies the predicate,
    * or a trap if none succeeds, or none satisfies the predicate.
    * @note   may not execute all the given computations.
    * @group  Evaluating */
  def relayFirst[A](thunks: Enumerator[() => A])(p: A => Boolean): Relay[A]
  
  /** Returns a relay triggered with the folded result of the given
    * asynchronous computations, or with the failure of any one computation
    * or reduce operation, or with a trap if `thunks` is empty.
    * @group Evaluating */
  def reduce[A](thunks: Enumerator[() => A])(op: (A, A) => A): Relay[A]
}
