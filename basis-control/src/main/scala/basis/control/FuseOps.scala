/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.control

/** Exception handling [[Else]] combinators.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Conditional
  * 
  * @groupprio  Composing   1
  * @groupprio  Recovering  2
  */
final class FuseOps[+A, +B](self: A Else B, trip: Throwable => Trap[B]) {
  /** Binds the result of a function applied to this `Free` value,
    * or taps an exception, or returns this `Trap`.
    * @group Composing */
  def map[X](f: A => X): X Else B =
    macro FuseMacros.map[A, X, B]
  
  /** Returns the result of an `Else` function applied to this `Free` value,
    * or traps an exception, or returns this `Trap`.
    * @group Composing */
  def flatMap[X, Y >: B](f: A => (X Else Y)): X Else Y =
    macro FuseMacros.flatMap[A, X, Y]
  
  /** Binds the result of a function applied to this `Trap` value,
    * if defined, or traps an exception, or returns this.
    * @group Recovering */
  def recover[X >: A](q: PartialFunction[B, X]): X Else B =
    macro FuseMacros.recover[X, B]
  
  /** Returns the result of an `Else` function applied to this `Trap` value,
    * if defined, or traps an exception, or returns this.
    * @group Recovering */
  def recoverWith[X >: A, Y >: B](q: PartialFunction[B, X Else Y]): X Else Y =
    macro FuseMacros.recoverWith[X, B, Y]
  
  /** Returns this `Free` if its value satisfies a predicate,
    * or `Trap` if its value does not satisfy the predicate,
    * or traps an exception, or returns this `Trap`.
    * @group Composing */
  def filter(p: A => Boolean): A Else B =
    macro FuseMacros.filter[A, B]
  
  /** Returns this `Free` if its value satisfies the predicate,
    * or `Trap` if its value does not satisfy the predicate,
    * or traps an exception, or returns this `Trap`; equivalent to `filter`.
    * @group Composing */
  def withFilter(p: A => Boolean): A Else B =
    macro FuseMacros.filter[A, B]
}
