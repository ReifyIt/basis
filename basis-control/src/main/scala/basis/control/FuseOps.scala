/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.control

/** Exception-handling [[Else]] combinators.
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
  /** Binds the result of a function applied to the value of this `Bind`,
    * otherwise returns this `Trap` or a tripped exception.
    * @group Composing */
  def map[X](f: A => X): X Else B =
    macro FuseMacros.map[A, X, B]
  
  /** Returns the binding of a function applied to the value of this `Bind`,
    * otherwise returns this `Trap` or a tripped exception.
    * @group Composing */
  def flatMap[X, Y >: B](f: A => (X Else Y)): X Else Y =
    macro FuseMacros.flatMap[A, X, Y]
  
  /** Binds the result of a function applied to the value of this `Trap`,
    * if defined, otherwise returns this or a tripped exception.
    * @group Recovering */
  def recover[X >: A](q: PartialFunction[B, X]): X Else B =
    macro FuseMacros.recover[X, B]
  
  /** Returns the binding of a function applied to the value of this `Trap`,
    * if defined, otherwise returns this or a tripped exception.
    * @group Recovering */
  def recoverWith[X >: A, Y >: B](q: PartialFunction[B, X Else Y]): X Else Y =
    macro FuseMacros.recoverWith[X, B, Y]
  
  /** Returns this `Bind` if its value satisfies a predicate,
    * returns the unit `Trap` if its value does not satisfy the predicate,
    * otherwise returns this `Trap` or a tripped exception.
    * @group Composing */
  def filter(p: A => Boolean): A Else B =
    macro FuseMacros.filter[A, B]
  
  /** Returns this `Bind` if its value satisfies the predicate,
    * returns the unit `Trap` if its value does not satisfy the predicate,
    * otherwise returns this `Trap` or a tripped exception;
    * equivalent to `filter`.
    * @group Composing */
  def withFilter(p: A => Boolean): A Else B =
    macro FuseMacros.filter[A, B]
}
