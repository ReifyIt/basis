/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.control

import Predef.<:<

/** Standard [[Else]] operations.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Conditional
  * 
  * @groupprio  Evaluating  1
  * @groupprio  Composing   2
  * @groupprio  Recovering  3
  * @groupname  Handling    Exception Handling
  * @groupprio  Handling    4
  */
final class ElseOps[+A, +B](self: A Else B) {
  /** Returns the value of this `Bind`, or the `default` value for a `Trap`.
    * @group Evaluating */
  def getOrElse[X >: A](default: => X): X =
    macro ElseMacros.getOrElse[X]
  
  /** Returns this `Bind`, or the `other` binding for a `Trap`.
    * @group Composing */
  def orElse[X >: A, Y](other: X Else Y): X Else Y =
    macro ElseMacros.orElse[X, Y]
  
  /** Returns the value of this `Bind`, or `null` for a `Trap`.
    * @group Evaluating */
  def orNull[X >: A](implicit isNullable: Null <:< X): X =
    macro ElseMacros.orNull[X]
  
  /** Apllies a function to the value of this `Bind`, or returns the zero
    * value for a `Trap`; equivalent to `map f getOrElse z`.
    * @group Evaluating */
  def fold[X](z: X)(f: A => X): X =
    macro ElseMacros.fold[A, X]
  
  /** Returns `true` if the value of this `Bind` satisfies the predicate,
    * otherwise returns `false`.
    * @group Evaluating */
  def exists(p: A => Boolean): Boolean =
    macro ElseMacros.exists[A]
  
  /** Applies a function to the value of this `Bind`, otherwise does nothing.
    * @group Evaluating */
  def foreach[U](f: A => U): Unit =
    macro ElseMacros.foreach[A, U]
  
  /** Binds the result of a function applied to the value of this `Bind`,
    * otherwise returns this `Trap`.
    * @group Composing */
  def map[X](f: A => X): X Else B =
    macro ElseMacros.map[A, X, B]
  
  /** Returns the binding of a function applied to the value of this `Bind`,
    * otherwise returns this `Trap`.
    * @group Composing */
  def flatMap[X, Y >: B](f: A => (X Else Y)): X Else Y =
    macro ElseMacros.flatMap[A, X, Y]
  
  /** Binds the result of a function applied to the value of this `Trap`,
    * if defined, otherwise returns this.
    * @group Recovering */
  def recover[X >: A](q: PartialFunction[B, X]): X Else B =
    macro ElseMacros.recover[X, B]
  
  /** Returns the binding of a function applied to the value of this `Trap`,
    * if defined, otherwise returns this.
    * @group Recovering */
  def recoverWith[X >: A, Y >: B](q: PartialFunction[B, X Else Y]): X Else Y =
    macro ElseMacros.recoverWith[X, B, Y]
  
  /** Returns this `Bind` if its value satisfies the predicate,
    * returns the unit `Trap` if its value does not satisfy the predicate,
    * otherwise returns this `Trap`.
    * @group Composing */
  def filter(p: A => Boolean): A Else B =
    macro ElseMacros.filter[A, B]
  
  /** Returns this `Bind` if its value satisfies the predicate,
    * returns the unit `Trap` if its value does not satisfy the predicate,
    * otherwise returns this `Trap`; equivalent to `filter`.
    * @group Composing */
  def withFilter(p: A => Boolean): A Else B =
    macro ElseMacros.filter[A, B]
  
  /** Selects [[FuseOps fused]] combinators that trap exceptions with the given handler.
    * @group Handling */
  def fuse[Y >: B](trip: Throwable => Trap[Y]): FuseOps[A, Y] =
    macro ElseMacros.fuse[A, Y]
  
  /** Selects [[FuseOps fused]] combinators that trap non-fatal exceptions.
    * @group Handling */
  def fuse(implicit isTry: B <:< Throwable): FuseOps[A, Throwable] =
    macro ElseMacros.fuseTry[A]
}
