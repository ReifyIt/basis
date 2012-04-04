/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import scala.annotation.implicitNotFound

/** A typeclass representing the multiplicative identity, or ''one'' element of
  * a type. As a function, this typeclass converts the singleton `One` object to
  * a multiplicative identity element by returning the value contained by the
  * typeclass. This makes possible implicit conversions from the `One` singleton
  * to the multiplicative identity element of an expression's inferred type;
  * effectively making `One` a polymorphic identifier for the multiplicative
  * identity of a type.
  * 
  * Incorporate identity elements into broader typeclasses when possible.
  * Passing identity elements around pollutes method signatures and gets
  * annoying. The verbose name `MultiplicativeIdentity` was partly chosen to
  * discourage excessive use.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * scala> implicit val one = new MultiplicativeIdentity(1.0)
  * one: basis.algebra.MultiplicativeIdentity[Double] = MultiplicativeIdentity(0.0)
  * 
  * scala> One: Double
  * res0: Double = 1.0
  * 
  * scala> math.sqrt(One)
  * res1: Double = 1.0
  * }}}
  * 
  * @constructor Constructs a typeclass with a multiplicative identity.
  * @tparam T       the type of the identity element.
  * @param  value   the multiplicative identity element.
  */
@implicitNotFound("Cannot find implicit MultiplicativeIdentity typeclass for ${T}.")
final class MultiplicativeIdentity[T](value: T) extends (One.type => T) {
  /** Returns the multiplicative identity element of this typeclass. */
  def apply(id: One.type): T = value
  
  override def toString: String =
    "MultiplicativeIdentity"+"("+ value +")"
}

/** A polymorphic identifier for the multiplicative identity, or ''one'' element of a type. */
object One {
  /** Returns the multiplicative identity element of a type. By specifying the
    * type of element to return, `One[T]` eliminates ambiguities that arise
    * when implicitly converting `One: T` where `T` is inferred.
    * 
    * @tparam T         the type of the identity element to return.
    * @param  identity  the typeclass containing the identity element.
    * @return the value obtained from the typeclass.
    */
  def apply[T](implicit identity: MultiplicativeIdentity[T]): T = One
  
  /** Returns a new multiplicative identity typeclass for the given value. */
  def apply[T](value: T): MultiplicativeIdentity[T] =
    new MultiplicativeIdentity[T](value)
  
  override def toString: String = "One"
}
