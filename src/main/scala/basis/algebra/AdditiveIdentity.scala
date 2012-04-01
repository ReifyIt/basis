/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

import scala.annotation.implicitNotFound

/** A typeclass representing the additive identity, or ''zero'' element of a type.
  * As a function, this typeclass converts the singleton `Zero` object to an
  * additive identity element by returning the value contained by the typeclass.
  * This makes possible implicit conversions from the `Zero` singleton to the
  * additive identity element of an expression's inferred type; effectively
  * making `Zero` a polymorphic identifier for the additive identity of a type.
  * 
  * Incorporate identity elements into broader typeclasses when possible.
  * Passing identity elements around pollutes method signatures and gets
  * annoying. The verbose name `AdditiveIdentity` was partly chosen to
  * discourage excessive use.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * scala> implicit val zero = new AdditiveIdentity(0.0)
  * zero: basis.algebra.AdditiveIdentity[Double] = AdditiveIdentity(0.0)
  * 
  * scala> Zero: Double
  * res0: Double = 0.0
  * 
  * scala> math.sqrt(Zero)
  * res1: Double = 0.0
  * }}}
  * 
  * @constructor Constructs a typeclass with an additive identity.
  * @tparam T       the type of the identity element.
  * @param  value   the additive identity element.
  */
@implicitNotFound("Cannot find implicit AdditiveIdentity typeclass for ${T}.")
final class AdditiveIdentity[T](value: T) extends (Zero.type => T) {
  /** Returns the additive identity element of this typeclass. */
  def apply(id: Zero.type): T = value
  
  override def toString: String =
    "AdditiveIdentity"+"("+ value +")"
}

/** A polymorphic identifier for the additive identity, or ''zero'' element of a type. */
object Zero {
  /** Returns the additive identity element of a type. By specifying the type
    * of element to return, `Zero[T]` eliminates ambiguities that arise when
    * implicitly converting `Zero: T` where `T` is inferred.
    * 
    * @tparam T         the type of the identity element to return.
    * @param  identity  the typeclass containing the identity element.
    * @return the value obtained from the typeclass.
    */
  def apply[T](implicit identity: AdditiveIdentity[T]): T = Zero
  
  override def toString: String = "Zero"
}
