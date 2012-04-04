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
  * @author Chris Sachs
  * 
  * @example {{{
  * scala> implicit val zero = new Zero(0.0)
  * zero: basis.algebra.Zero[Double] = Zero(0.0)
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
@implicitNotFound("Cannot find implicit Zero typeclass for ${T}.")
final class Zero[T](value: T) extends (Zero.type => T) {
  /** Returns the additive identity element of this typeclass. */
  def apply(id: Zero.type): T = value
  
  override def toString: String =
    "Zero"+"("+ value +")"
}

/** A polymorphic identifier for the additive identity, or ''zero'' element of a type. */
object Zero {
  /** Returns the additive identity element of a type. By specifying the type
    * of element to return, `Zero[T]` eliminates ambiguities that arise when
    * implicitly converting `Zero: T` where `T` is inferred.
    * 
    * @tparam T     the type of the identity element to return.
    * @param  zero  the typeclass containing the identity element.
    * @return the value obtained from the typeclass.
    */
  def apply[T](implicit zero: Zero[T]): T = Zero
  
  override def toString: String = "Zero"
}
