/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

/** A generic space of 2x2 matrices over a ring.
  * 
  * @author Chris Sachs
  * 
  * @tparam V   The space of rows and columns.
  * @tparam S   The set of scalars.
  */
class F2x2[V <: Vector2Space[S] with Singleton, S <: Field with Singleton]
    (val Scalar: S)(Vector: V)
  extends Matrix2x2Space[V, S] {
  
  final class Element(
      val _1_1: Scalar, val _1_2: Scalar,
      val _2_1: Scalar, val _2_2: Scalar)
    extends super.Element
  
  override type Matrix = Element
  
  override def Row: V = Vector
  override def Col: V = Vector
  
  override lazy val zero: Matrix = super.zero
  override lazy val unit: Matrix = super.unit
  
  override def apply(
      _1_1: Scalar, _1_2: Scalar,
      _2_1: Scalar, _2_2: Scalar): Matrix =
    new Matrix(
      _1_1, _1_2,
      _2_1, _2_2)
  
  override def toString: String =
    "F2x2"+"("+ Scalar +")"+"("+ Vector +")"
}
