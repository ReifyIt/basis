/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

class F2x2[V <: Vector2Space[S] with Singleton, W <: Vector2Space[S] with Singleton, S <: Field with Singleton]
    (val Scalar: S)(val Row: V, val Col: W)
  extends Matrix2x2Space[V, W, S] {
  
  final class Element(
      val _1_1: Scalar, val _1_2: Scalar,
      val _2_1: Scalar, val _2_2: Scalar)
    extends super.Element
  
  override type Matrix = Element
  
  override lazy val Transpose = new F2x2[W, V, S](Scalar)(Col, Row)
  
  override lazy val zero: Matrix = super.zero
  
  override lazy val unit: Matrix = super.unit
  
  override def apply(
      _1_1: Scalar, _1_2: Scalar,
      _2_1: Scalar, _2_2: Scalar): Matrix =
    new Matrix(
      _1_1, _1_2,
      _2_1, _2_2)
  
  override def toString: String =
    "F2x2"+"("+ Scalar +")"+"("+ Row +", "+ Col +")"
}
