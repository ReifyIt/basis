/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

class F4x4[V <: Vector4Space[S] with Singleton, W <: Vector4Space[S] with Singleton, S <: Field with Singleton]
    (val Scalar: S)(val Row: V, val Col: W)
  extends Matrix4x4Space[V, W, S] {
  
  final class Element(
      val _1_1: Scalar, val _1_2: Scalar, val _1_3: Scalar, val _1_4: Scalar,
      val _2_1: Scalar, val _2_2: Scalar, val _2_3: Scalar, val _2_4: Scalar,
      val _3_1: Scalar, val _3_2: Scalar, val _3_3: Scalar, val _3_4: Scalar,
      val _4_1: Scalar, val _4_2: Scalar, val _4_3: Scalar, val _4_4: Scalar)
    extends super.Element
  
  override type Matrix = Element
  
  override lazy val Transpose = new F4x4[W, V, S](Scalar)(Col, Row)
  
  override lazy val zero: Matrix = super.zero
  
  override def apply(
      _1_1: Scalar, _1_2: Scalar, _1_3: Scalar, _1_4: Scalar,
      _2_1: Scalar, _2_2: Scalar, _2_3: Scalar, _2_4: Scalar,
      _3_1: Scalar, _3_2: Scalar, _3_3: Scalar, _3_4: Scalar,
      _4_1: Scalar, _4_2: Scalar, _4_3: Scalar, _4_4: Scalar): Matrix =
    new Matrix(
      _1_1, _1_2, _1_3, _1_4,
      _2_1, _2_2, _2_3, _2_4,
      _3_1, _3_2, _3_3, _3_4,
      _4_1, _4_2, _4_3, _4_4)
  
  override def toString: String =
    "F4x4"+"("+ Scalar +")"+"("+ Row +", "+ Col +")"
}
