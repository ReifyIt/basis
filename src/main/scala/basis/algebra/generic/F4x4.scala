/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

class F4x4
    [V <: Vector4 { type Vector = V; type Scalar = F },
     F <: Field { type Vector = F }]
    (val Vec: Vector4Space { type Vector = V; type Scalar = F })
  extends Matrix4x4Space {
  
  override type Matrix = MatrixF4x4[V, F]
  override type Vec    = V
  override type Scalar = F
  
  override def apply(
      _1_1: Scalar, _1_2: Scalar, _1_3: Scalar, _1_4: Scalar,
      _2_1: Scalar, _2_2: Scalar, _2_3: Scalar, _2_4: Scalar,
      _3_1: Scalar, _3_2: Scalar, _3_3: Scalar, _3_4: Scalar,
      _4_1: Scalar, _4_2: Scalar, _4_3: Scalar, _4_4: Scalar) =
    new Matrix(this)(
      _1_1, _1_2, _1_3, _1_4,
      _2_1, _2_2, _2_3, _2_4,
      _3_1, _3_2, _3_3, _3_4,
      _4_1, _4_2, _4_3, _4_4)
  
  override def toString: String = "("+ Vec +" map "+ Vec +")"
}
