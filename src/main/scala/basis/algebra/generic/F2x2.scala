/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

class F2x2
    [V <: Vector2 { type Vector = V; type Scalar = F },
     F <: Field { type Vector = F }]
    (val Vec: Vector2Space { type Vector = V; type Scalar = F })
  extends Matrix2x2Space {
  
  override type Matrix = MatrixF2x2[V, F]
  override type Vec    = V
  override type Scalar = F
  
  override def apply(
      _1_1: Scalar, _1_2: Scalar,
      _2_1: Scalar, _2_2: Scalar) =
    new Matrix(this)(
      _1_1, _1_2,
      _2_1, _2_2)
  
  override def toString: String = "("+ Vec +" map "+ Vec +")"
}
