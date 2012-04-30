/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

final class MatrixF2x2
    [V <: Vector2 { type Vector = V; type Scalar = F },
     F <: Field { type Vector = F }] private[generic]
    (val Matrix: F2x2[V, F])
    (val _1_1: F, val _1_2: F,
     val _2_1: F, val _2_2: F)
  extends Matrix2x2Like { self =>
  
  override type Matrix = MatrixF2x2[V, F]
  override type Vec    = V
  override type Scalar = F
  
  override def Row: Vector2Space {
    type Vector = self.Vec
    type Scalar = self.Scalar
  } = Matrix.Vec
  
  override def Col: Vector2Space {
    type Vector = self.Vec
    type Scalar = self.Scalar
  } = Matrix.Vec
}
