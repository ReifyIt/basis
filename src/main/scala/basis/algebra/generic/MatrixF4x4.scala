/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

final class MatrixF4x4
    [V <: Vector4 { type Vector = V; type Scalar = F },
     F <: Field { type Vector = F }] private[generic]
    (val Matrix: F4x4[V, F])
    (val _1_1: F, val _1_2: F, val _1_3: F, val _1_4: F,
     val _2_1: F, val _2_2: F, val _2_3: F, val _2_4: F,
     val _3_1: F, val _3_2: F, val _3_3: F, val _3_4: F,
     val _4_1: F, val _4_2: F, val _4_3: F, val _4_4: F)
  extends Matrix4x4Like { self =>
  
  override type Matrix = MatrixF4x4[V, F]
  override type Vec    = V
  override type Scalar = F
  
  override def Row: Vector4Space {
    type Vector = self.Vec
    type Scalar = self.Scalar
  } = Matrix.Vec
  
  override def Col: Vector4Space {
    type Vector = self.Vec
    type Scalar = self.Scalar
  } = Matrix.Vec
}
