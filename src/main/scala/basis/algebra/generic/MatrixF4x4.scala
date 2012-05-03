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
     F <: Field { type Vector = F }] private
    (val Matrix: MatrixF4x4.Space[V, F])
    (val _1_1: F, val _1_2: F, val _1_3: F, val _1_4: F,
     val _2_1: F, val _2_2: F, val _2_3: F, val _2_4: F,
     val _3_1: F, val _3_2: F, val _3_3: F, val _3_4: F,
     val _4_1: F, val _4_2: F, val _4_3: F, val _4_4: F)
  extends Matrix4x4 { self =>
  
  override type Matrix = MatrixF4x4[V, F]
  override type Span   = V
  override type Scalar = F
}

object MatrixF4x4 {
  def apply[F <: Field { type Vector = F }]
      (Span: Vector4.Space { type Scalar = F },
       Scalar: Field.Space { type Vector = F }) =
    new Space[Span.type#Vector, F](Span, Scalar)
  
  class Space
      [V <: Vector4 { type Vector = V; type Scalar = F },
       F <: Field { type Vector = F }]
      (val Span: Vector4.Space { type Vector = V; type Scalar = F },
       val Scalar: Field.Space { type Vector = F })
    extends Matrix4x4.Space {
    
    override type Matrix = MatrixF4x4[V, F]
    override type Span   = V
    override type Scalar = F
    
    override def Row = Span
    override def Col = Span
    
    override lazy val zero: Matrix = super.zero
    override lazy val unit: Matrix = super.unit
    
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
    
    override def toString: String = "("+ Span +" map "+ Span +")"
  }
}
