/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package generic

final class MatrixF3x3
    [V <: Vector3 { type Vector = V; type Scalar = F },
     F <: Field { type Vector = F }] private
    (val Matrix: MatrixF3x3.Space[V, F])
    (val _1_1: F, val _1_2: F, val _1_3: F,
     val _2_1: F, val _2_2: F, val _2_3: F,
     val _3_1: F, val _3_2: F, val _3_3: F)
  extends Matrix3x3 { self =>
  
  override type Matrix = MatrixF3x3[V, F]
  override type Span   = V
  override type Scalar = F
}

object MatrixF3x3 {
  def apply[F <: Field { type Vector = F }]
      (Span: Vector3.Space { type Scalar = F },
       Scalar: Field.Space { type Vector = F }) =
    new Space[Span.type#Vector, F](Span, Scalar)
  
  class Space
      [V <: Vector3 { type Vector = V; type Scalar = F },
       F <: Field { type Vector = F }]
      (val Span: Vector3.Space { type Vector = V; type Scalar = F },
       val Scalar: Field.Space { type Vector = F })
    extends Matrix3x3.Space {
    
    override type Matrix = MatrixF3x3[V, F]
    override type Span   = V
    override type Scalar = F
    
    override def Row = Span
    override def Col = Span
    
    override lazy val zero: Matrix = super.zero
    override lazy val unit: Matrix = super.unit
    
    override def apply(
        _1_1: Scalar, _1_2: Scalar, _1_3: Scalar,
        _2_1: Scalar, _2_2: Scalar, _2_3: Scalar,
        _3_1: Scalar, _3_2: Scalar, _3_3: Scalar) =
      new Matrix(this)(
        _1_1, _1_2, _1_3,
        _2_1, _2_2, _2_3,
        _3_1, _3_2, _3_3)
    
    override def toString: String = "("+ Span +" map "+ Span +")"
  }
}
