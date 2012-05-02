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
     F <: Field { type Vector = F }] private
    (val Matrix: MatrixF2x2.Space[V, F])
    (val _1_1: F, val _1_2: F,
     val _2_1: F, val _2_2: F)
  extends Matrix2x2.Template { self =>
  
  override type Matrix = MatrixF2x2[V, F]
  override type Span  = V
  override type Scalar = F
  
  override def Row: Vector2.Space {
    type Vector = self.Span
    type Scalar = self.Scalar
  } = Matrix.Span
  
  override def Col: Vector2.Space {
    type Vector = self.Span
    type Scalar = self.Scalar
  } = Matrix.Span
}

object MatrixF2x2 {
  def apply[F <: Field { type Vector = F }]
      (Span: Vector2.Space { type Scalar = F },
       Scalar: Field.Space { type Vector = F }) =
    new Space[Span.type#Vector, F](Span, Scalar)
  
  class Space
      [V <: Vector2 { type Vector = V; type Scalar = F },
       F <: Field { type Vector = F }]
      (val Span: Vector2.Space { type Vector = V; type Scalar = F },
       val Scalar: Field.Space { type Vector = F })
    extends Field.Scalar with Matrix2x2.Space {
    
    override type Matrix = MatrixF2x2[V, F]
    override type Span   = V
    override type Scalar = F
    
    lazy val zero: Matrix = {
      val z = Scalar.zero
      apply(z, z,  z, z)
    }
    
    lazy val unit: Matrix = {
      val z = Scalar.zero
      val u = Scalar.unit
      apply(u, z,  z, u)
    }
    
    override def apply(
        _1_1: Scalar, _1_2: Scalar,
        _2_1: Scalar, _2_2: Scalar) =
      new Matrix(this)(
        _1_1, _1_2,
        _2_1, _2_2)
    
    override def toString: String = "("+ Span +" map "+ Span +")"
  }
}
