/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait SquareMatrix extends Any with Linear with Ring with Matrix { self =>
  override type Matrix
  
  override type T = Matrix
  
  type Vec
  
  override type Row = Vec
  
  override type Col = Vec
  
  override type Scalar
  
  override def Row: VectorSpace {
    type Vector = self.Vec
    type Scalar = self.Scalar
  }
  
  override def Col: VectorSpace {
    type Vector = self.Vec
    type Scalar = self.Scalar
  }
  
  override def M: Int
  
  override def N: Int
  
  override def apply(k: Int): Scalar
  
  override def apply(i: Int, j: Int): Scalar
  
  override def row(i: Int): Row
  
  override def col(j: Int): Col
  
  override def + (that: Matrix): Matrix
  
  override def unary_- : Matrix
  
  override def - (that: Matrix): Matrix
  
  override def :* (scalar: Scalar): Matrix
  
  override def *: (scalar: Scalar): Matrix
  
  override def :⋅ (vector: Row): Col
  
  override def ⋅: (vector: Col): Row
  
  override def ⋅ [U <: basis.algebra.Vector { type Vector = U; type Scalar = self.Scalar }]
      (that: basis.algebra.Matrix { type Row = U; type Col = self.Row; type Scalar = self.Scalar })
    : basis.algebra.Matrix { type Row = U; type Col = self.Col; type Scalar = self.Scalar }
  
  override def T: T
  
  override def * (that: Matrix): Matrix
  
  def inverse: Option[Matrix]
  
  def det: Scalar
  
  def trace: Scalar
}
