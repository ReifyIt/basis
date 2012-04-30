/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Matrix extends Any with Linear { self =>
  override type Point = Matrix
  
  override type Vector = Matrix
  
  type Matrix
  
  type T
  
  type Row
  
  type Col
  
  override type Scalar
  
  def Row: VectorSpace {
    type Vector = self.Row
    type Scalar = self.Scalar
  }
  
  def Col: VectorSpace {
    type Vector = self.Col
    type Scalar = self.Scalar
  }
  
  def M: Int
  
  def N: Int
  
  def apply(k: Int): Scalar
  
  def apply(i: Int, j: Int): Scalar
  
  def row(i: Int): Row
  
  def col(j: Int): Col
  
  override def + (that: Matrix): Matrix
  
  override def unary_- : Matrix
  
  override def - (that: Matrix): Matrix
  
  override def :* (scalar: Scalar): Matrix
  
  override def *: (scalar: Scalar): Matrix
  
  def :⋅ (vector: Row): Col
  
  def ⋅: (vector: Col): Row
  
  def ⋅ [U <: basis.algebra.Vector { type Vector = U; type Scalar = self.Scalar }]
      (that: basis.algebra.Matrix { type Row = U; type Col = self.Row; type Scalar = self.Scalar })
    : basis.algebra.Matrix { type Row = U; type Col = self.Col; type Scalar = self.Scalar }
  
  def T: T
}
