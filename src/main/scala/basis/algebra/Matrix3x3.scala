/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Matrix3x3 extends Any with SquareMatrix { self =>
  override type Matrix
  
  override type Vec
  
  override type Scalar
  
  override def Row: Vector3Space {
    type Vector = self.Vec
    type Scalar = self.Scalar
  }
  
  override def Col: Vector3Space {
    type Vector = self.Vec
    type Scalar = self.Scalar
  }
  
  def _1_1: Scalar
  def _1_2: Scalar
  def _1_3: Scalar
  def _2_1: Scalar
  def _2_2: Scalar
  def _2_3: Scalar
  def _3_1: Scalar
  def _3_2: Scalar
  def _3_3: Scalar
  
  override def M: Int
  
  override def N: Int
  
  override def apply(k: Int): Scalar
  
  override def apply(i: Int, j: Int): Scalar
  
  override def row(i: Int): Row
  
  def row1: Row
  
  def row2: Row
  
  def row3: Row
  
  override def col(j: Int): Col
  
  def col1: Col
  
  def col2: Col
  
  def col3: Col
  
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
  
  override def inverse: Option[Matrix]
  
  override def det: Scalar
  
  override def trace: Scalar
}
