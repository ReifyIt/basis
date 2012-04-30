/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

trait RealMatrix extends Any with Matrix { self =>
  override type Matrix
  
  override type T
  
  override type Row
  
  override type Col
  
  override type Scalar = Real
  
  override def Row: RealVectorSpace {
    type Vector = self.Row
  }
  
  override def Col: RealVectorSpace {
    type Vector = self.Col
  }
  
  override def M: Int
  
  override def N: Int
  
  override def apply(k: Int): Real
  
  override def apply(i: Int, j: Int): Real
  
  override def row(i: Int): Row
  
  override def col(j: Int): Col
  
  override def + (that: Matrix): Matrix
  
  override def unary_- : Matrix
  
  override def - (that: Matrix): Matrix
  
  override def :* (scalar: Scalar): Matrix
  
  override def *: (scalar: Scalar): Matrix
  
  override def :⋅ (vector: Row): Col
  
  override def ⋅: (vector: Col): Row
  
  override def ⋅ [U <: basis.algebra.Vector { type Vector = U; type Scalar = Real }]
      (that: basis.algebra.Matrix { type Row = U; type Col = self.Row; type Scalar = Real })
    : basis.algebra.Matrix { type Row = U; type Col = self.Col; type Scalar = Real }
  
  def ⋅ [U <: RealVector { type Vector = U }]
      (that: RealMatrix { type Row = U; type Col = self.Row })
    : RealMatrix { type Row = U; type Col = self.Col }
  
  override def T: T
}
