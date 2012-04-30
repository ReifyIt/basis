/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait SquareMatrixSpace extends MatrixSpace { self =>
  override type Matrix <: SquareMatrix {
    type Matrix = self.Matrix
    type Vec    = self.Vec
    type Scalar = self.Scalar
  }
  
  override type T = Matrix
  
  type Vec <: basis.algebra.Vector {
    type Vector = self.Vec
    type Scalar = self.Scalar
  }
  
  override type Row = Vec
  
  override type Col = Vec
  
  override type Scalar
  
  override def T: this.type = this
  
  override def M: Int
  
  override def N: Int
  
  override def apply(entries: TraversableOnce[Scalar]): Matrix
}
