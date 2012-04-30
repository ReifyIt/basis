/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait SquareMatrixLike extends Any with Equals with Linear with Ring with MatrixLike with SquareMatrix { self =>
  override type Matrix <: SquareMatrix {
    type Matrix = self.Matrix
    type Vec    = self.Vec
    type Scalar = self.Scalar
  }
  
  override type Vec <: basis.algebra.Vector {
    type Vector = self.Vec
    type Scalar = self.Scalar
  }
  
  override type Scalar <: Ring {
    type Vector = self.Scalar
  }
  
  override def Matrix: SquareMatrixSpace {
    type Matrix = self.Matrix
    type Vec    = self.Vec
    type Scalar = self.Scalar
  }
  
  override def Row: VectorSpace {
    type Vector = self.Vec
    type Scalar = self.Scalar
  }
  
  override def Col: VectorSpace {
    type Vector = self.Vec
    type Scalar = self.Scalar
  }
}
