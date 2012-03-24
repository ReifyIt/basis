/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

package object algebra {
  type Module = Singleton {
    type Vector <: LinearVector[Vector, Scalar]
    type Scalar <: Ring[Scalar]
  }
  
  type VectorSpace = Singleton {
    type Vector <: LinearVector[Vector, Scalar]
    type Scalar <: Field[Scalar]
  }
  
  type CompleteVectorSpace = Singleton {
    type Vector <: LinearVector[Vector, Scalar]
    type Scalar <: CompleteField[Scalar]
  }
  
  type OrderedVectorSpace = Singleton {
    type Vector <: LinearVector[Vector, Scalar]
    type Scalar <: OrderedRing[Scalar] with Field[Scalar]
  }
  
  type CompleteOrderedVectorSpace = Singleton {
    type Vector <: LinearVector[Vector, Scalar]
    type Scalar <: OrderedRing[Scalar] with CompleteField[Scalar]
  }
  
  type AffineSpace = Singleton {
    type Point  <: AffinePoint[Point, Vector, Scalar]
    type Vector <: LinearVector[Vector, Scalar]
    type Scalar <: Field[Scalar]
  }
  
  type EuclideanSpace = Singleton {
    type Point  <: AffinePoint[Point, Vector, Scalar]
    type Vector <: EuclideanVector[Vector, Scalar]
    type Scalar <: OrderedRing[Scalar] with CompleteField[Scalar]
  }
  
  type MatrixSpace = Singleton {
    type Matrix       <: basis.algebra.Matrix[Matrix, Transpose, ColumnVector, RowVector, Scalar]
    type Transpose    <: basis.algebra.Matrix[Transpose, Matrix, RowVector, ColumnVector, Scalar]
    type ColumnVector <: LinearVector[ColumnVector, Scalar]
    type RowVector    <: LinearVector[RowVector, Scalar]
    type Scalar       <: Field[Scalar]
    type Vector       >: Matrix
  }
  
  type R2 = Singleton {
    type Point  = PointR2
    type Vector = VectorR2
    type Scalar = Real
  }
  
  type R3 = Singleton {
    type Point  = PointR3
    type Vector = VectorR3
    type Scalar = Real
  }
  
  type R4 = Singleton {
    type Vector = VectorR4
    type Scalar = Real
  }
  
  type R2x2 = Singleton {
    type Matrix       = MatrixR2x2
    type Transpose    = MatrixR2x2
    type ColumnVector = VectorR2
    type RowVector    = VectorR2
    type Scalar       = Real
    type Vector       = MatrixR2x2
  }
  
  type R3x3 = Singleton {
    type Matrix       = MatrixR3x3
    type Transpose    = MatrixR3x3
    type ColumnVector = VectorR3
    type RowVector    = VectorR3
    type Scalar       = Real
    type Vector       = MatrixR3x3
  }
  
  type R4x4 = Singleton {
    type Matrix       = MatrixR4x4
    type Transpose    = MatrixR4x4
    type ColumnVector = VectorR4
    type RowVector    = VectorR4
    type Scalar       = Real
    type Vector       = MatrixR4x4
  }
  
  implicit def DoubleToReal(value: Double): Real = new Real(value)
  
  implicit def RealToDouble(real: Real): Double = real.toDouble
}
