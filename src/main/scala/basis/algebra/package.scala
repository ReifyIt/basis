/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

package object algebra {
  type AffineSpace = Affine.Space with Singleton
  
  type LinearSpace = Linear.Space with Singleton
  
  type VectorSpace = Vector.Space with Singleton
  
  type Vector2Space = Vector2.Space with Singleton
  
  type Vector3Space = Vector3.Space with Singleton
  
  type Vector4Space = Vector4.Space with Singleton
  
  type MatrixSpace = Matrix.Space with Singleton
  
  type SquareMatrixSpace = SquareMatrix.Space with Singleton
  
  type Matrix2x2Space = Matrix2x2.Space with Singleton
  
  type Matrix3x3Space = Matrix3x3.Space with Singleton
  
  type Matrix4x4Space = Matrix4x4.Space with Singleton
  
  type SomeRing = Singleton {
    type Scalar <: Ring { type Vector = Scalar }
  }
  
  type SomeOrderedRing = Singleton {
    type Scalar <: OrderedRing { type Vector = Scalar }
  }
  
  type SomeField = Singleton {
    type Scalar <: Field { type Vector = Scalar }
  }
  
  type SomeOrderedField = Singleton {
    type Scalar <: OrderedRing with Field { type Vector = Scalar }
  }
  
  type SomeCompleteField = Singleton {
    type Scalar <: CompleteField { type Vector = Scalar }
  }
  
  type SomeCompleteOrderedField = Singleton {
    type Scalar <: OrderedRing with CompleteField { type Vector = Scalar }
  }
  
  type SomeRealField = Singleton {
    type Scalar <: RealField { type Vector = Scalar }
  }
}
