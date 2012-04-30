/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

package object algebra {
  type SomeAffineSpace = AffineSpace with Singleton
  
  type SomeLinearSpace = LinearSpace with Singleton
  
  type SomeVectorSpace = VectorSpace with Singleton
  
  type SomeVector2Space = Vector2Space with Singleton
  
  type SomeVector3Space = Vector3Space with Singleton
  
  type SomeVector4Space = Vector4Space with Singleton
  
  type SomeMatrixSpace = MatrixSpace with Singleton
  
  type SomeSquareMatrixSpace = SquareMatrixSpace with Singleton
  
  type SomeMatrix2x2Space = Matrix2x2Space with Singleton
  
  type SomeMatrix3x3Space = Matrix3x3Space with Singleton
  
  type SomeMatrix4x4Space = Matrix4x4Space with Singleton
  
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
