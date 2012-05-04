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
  
  type MatrixRingSpace = MatrixRing.Space with Singleton
  
  type Matrix2x2Space = Matrix2x2.Space with Singleton
  
  type Matrix3x3Space = Matrix3x3.Space with Singleton
  
  type Matrix4x4Space = Matrix4x4.Space with Singleton
  
  
  type RingSpace = Ring.Space with Singleton
  
  type RingScalar = Ring.Scalar with Singleton
  
  type FieldSpace = Field.Space with Singleton
  
  type FieldScalar = Field.Scalar with Singleton
  
  type OrderedRingSpace = OrderedRing.Space with Singleton
  
  type OrderedRingScalar = OrderedRing.Scalar with Singleton
  
  type OrderedFieldSpace = OrderedField.Space with Singleton
  
  type OrderedFieldScalar = OrderedField.Scalar with Singleton
  
  type CompleteFieldSpace = CompleteField.Space with Singleton
  
  type CompleteFieldScalar = CompleteField.Scalar with Singleton
  
  type RealFieldSpace = RealField.Space with Singleton
  
  type RealFieldScalar = RealField.Scalar with Singleton
  
  
  type IntervalRingSpace = IntervalRing.Space with Singleton
  
  type IntervalRingScalar = IntervalRing.Scalar with Singleton
  
  type IntervalFieldSpace = IntervalField.Space with Singleton
  
  type IntervalFieldScalar = IntervalField.Scalar with Singleton
}
