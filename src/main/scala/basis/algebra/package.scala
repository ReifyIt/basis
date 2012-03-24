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
  
  implicit def DoubleToReal(value: Double): Real = new Real(value)
  
  implicit def RealToDouble(real: Real): Double = real.toDouble
}
