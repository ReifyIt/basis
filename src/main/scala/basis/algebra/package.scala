/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** Contains algebraic structures.
  * 
  * ==Mathematical spaces==
  * 
  * Mathematical spaces are modelled by singleton types with type members
  * defined by type refinements. For example, here's the definition of an
  * abstract vector space:
  * 
  * {{{
  * type VectorSpace = Singleton {
  *   type Vector <: basis.algebra.Vector[Vector, Scalar]
  *   type Scalar <: basis.algebra.Field[Scalar]
  * }
  * }}}
  * 
  * This definition "bundles up" multiple bounded types into a single new type.
  * Follow the same pattern using type equality instead of type bounds to define
  * a concrete space. Here's a definition of a concrete vector space:
  * 
  * {{{
  * type R2 = Singleton {
  *   type Vector = VectorR2
  *   type Scalar = Real
  * }
  * }}}
  * 
  * To use a space, parameterize a class or function with a type variable
  * bounded by the abstract space. Then access the type members of the space
  * with type projections. Here's a generic linear combination function:
  * 
  * {{{
  * scala> def combine[V <: VectorSpace](
  *            a: V#Scalar, x: V#Vector,
  *            b: V#Scalar, y: V#Vector): V#Vector =
  *      |   a *: x + b *: y
  * combine: [V <: basis.algebra.package.VectorSpace](a: V#Scalar, x: V#Vector, b: V#Scalar, y: V#Vector)V#Vector
  * 
  * scala> combine[R1](2, 3, 4, 5)
  * res0: basis.algebra.package.R1#Vector = 26.0
  * 
  * scala> combine[R2](1, VectorR2(3, 4), 2, VectorR2(5, 6))
  * res1: basis.algebra.package.R2#Vector = VectorR2(13.0, 16.0)
  * }}}
  */
package object algebra {
  /** An abstract linear space over a ring. */
  type Module = Singleton {
    type Vector <: algebra.Vector[Vector, Scalar]
    type Scalar <: algebra.Ring[Scalar]
  }
  
  /** An abstract linear space over a field. */
  type VectorSpace = Singleton {
    type Vector <: algebra.Vector[Vector, Scalar]
    type Scalar <: algebra.Field[Scalar]
  }
  
  /** An abstract linear space over a complete field. */
  type CompleteVectorSpace = Singleton {
    type Vector <: algebra.Vector[Vector, Scalar]
    type Scalar <: algebra.CompleteField[Scalar]
  }
  
  /** An abstract linear space over an ordered field. */
  type OrderedVectorSpace = Singleton {
    type Vector <: algebra.Vector[Vector, Scalar]
    type Scalar <: algebra.OrderedRing[Scalar] with algebra.Field[Scalar]
  }
  
  /** An abstract linear space over a complete ordered field. */
  type CompleteOrderedVectorSpace = Singleton {
    type Vector <: algebra.Vector[Vector, Scalar]
    type Scalar <: algebra.OrderedRing[Scalar] with algebra.CompleteField[Scalar]
  }
  
  /** An abstract linear space over the `Real` field. */
  type RealVectorSpace = Singleton {
    type Vector <: algebra.RealVector[Vector]
    type Scalar  = algebra.Real
  }
  
  /** An abstract affine space over a field. */
  type AffineSpace = Singleton {
    type Point  <: algebra.AffinePoint[Point, Vector, Scalar]
    type Vector <: algebra.Vector[Vector, Scalar]
    type Scalar <: algebra.Field[Scalar]
  }
  
  /** An abstract affine space over a complete field. */
  type CompleteAffineSpace = Singleton {
    type Point  <: algebra.AffinePoint[Point, Vector, Scalar]
    type Vector <: algebra.Vector[Vector, Scalar]
    type Scalar <: algebra.CompleteField[Scalar]
  }
  
  /** An abstract affine space over an ordered field. */
  type OrderedAffineSpace = Singleton {
    type Point  <: algebra.AffinePoint[Point, Vector, Scalar]
    type Vector <: algebra.Vector[Vector, Scalar]
    type Scalar <: algebra.OrderedRing[Scalar] with algebra.Field[Scalar]
  }
  
  /** An abstract affine space over a complete ordered field. */
  type CompleteOrderedAffineSpace = Singleton {
    type Point  <: algebra.AffinePoint[Point, Vector, Scalar]
    type Vector <: algebra.Vector[Vector, Scalar]
    type Scalar <: algebra.OrderedRing[Scalar] with algebra.CompleteField[Scalar]
  }
  
  /** An abstract affine space over the `Real` field. */
  type RealAffineSpace = Singleton {
    type Point  <: algebra.AffinePoint[Point, Vector, Scalar]
    type Vector <: algebra.RealVector[Vector]
    type Scalar  = algebra.Real
  }
  
  /** An abstract euclidean space. */
  type EuclideanSpace = Singleton {
    type Point  <: algebra.AffinePoint[Point, Vector, Scalar]
    type Vector <: algebra.EuclideanVector[Vector, Scalar]
    type Scalar <: algebra.OrderedRing[Scalar] with algebra.CompleteField[Scalar]
  }
  
  /** An abstract euclidean space over the `Real` field. */
  type RealEuclideanSpace = Singleton {
    type Point  <: algebra.AffinePoint[Point, Vector, Scalar]
    type Vector <: algebra.EuclideanVector[Vector, Scalar] with algebra.RealVector[Vector]
    type Scalar  = algebra.Real
  }
  
  /** An abstract linear space of matrices over a field. */
  type MatrixSpace = Singleton {
    type Matrix       <: algebra.Matrix[Matrix, Transpose, ColumnVector, RowVector, Scalar]
    type Transpose    <: algebra.Matrix[Transpose, Matrix, RowVector, ColumnVector, Scalar]
    type ColumnVector <: algebra.Vector[ColumnVector, Scalar]
    type RowVector    <: algebra.Vector[RowVector, Scalar]
    type Scalar       <: algebra.Field[Scalar]
    type Vector       >: Matrix <: Matrix
  }
  
  /** An abstract linear space of matrices over the `Real` field. */
  type RealMatrixSpace = Singleton {
    type Matrix       <: algebra.Matrix[Matrix, Transpose, ColumnVector, RowVector, Scalar] with algebra.RealVector[Matrix]
    type Transpose    <: algebra.Matrix[Transpose, Matrix, RowVector, ColumnVector, Scalar] with algebra.RealVector[Transpose]
    type ColumnVector <: algebra.Vector[ColumnVector, Scalar] with algebra.RealVector[ColumnVector]
    type RowVector    <: algebra.Vector[RowVector, Scalar] with algebra.RealVector[RowVector]
    type Scalar        = algebra.Real
    type Vector       >: Matrix <: Matrix
  }
  
  /** An abstract linear space of square matrices over a field. */
  type SquareMatrixSpace = Singleton {
    type Matrix       <: algebra.SquareMatrix[Matrix, ColumnVector, Scalar]
    type Transpose    >: Matrix <: Matrix
    type ColumnVector <: algebra.Vector[ColumnVector, Scalar]
    type RowVector    >: ColumnVector <: ColumnVector
    type Scalar        = algebra.Real
    type Vector       >: Matrix <: Matrix
  }
  
  /** An abstract linear space of square matrices over the `Real` field. */
  type RealSquareMatrixSpace = Singleton {
    type Matrix       <: algebra.SquareMatrix[Matrix, ColumnVector, Scalar] with algebra.RealVector[Matrix]
    type Transpose    >: Matrix <: Matrix
    type ColumnVector <: algebra.Vector[ColumnVector, Scalar] with algebra.RealVector[ColumnVector]
    type RowVector    >: ColumnVector <: ColumnVector
    type Scalar        = algebra.Real
    type Vector       >: Matrix <: Matrix
  }
  
  /** A concrete 1-dimensional `Integer` module. */
  type Z1 = Singleton {
    type Point  = Integer
    type Vector = Integer
    type Scalar = Integer
  }
  
  /** A concrete 2-dimensional `Integer` module. */
  type Z2 = Singleton {
    type Point  = VectorZ2
    type Vector = VectorZ2
    type Scalar = Integer
  }
  
  /** A concrete 3-dimensional `Integer` module. */
  type Z3 = Singleton {
    type Point  = VectorZ3
    type Vector = VectorZ3
    type Scalar = Integer
  }
  
  /** A concrete ''N''-dimensional `Integer` module. */
  type ZN = Singleton {
    type Point  = VectorZN
    type Vector = VectorZN
    type Scalar = Integer
  }
  
  /** A concrete 1-dimensional `Real` euclidean space. */
  type R1 = Singleton {
    type Point  = Real
    type Vector = Real
    type Scalar = Real
  }
  
  /** A concrete 2-dimensional `Real` euclidean space. */
  type R2 = Singleton {
    type Point  = PointR2
    type Vector = VectorR2
    type Scalar = Real
  }
  
  /** A concrete 3-dimensional `Real` euclidean space. */
  type R3 = Singleton {
    type Point  = PointR3
    type Vector = VectorR3
    type Scalar = Real
  }
  
  /** A concrete 4-dimensional `Real` euclidean space. */
  type R4 = Singleton {
    type Point  = VectorR4
    type Vector = VectorR4
    type Scalar = Real
  }
  
  /** A concrete ''N''-dimensional `Real` euclidean space. */
  type RN = Singleton {
    type Point  = VectorRN
    type Vector = VectorRN
    type Scalar = Real
  }
  
  /** A concrete 2x2 `Real` matrix space. */
  type R2x2 = Singleton {
    type Matrix       = MatrixR2x2
    type Transpose    = MatrixR2x2
    type ColumnVector = VectorR2
    type RowVector    = VectorR2
    type Point        = MatrixR2x2
    type Vector       = MatrixR2x2
    type Scalar       = Real
  }
  
  /** A concrete 3x3 `Real` matrix space. */
  type R3x3 = Singleton {
    type Matrix       = MatrixR3x3
    type Transpose    = MatrixR3x3
    type ColumnVector = VectorR3
    type RowVector    = VectorR3
    type Point        = MatrixR3x3
    type Vector       = MatrixR3x3
    type Scalar       = Real
  }
  
  /** A concrete 4x4 `Real` matrix space. */
  type R4x4 = Singleton {
    type Matrix       = MatrixR4x4
    type Transpose    = MatrixR4x4
    type ColumnVector = VectorR4
    type RowVector    = VectorR4
    type Point        = MatrixR4x4
    type Vector       = MatrixR4x4
    type Scalar       = Real
  }
  
  /** A concrete ''M''x''N'' `Real` matrix space. */
  type RMxN = Singleton {
    type Matrix       = MatrixRMxN
    type Transpose    = MatrixRMxN
    type ColumnVector = VectorRN
    type RowVector    = VectorRN
    type Point        = MatrixRMxN
    type Vector       = MatrixRMxN
    type Scalar       = Real
  }
}
