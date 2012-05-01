/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

package object generic {
  type F2[F <: Ring { type Vector = F }] = VectorF2.Space[F] with Singleton
  
  type F2x2[V <: Vector2 { type Vector = V; type Scalar = F },
            F <: Field { type Vector = F }] =
    MatrixF2x2.Space[V, F] with Singleton
  
  type F3[F <: Ring { type Vector = F }] = VectorF3.Space[F] with Singleton
  
  type F3x3[V <: Vector3 { type Vector = V; type Scalar = F },
            F <: Field { type Vector = F }] =
    MatrixF3x3.Space[V, F] with Singleton
  
  type F4[F <: Ring { type Vector = F }] = VectorF4.Space[F] with Singleton
  
  type F4x4[V <: Vector4 { type Vector = V; type Scalar = F },
            F <: Field { type Vector = F }] =
    MatrixF4x4.Space[V, F] with Singleton
  
  type FN[F <: Ring { type Vector = F }] = VectorFN.Space[F] with Singleton
  
  type FMxN[V <: Vector { type Vector = V; type Scalar = F },
            W <: Vector { type Vector = W; type Scalar = F },
            F <: Ring { type Vector = F }] =
    MatrixFMxN.Space[V, W, F] with Singleton
}
