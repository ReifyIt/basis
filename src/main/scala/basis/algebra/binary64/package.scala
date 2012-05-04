/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

package object binary64 {
  type RealVectorSpace = RealVector.Space with Singleton
  
  type RealMatrixSpace = RealMatrix.Space with Singleton
  
  type IntegerVectorSpace = IntegerVector.Space with Singleton
  
  type R1 = Real.type
  lazy val R1 = Real
  
  type R2 = R2.type
  lazy val R2 = VectorR2
  
  type R2x2 = R2x2.type
  lazy val R2x2 = MatrixR2x2
  
  type R3 = R3.type
  lazy val R3 = VectorR3
  
  type R3x3 = R3x3.type
  lazy val R3x3 = MatrixR3x3
  
  type R4 = R4.type
  lazy val R4 = VectorR4
  
  type R4x4 = R4x4.type
  lazy val R4x4 = MatrixR4x4
  
  type Z1 = Integer.type
  lazy val Z1 = Integer
  
  type Z2 = Z2.type
  lazy val Z2 = VectorZ2
  
  type Z3 = Z3.type
  lazy val Z3 = VectorZ3
}
