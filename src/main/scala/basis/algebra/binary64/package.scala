/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

package object binary64 {
  type SomeRealVectorSpace = RealVectorSpace with Singleton
  
  type SomeRealMatrixSpace = RealMatrixSpace with Singleton
  
  lazy val R2 = new R2
  
  lazy val R2x2 = new R2x2
  
  lazy val R3 = new R3
  
  lazy val R3x3 = new R3x3
  
  lazy val R4 = new R4
  
  lazy val R4x4 = new R4x4
}
