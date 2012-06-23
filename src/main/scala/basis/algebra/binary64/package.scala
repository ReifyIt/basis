/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

package object binary64 {
  type R = Real.type
  lazy val R: R = Real
  
  
  type R2 = VectorR2.type
  lazy val R2: R2 = VectorR2
  
  type R3 = VectorR3.type
  lazy val R3: R3 = VectorR3
  
  type R4 = VectorR4.type
  lazy val R4: R4 = VectorR4
  
  
  type R2x2 = MatrixR2x2.type
  lazy val R2x2: R2x2 = MatrixR2x2
  
  type R3x3 = MatrixR3x3.type
  lazy val R3x3: R3x3 = MatrixR3x3
  
  type R4x4 = MatrixR4x4.type
  lazy val R4x4: R4x4 = MatrixR4x4
  
  
  type Z = Integer.type
  lazy val Z: Z = Integer
  
  
  type Z2 = VectorZ2.type
  lazy val Z2: Z2 = VectorZ2
  
  type Z3 = VectorZ3.type
  lazy val Z3: Z3 = VectorZ3
  
  
  def R(N: Int): RealVectorSpace = N match {
    case 2 => R2
    case 3 => R3
    case 4 => R4
    case N => new RN(N)
  }
  
  def Z(N: Int): IntegerModule = N match {
    case 2 => Z2
    case 3 => Z3
    case N => new ZN(N)
  }
}
