/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

package object binary {
  type Real = Real.Value
  
  lazy val Real = new FloatingPoint(2)(512)
  
  
  lazy val R2 = F2(Real)
  
  lazy val R3 = F3(Real)
  
  lazy val R4 = F4(Real)
  
  type RN = FN[Real.type]
  
  def RN(N: Int): RN = FN(Real)(N)
  
  
  lazy val R2x2 = F2x2(Real)(R2)
  
  lazy val R3x3 = F3x3(Real)(R3)
  
  lazy val R4x4 = F4x4(Real)(R4)
  
  type RMxN[V <: FN[Real.type] with Singleton, W <: FN[Real.type] with Singleton] = FMxN[V, W, Real.type]
  
  def RMxN(V: FN[Real.type], W: FN[Real.type]): RMxN[V.type, W.type] = FMxN(Real)(V, W)
}
