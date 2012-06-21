/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

package object decimal {
  type Integer = binary.Integer
  
  val Integer: binary.Integer.type = binary.Integer
  
  type Real = Real.Vector
  
  val Real = new binary.FloatingPoint(10)(128)
  
  type RN = generic.FN[Real.type]
  
  def RN(N: Int): RN = new generic.FN[Real.type](Real)(N)
  
  lazy val R2 = new generic.F2[Real.type](Real)
  
  lazy val R3 = new generic.F3[Real.type](Real)
  
  lazy val R4 = new generic.F4[Real.type](Real)
  
  type RMxN[V <: VectorSpace[Real.type] with Singleton, W <: VectorSpace[Real.type] with Singleton] =
    generic.FMxN[V, W, Real.type]
  
  def RMxN(V: VectorSpace[Real.type], W: VectorSpace[Real.type]): RMxN[V.type, W.type] =
    new generic.FMxN[V.type, W.type, Real.type](Real)(V, W)
  
  lazy val R2x2 = new generic.F2x2[R2.type, R2.type, Real.type](Real)(R2, R2)
  
  lazy val R3x3 = new generic.F3x3[R3.type, R3.type, Real.type](Real)(R3, R3)
  
  lazy val R4x4 = new generic.F4x4[R4.type, R4.type, Real.type](Real)(R4, R4)
}
