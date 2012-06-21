/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait LinearSpace[S <: Ring with Singleton] {
  trait Element extends Any {
    def + (that: Vector): Vector
    
    def unary_- : Vector
    
    def - (that: Vector): Vector
    
    def :* (scalar: Scalar): Vector
    
    def *: (scalar: Scalar): Vector
  }
  
  type Vector <: Element
  
  type Scalar = S#Value
  
  def Scalar: S
  
  def zero: Vector
}
