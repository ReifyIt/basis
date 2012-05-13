/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait AffineSpace[S <: Ring with Singleton] extends LinearSpace[S] {
  trait Element extends Any {
    def + (vector: Vector): Point
    
    def - (vector: Vector): Point
    
    def - (that: Point): Vector
  }
  
  type Point <: Element
  
  def origin: Point
}
