/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait OrderedRing extends Ring {
  trait Element extends Any with super.Element {
    override def + (that: Vector): Vector
    
    override def unary_- : Vector
    
    override def - (that: Vector): Vector
    
    override def * (that: Vector): Vector
    
    def abs: Vector
    
    def min(that: Vector): Vector
    
    def max(that: Vector): Vector
    
    def < (that: Vector): Boolean
    
    def <= (that: Vector): Boolean
    
    def > (that: Vector): Boolean
    
    def >= (that: Vector): Boolean
  }
  
  override type Vector <: Element
  
  override def zero: Vector
  
  override def unit: Vector
}
