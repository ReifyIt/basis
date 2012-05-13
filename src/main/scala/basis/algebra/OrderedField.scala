/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait OrderedField extends OrderedRing with Field {
  trait Element extends Any with super[OrderedRing].Element with super[Field].Element {
    override def + (that: Vector): Vector
    
    override def unary_- : Vector
    
    override def - (that: Vector): Vector
    
    override def * (that: Vector): Vector
    
    override def inverse: Vector
    
    override def / (that: Vector): Vector
    
    override def abs: Vector
    
    override def min(that: Vector): Vector
    
    override def max(that: Vector): Vector
    
    override def < (that: Vector): Boolean
    
    override def <= (that: Vector): Boolean
    
    override def > (that: Vector): Boolean
    
    override def >= (that: Vector): Boolean
  }
  
  override type Vector <: Element
  
  override def zero: Vector
  
  override def unit: Vector
}
