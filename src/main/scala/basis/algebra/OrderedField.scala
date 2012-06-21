/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait OrderedField extends OrderedRing with Field {
  trait Element extends Any with super[OrderedRing].Element with super[Field].Element {
    override def + (that: Value): Value
    
    override def unary_- : Value
    
    override def - (that: Value): Value
    
    override def * (that: Value): Value
    
    override def inverse: Value
    
    override def / (that: Value): Value
    
    override def abs: Value
    
    override def min(that: Value): Value
    
    override def max(that: Value): Value
    
    override def < (that: Value): Boolean
    
    override def <= (that: Value): Boolean
    
    override def > (that: Value): Boolean
    
    override def >= (that: Value): Boolean
  }
  
  override type Value <: Element
  
  override def zero: Value
  
  override def unit: Value
}
