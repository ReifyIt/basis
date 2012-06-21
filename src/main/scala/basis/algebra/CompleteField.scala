/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait CompleteField extends Field {
  trait Element extends Any with super.Element {
    override def + (that: Value): Value
    
    override def unary_- : Value
    
    override def - (that: Value): Value
    
    override def * (that: Value): Value
    
    override def inverse: Value
    
    override def / (that: Value): Value
    
    def pow(that: Value): Value
    
    def sqrt: Value
  }
  
  override type Value <: Element
  
  override def zero: Value
  
  override def unit: Value
}
