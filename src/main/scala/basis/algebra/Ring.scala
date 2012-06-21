/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Ring {
  trait Element extends Any {
    def + (that: Value): Value
    
    def unary_- : Value
    
    def - (that: Value): Value
    
    def * (that: Value): Value
  }
  
  type Value <: Element
  
  def zero: Value
  
  def unit: Value
}
