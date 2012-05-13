/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Field extends Ring {
  trait Element extends Any with super.Element {
    override def + (that: Vector): Vector
    
    override def unary_- : Vector
    
    override def - (that: Vector): Vector
    
    override def * (that: Vector): Vector
    
    def inverse: Vector
    
    def / (that: Vector): Vector
  }
  
  override type Vector <: Element
  
  override def zero: Vector
  
  override def unit: Vector
}
