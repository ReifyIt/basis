/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait CompleteField extends Any with Field {
  override type Vector
  
  override def + (that: Vector): Vector
  
  override def unary_- : Vector
  
  override def - (that: Vector): Vector
  
  override def * (that: Vector): Vector
  
  override def inverse: Vector
  
  override def / (that: Vector): Vector
  
  def pow(that: Vector): Vector
  
  def sqrt: Vector
}

object CompleteField {
  trait Space extends Field.Space { self =>
    override type Vector <: CompleteField {
      type Vector = self.Vector
    }
    
    override def zero: Vector
    
    override def unit: Vector
  }
  
  trait Scalar extends Field.Scalar { self =>
    override type Scalar <: CompleteField {
      type Vector = self.Scalar
    }
    
    override def Scalar: CompleteField.Space {
      type Vector = self.Scalar
    }
  }
}
