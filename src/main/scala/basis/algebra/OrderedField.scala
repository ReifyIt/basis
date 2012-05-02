/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait OrderedField extends Any with OrderedRing with Field {
  override type Vector
  
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

object OrderedField {
  trait Space extends OrderedRing.Space with Field.Space { self =>
    override type Vector <: OrderedField {
      type Vector = self.Vector
    }
    
    override def zero: Vector
    
    override def unit: Vector
  }
  
  trait Scalar extends OrderedRing.Scalar with Field.Scalar { self =>
    override type Scalar <: OrderedField {
      type Vector = self.Scalar
    }
    
    override def Scalar: OrderedField.Space {
      type Vector = self.Scalar
    }
  }
}
