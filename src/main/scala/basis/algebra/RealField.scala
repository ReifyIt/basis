/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait RealField extends Any with OrderedField with CompleteField {
  override type Vector
  
  override def + (that: Vector): Vector
  
  override def unary_- : Vector
  
  override def - (that: Vector): Vector
  
  override def * (that: Vector): Vector
  
  override def inverse: Vector
  
  override def / (that: Vector): Vector
  
  override def pow(that: Vector): Vector
  
  override def sqrt: Vector
  
  override def abs: Vector
  
  override def min(that: Vector): Vector
  
  override def max(that: Vector): Vector
  
  override def < (that: Vector): Boolean
  
  override def <= (that: Vector): Boolean
  
  override def > (that: Vector): Boolean
  
  override def >= (that: Vector): Boolean
}

object RealField {
  trait Space extends OrderedField.Space with CompleteField.Space { self =>
    override type Vector <: RealField {
      type Vector = self.Vector
    }
    
    override def zero: Vector
    
    override def unit: Vector
  }
  
  trait Scalar extends OrderedField.Scalar with CompleteField.Scalar { self =>
    override type Scalar <: RealField {
      type Vector = self.Scalar
    }
    
    override def Scalar: RealField.Space {
      type Vector = self.Scalar
    }
  }
}
