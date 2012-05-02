/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait OrderedRing extends Any with Ring {
  override type Vector
  
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

object OrderedRing {
  trait Space extends Ring.Space { self =>
    override type Vector <: OrderedRing {
      type Vector = self.Vector
    }
    
    override def zero: Vector
    
    override def unit: Vector
  }
  
  trait Scalar extends Ring.Scalar { self =>
    override type Scalar <: OrderedRing {
      type Vector = self.Scalar
    }
    
    override def Scalar: OrderedRing.Space {
      type Vector = self.Scalar
    }
  }
}
