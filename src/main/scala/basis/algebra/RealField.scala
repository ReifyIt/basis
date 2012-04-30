/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait RealField extends Any with OrderedRing with CompleteField {
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
