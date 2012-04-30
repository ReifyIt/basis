/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

trait RealVector extends Any with Vector {
  override type Vector
  
  override type Scalar = Real
  
  override def N: Int
  
  override def apply(i: Int): Real
  
  override def + (that: Vector): Vector
  
  override def unary_- : Vector
  
  override def - (that: Vector): Vector
  
  override def :* (scalar: Real): Vector
  
  override def *: (scalar: Real): Vector
  
  override def ⋅ (that: Vector): Real
}
