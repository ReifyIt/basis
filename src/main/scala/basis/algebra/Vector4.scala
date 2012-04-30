/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait Vector4 extends Any with Vector {
  override type Vector
  
  override type Scalar
  
  def x: Scalar
  def y: Scalar
  def z: Scalar
  def w: Scalar
  
  override def N: Int
  
  override def apply(i: Int): Scalar
  
  override def + (that: Vector): Vector
  
  override def unary_- : Vector
  
  override def - (that: Vector): Vector
  
  override def :* (scalar: Scalar): Vector
  
  override def *: (scalar: Scalar): Vector
  
  def ⋅ (that: Vector): Scalar
}
