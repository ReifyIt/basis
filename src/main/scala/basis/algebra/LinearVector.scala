/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait LinearVector[Vector, -Scalar] extends AffinePoint[Vector, Vector, Scalar] {
  def + (that: Vector): Vector
  
  def unary_- : Vector
  
  def - (that: Vector): Vector
  
  def :* (scalar: Scalar): Vector
  
  def *: (scalar: Scalar): Vector
  
  def :+ (that: Vector): Vector = this + that
  
  def :- (that: Vector): Vector = this - that
}
