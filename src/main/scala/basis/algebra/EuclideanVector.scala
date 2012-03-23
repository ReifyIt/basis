/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait EuclideanVector[Vector, Scalar] extends LinearVector[Vector, Scalar] {
  def / (scalar: Scalar): Vector
  
  def ⋅ (that: Vector): Scalar
  
  def norm: Scalar
}
