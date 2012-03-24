/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

trait EuclideanVector[EuclideanVector, Scalar] extends Vector[EuclideanVector, Scalar] {
  def / (scalar: Scalar): EuclideanVector
  
  def ⋅ (that: EuclideanVector): Scalar
  
  def norm: Scalar
}
