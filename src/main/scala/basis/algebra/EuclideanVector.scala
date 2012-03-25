/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** A vector in a euclidean space.
  * 
  * @author Chris Sachs
  * 
  * @tparam EuclideanVector   the vector type of the euclidean space.
  * @tparam Scalar            the scalar type of the euclidean space.
  */
trait EuclideanVector[EuclideanVector, Scalar] extends Vector[EuclideanVector, Scalar] {
  /** Divides this $vector by a $scalar. Equivalent to multiplying this $vector
    * by the $scalar's multiplicative inverse.
    * 
    * @param  scalar  the $scalar to divide by.
    * @return the scaled $vector.
    */
  def / (scalar: Scalar): EuclideanVector
  
  /** Returns the dot product of this $vector and another $vector. The method
    * name uses the unicode dot operator U+22C5.
    * 
    * @param  that  the other $vector.
    * @return the scalar product of this $vector and the other $vector.
    */
  def ⋅ (that: EuclideanVector): Scalar
  
  /** Returns the magnitude of this $vector. */
  def norm: Scalar
}
