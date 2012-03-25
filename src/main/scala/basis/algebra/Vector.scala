/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** A vector in a module.
  * 
  * @author Chris Sachs
  * 
  * @define point   $vector
  * @define vector  vector
  * @define nospace
  * 
  * @tparam Vector  the vector type of the module.
  * @tparam Scalar  the scalar type of the module.
  */
trait Vector[Vector, -Scalar] extends AffinePoint[Vector, Vector, Scalar] {
  /** Adds a $vector to this $vector.
    * 
    * @param  that  the $vector to add.
    * @return the vector sum of this $vector and the other $vector.
    */
  def + (that: Vector): Vector
  
  /** Negates this $vector.
    * 
    * @return the vector opposite of this $vector.
    */
  def unary_- : Vector
  
  /** Subtracts a $vector from this $vector.
    * 
    * @param  that  the $vector to subtract.
    * @return the vector difference of this $vector and the other $vector.
    */
  def - (that: Vector): Vector
  
  /** Multiples this $vector by a $scalar on the right.
    * 
    * @param  scalar  the $scalar to multiply by.
    * @return the scaled $vector.
    */
  def :* (scalar: Scalar): Vector
  
  /** Multiplies this $vector by a $scalar on the left.
    * 
    * @param  scalar  the $scalar to multiply by.
    * @return the scaled $vector.
    */
  def *: (scalar: Scalar): Vector
  
  def :+ (that: Vector): Vector = this + that
  
  def :- (that: Vector): Vector = this - that
}
