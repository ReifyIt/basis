/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** A vector in a `Real` vector space.
  * 
  * @author Chris Sachs
  * 
  * @tparam RealVector  the vector type of the real vector space.
  */
trait RealVector[RealVector] extends Vector[RealVector, Real] {
  /** Multiplies this $vector by a `Double` value on the right.
    * 
    * @param  scalar  the `Double` value to multiply by.
    * @return the scaled $vector.
    */
  def :* (scalar: Double): RealVector
  
  /** Multiplies this $vector by a `Double` value on the left.
    * 
    * @param  scalar  the `Double` value to multiply by.
    * @return the scaled $vector.
    */
  def *: (scalar: Double): RealVector
  
  /** Divides this $vector by a `Double` value.
    * 
    * @param  scalar  the `Double` value to divide by.
    * @return the scaled $vector.
    */
  def / (scalar: Double): RealVector
  
  def :* (scalar: Real): RealVector = this :* scalar.toDouble
  
  def *: (scalar: Real): RealVector = scalar.toDouble *: this
  
  /** Divides this $vector by a $scalar.
    * 
    * @param  scalar  the $scalar to divide by.
    * @return the scaled $vector.
    */
  def / (scalar: Real): RealVector = this / scalar.toDouble
}
