/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** A vector in an `Integer` module.
  * 
  * @author Chris Sachs
  * 
  * @tparam IntegerVector   the vector type of the module.
  */
trait IntegerVector[IntegerVector] extends Vector[IntegerVector, Integer] {
  /** Multiplies this $vector by a `Long` value on the right.
    * 
    * @param  scalar  the `Long` value to multiply by.
    * @return the scaled $vector.
    */
  def :* (scalar: Long): IntegerVector
  
  /** Multiplies this $vector by a `Long` value on the left.
    * 
    * @param  scalar  the `Long` value to multiply by.
    * @return the scaled $vector.
    */
  def *: (scalar: Long): IntegerVector
  
  def :* (scalar: Integer): IntegerVector = this :* scalar.toLong
  
  def *: (scalar: Integer): IntegerVector = scalar.toLong *: this
}
