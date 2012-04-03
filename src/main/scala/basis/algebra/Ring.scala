/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** An element of a commutative ring. A ring is an abelian group under addition
  * and a semigroup under multiplication. Multiplication distributes over addition.
  * 
  * $LongMorphismInfo
  * 
  * @author Chris Sachs
  * 
  * @tparam Ring  the element type of the ring.
  * 
  * @define LongMorphismInfo
  * A `$Element` has a morphism from `Long` values to $element values.
  * An `Long` value ''n'' represents the ''nth'' addition of the `$Element`'s
  * multiplicative identity to its additive identity.
  * 
  * @define vector    $element
  * @define scalar    $element
  * @define Element   Ring
  * @define element   `Ring` value
  */
trait Ring[Ring] extends Vector[Ring, Ring] {
  /** Adds an `Long` value to this $element.
    * 
    * @param  n   the `Long` value to add.
    * @return the sum of this $element and the `Long` value.
    */
  def + (n: Long): Ring
  
  /** Subtracts an `Long` value from this $element.
    * 
    * @param  n   the `Long` value to subtract.
    * @return the difference of this $element and the `Long` value.
    */
  def - (n: Long): Ring
  
  /** Multiplies this $element by another $element.
    * 
    * @param  that  the $element to multiply by.
    * @return the product of this $element and the other $element.
    */
  def * (that: Ring): Ring
  
  /** Multiplies this $element by an `Long` value.
    * 
    * @param  n   the `Long` value to multiply by.
    * @return the product of this $element and the `Long` value.
    */
  def * (n: Long): Ring
  
  /** Raises this $element to a positive `Long` power.
    * 
    * @param  n   the `Long` exponent.
    * @return this $element multiplied by itself `n` times.
    */
  def pow(n: Long): Ring
  
  def :* (that: Ring): Ring = this * that
  
  def *: (that: Ring): Ring = this * that
}
