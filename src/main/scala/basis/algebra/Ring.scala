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
  * $IntMorphismInfo
  * 
  * @author Chris Sachs
  * 
  * @define IntMorphismInfo
  * A `$Element` has a morphism from `Int` values to $element values.
  * An `Int` value ''n'' represents the ''nth'' addition of the `$Element`'s
  * multiplicative identity to its additive identity.
  * 
  * @define vector    $element
  * @define Element   Ring
  * @define element   `Ring` value
  * @define nospace
  * 
  * @tparam Ring  the element type of the ring.
  */
trait Ring[Ring] extends Vector[Ring, Ring] {
  /** Adds an `Int` value to this $element.
    * 
    * @param  n   the `Int` value to add.
    * @return the sum of this $element and the `Int` value.
    */
  def + (n: Int): Ring
  
  /** Subtracts an `Int` value from this $element.
    * 
    * @param  n   the `Int` value to subtract.
    * @return the difference of this $element and the `Int` value.
    */
  def - (n: Int): Ring
  
  /** Multiplies this $element by another $element.
    * 
    * @param  that  the $element to multiply by.
    * @return the product of this $element and the other $element.
    */
  def * (that: Ring): Ring
  
  /** Multiplies this $element by an `Int` value.
    * 
    * @param  n   the `Int` value to multiply by.
    * @return the product of this $element and the `Int` value.
    */
  def * (n: Int): Ring
  
  /** Raises this $element to a positive `Int` power.
    * 
    * @param  n   the `Int` exponent.
    * @return this $element multiplied by itself `n` times.
    */
  def pow(n: Int): Ring
  
  def :* (that: Ring): Ring = this * that
  
  def *: (that: Ring): Ring = this * that
}
