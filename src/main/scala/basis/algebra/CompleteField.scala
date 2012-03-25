/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** An element of a complete field.
  * 
  * $IntMorphismInfo
  * 
  * $DoubleMorphismInfo
  * 
  * @author Chris Sachs
  * 
  * @define DoubleMorphismInfo
  * A `$Element` also has a morphism from `Double` values to `$Element` values.
  * A `Double` value ''x'' represents a rational approximation to an element of
  * this `$Element`.
  * 
  * @define NotAlgebraicallyClosedWarning
  * A `$Element` isn't required to be algebraically closed. So don't assume it
  * contains the roots of all values.
  * 
  * @define Element   CompleteField
  * @define element   `CompleteField` value
  * @define nospace
  * 
  * @tparam CompleteField   the element type of the complete field.
  */
trait CompleteField[CompleteField] extends Field[CompleteField] {
  /** Adds a `Double` value to this $element.
    * 
    * @param  x   the `Double` value to add.
    * @return the sum of this $element and the `Double` value.
    */
  def + (x: Double): CompleteField
  
  /** Subtracts a `Double` value from this $element.
    * 
    * @param  x   the `Double` value to subtract.
    * @return the difference of this $element and the `Double` value.
    */
  def - (x: Double): CompleteField
  
  /** Multiplies this $element by a `Double` value.
    * 
    * @param  x   the `Double` value to multiply by.
    * @return the product of this $element and the `Double` value.
    */
  def * (x: Double): CompleteField
  
  /** Divides this $element by a `Double` value.
    * 
    * @param  x   the `Double` value to divide by.
    * @return the quotient of this $element and the `Double` value.
    */
  def / (x: Double): CompleteField
  
  /** Raises this $element to a `$Element` power. $NotAlgebraicallyClosedWarning
    * 
    * @param  that  the `$Element` exponent.
    * @return the exponentiation of this $element by the other $element.
    */
  def pow(that: CompleteField): CompleteField
  
  /** Raises this $element to a `Double` power. $NotAlgebraicallyClosedWarning
    * 
    * @param  x   the `Double` exponent.
    * @return the exponentiation of this $element by the `Double` exponent.
    */
  def pow(x: Double): CompleteField
  
  /** Returns the square root of this $element. Raises this $element to the ½ power.
    * $NotAlgebraicallyClosedWarning */
  def sqrt: CompleteField
  
  def + (n: Int): CompleteField = this + n.toDouble
  
  def - (n: Int): CompleteField = this - n.toDouble
  
  def * (n: Int): CompleteField = this * n.toDouble
  
  def / (n: Int): CompleteField = this / n.toDouble
  
  /** Raises this $element to an `Int` power.
    * 
    * @param  n   the `Int` exponent.
    * @return the exponentiation of this $element by the `Int` exponent.
    */
  def pow(n: Int): CompleteField = pow(n.toDouble)
}
