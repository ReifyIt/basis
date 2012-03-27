/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** An element of a field.
  * 
  * $IntMorphismInfo
  * 
  * @author Chris Sachs
  * 
  * @tparam Field   the element type of the field.
  * 
  * @define Element   Field
  * @define element   `Field` value
  */
trait Field[Field] extends Ring[Field] {
  /** Returns the multiplicative inverse of this $element. */
  def reciprocal: Field
  
  /** Divides this $element by another $element.
    * 
    * @param  that  the $element to divide by.
    * @return the quotient of this $element and the other $element.
    */
  def / (that: Field): Field
  
  /** Divides this $element by an `Int` value.
    * 
    * @param  n   the `Int` value to divide by.
    * @return the quotient of this $element and the `Int` value.
    */
  def / (n: Int): Field
}
