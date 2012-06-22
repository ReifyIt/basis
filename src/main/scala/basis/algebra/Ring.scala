/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** An abstract ring structure. Addition associates and commutes, and
  * multiplication associates and distributes over addition. Addition and
  * multiplication both have an identity element, and every element has an
  * additive inverse. To the extent practicable, the following ring axioms
  * should hold.
  * 
  * '''Axioms for addition''':
  *   - if 𝑎 and 𝑏 are elements in `this`, then their sum 𝑎 + 𝑏 is also an element in `this`.
  *   - 𝑎 + 𝑏 == 𝑏 + 𝑎 for all elements 𝑎, 𝑏 in `this`.
  *   - (𝑎 + 𝑏) + 𝑐 == 𝑎 + (𝑏 + 𝑐) for all elements 𝑎, 𝑏, 𝑐 in `this`.
  *   - `this` has an element `zero` such that `zero` + 𝑎 == 𝑎 for every element 𝑎 in `this`.
  *   - to every element 𝑎 in `this` corresponds an element -𝑎 in `this` such that 𝑎 + (-𝑎) == `zero`.
  * 
  * '''Axioms for multiplication''':
  *   - if 𝑎 and 𝑏 are elements in `this`, then their product 𝑎 * 𝑏 is also an element in `this`.
  *   - (𝑎 * 𝑏) * 𝑐 == 𝑎 * (𝑏 * 𝑐) for all elements 𝑎, 𝑏, 𝑐 in `this`.
  *   - `this` has an element `unit` != `zero` such that `unit` * 𝑎 == 𝑎 for every element 𝑎 in `this`.
  * 
  * '''The distributive law''':
  *   - 𝑎 * (𝑏 + 𝑐) == (𝑎 * 𝑏) + (𝑎 * 𝑐) for all elements 𝑎, 𝑏, 𝑐 in `this`.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * // You can abstract over rings by parameterizing a class or
  * // function with a subtype of Ring with Singleton. Type elements
  * // with the #Value type projection of your Ring type parameter.
  * def testRingOperations[R <: Ring with Singleton](a: R#Value, b: R#Value, c: R#Value) {
  *   assert(a + b == b + a, "commutativity of addition")
  *   assert((a + b) + c == a + (b + c), "associativity of addition")
  *   assert((a * b) * c == a * (b * c), "associativity of multiplication")
  *   assert(a * (b + c) == (a * b) + (a * c), "distributivity of multiplication over addition")
  * }
  * 
  * // Alternatively, functions can use path-dependent types of a Ring parameter.
  * def testRingIdentities(R: Ring)(a: R.Value) {
  *   import R._
  *   assert(zero + a == a, "existence of additive identity")
  *   assert(a + (-a) == zero, "existence of additive inverse")
  *   assert(unit != zero && unit * a == a, "existence of multiplicative identity")
  * }
  * }}}
  * 
  * @define Structure   `Ring`
  * @define element     element
  */
trait Ring {
  /** An element of this $Structure. */
  trait Element extends Any {
    /** Returns the sum of this $element and another $element. */
    def + (that: Value): Value
    
    /** Returns the additive inverse of this $element. */
    def unary_- : Value
    
    /** Returns the difference between this $element and another $element. */
    def - (that: Value): Value
    
    /** Returns the product of this $element times another $element. */
    def * (that: Value): Value
  }
  
  /** The element type of this $Structure. */
  type Value <: Element
  
  /** Returns the additive identity element of this $Structure. */
  def zero: Value
  
  /** Returns the multiplicative identity element of this $Structure. */
  def unit: Value
}
