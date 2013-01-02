/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math

/** An abstract ring structure. Addition associates and commutes, and
  * multiplication associates and distributes over addition. Addition and
  * multiplication both have an identity element, and every element has an
  * additive inverse. To the extent practicable, the following axioms
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
  * @example {{{
  * // You can abstract over rings by parameterizing a class or
  * // function with a subtype of Ring with Singleton. Type elements
  * // with the #Element type projection of your Ring type parameter.
  * def testRingOperations[R <: Ring with Singleton](a: R#Element, b: R#Element, c: R#Element) {
  *   assert(a + b == b + a, "commutativity of addition")
  *   assert((a + b) + c == a + (b + c), "associativity of addition")
  *   assert((a * b) * c == a * (b * c), "associativity of multiplication")
  *   assert(a * (b + c) == (a * b) + (a * c), "distributivity of multiplication over addition")
  * }
  * 
  * // Alternatively, functions can use path-dependent types of a Ring parameter.
  * def testRingIdentities(R: Ring)(a: R.Element) {
  *   import R._
  *   assert(zero + a == a, "existence of additive identity")
  *   assert(a + (-a) == zero, "existence of additive inverse")
  *   assert(unit != zero && unit * a == a, "existence of multiplicative identity")
  * }
  * }}}
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * 
  * @define space   ring
  */
trait Ring {
  /** An element in this $space.
    * 
    * @define element   element
    */
  trait Value extends Any {
    /** Returns the sum of this $element and another $element. */
    def + (that: Element): Element
    
    /** Returns the additive inverse of this $element. */
    def unary_- : Element
    
    /** Returns the difference between this $element and another $element. */
    def - (that: Element): Element
    
    /** Returns the product of this $element times another $element. */
    def * (that: Element): Element
  }
  
  /** The type of elements in this $space. */
  type Element <: Value
  
  /** Returns the additive identity of this $space. */
  def zero: Element
  
  /** Returns the multiplicative identity of this $space. */
  def unit: Element
}
