/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math

/** A totally ordered abstract ring structure. Addition associates and
  * commutes, and multiplication associates and distributes over addition.
  * Addition and multiplication both have an identity element, and every
  * element has an additive inverse. To the extent practicable, the following
  * axioms should hold.
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
  * '''Order axioms''':
  *   - if 𝑎 <= 𝑏 and 𝑏 <= 𝑎 then 𝑎 == 𝑏 for all elements 𝑎, 𝑏 in `this`.
  *   - if 𝑎 <= 𝑏 and 𝑏 <= 𝑐 then 𝑎 <= 𝑐 for all elements 𝑎, 𝑏, 𝑐 in `this`.
  *   - 𝑎 <= 𝑏 or 𝑏 <= 𝑎 for all elements 𝑎, 𝑏 in `this`.
  * 
  * @example {{{
  * // You can abstract over ordered rings by parameterizing a class or
  * // function with a subtype of OrderedRing with Singleton. Type elements
  * // with the #Element type projection of your OrderedRing type parameter.
  * def testOrderedRingOperations[R <: OrderedRing with Singleton](a: R#Element, b: R#Element, c: R#Element) {
  *   assert(a + b == b + a, "commutativity of addition")
  *   assert((a + b) + c == a + (b + c), "associativity of addition")
  *   assert((a * b) * c == a * (b * c), "associativity of multiplication")
  *   assert(a * (b + c) == (a * b) + (a * c), "distributivity of multiplication over addition")
  *   if (a <= b) assert((a min b) == a, "existence of minima")
  *   if (a <= b) assert((a max b) == b, "existence of maxima")
  * }
  * 
  * // Alternatively, functions can use path-dependent types of an OrderedRing parameter.
  * def testOrderedRingIdentities(R: OrderedRing)(a: R.Element, b: R.Element) {
  *   import R._
  *   assert(zero + a == a, "existence of additive identity")
  *   assert(a + (-a) == zero, "existence of additive inverse")
  *   assert(unit != zero && unit * a == a, "existence of multiplicative identity")
  *   if (a <= b && b <= a) assert(a == b, "antisymmetry of ordering")
  *   if (a <= b && b <= c) assert(a <= c, "transitivity of ordering")
  *   assert(a <= b || b <= a, "totality of ordering")
  * }
  * }}}
  * 
  * @define space   ordered ring
  */
trait OrderedRing extends Ring {
  trait Value extends Any with super.Value {
    override def + (that: Element): Element
    
    override def unary_- : Element
    
    override def - (that: Element): Element
    
    override def * (that: Element): Element
    
    /** Returns the absolute value of this $element. */
    def abs: Element
    
    /** Returns the lesser of this $element and another $element. */
    def min(that: Element): Element
    
    /** Returns the greater of this $element and another $element. */
    def max(that: Element): Element
    
    /** Returns `true` if and only if this $element is strictly less than another $element. */
    def < (that: Element): Boolean
    
    /** Returns `true` if and only if this $element is less than or equal to another $element. */
    def <= (that: Element): Boolean
    
    /** Returns `true` if and only if this $element is strictly greater than another $element. */
    def > (that: Element): Boolean
    
    /** Returns `true` if and only if this $element is greater than or equal to another $element. */
    def >= (that: Element): Boolean
  }
  
  override type Element <: Value
  
  override def zero: Element
  
  override def unit: Element
}
