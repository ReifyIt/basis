/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.math

/** A complete abstract field structure. Addition associates and commutes,
  * and multiplication associates, commutes, and distributes over addition.
  * Addition and multiplication both have an identity element, every element
  * has an additive inverse, and every element except zero has a multiplicative
  * inverse. ''Completeness'' implies that every Cauchy sequence of elements
  * converges. To the extent practicable, the following axioms should hold.
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
  *   - 𝑎 * 𝑏 == 𝑏 * 𝑎 for all elements 𝑎, 𝑏 in `this`.
  *   - (𝑎 * 𝑏) * 𝑐 == 𝑎 * (𝑏 * 𝑐) for all elements 𝑎, 𝑏, 𝑐 in `this`.
  *   - `this` has an element `unit` != `zero` such that `unit` * 𝑎 == 𝑎 for every element 𝑎 in `this`.
  *   - if 𝑎 is in `this` and 𝑎 != `zero` then there exists an element 𝑎.`inverse` such that 𝑎 * 𝑎.`inverse` == `unit`.
  * 
  * '''The distributive law''':
  *   - 𝑎 * (𝑏 + 𝑐) == (𝑎 * 𝑏) + (𝑎 * 𝑐) for all elements 𝑎, 𝑏, 𝑐 in `this`.
  * 
  * '''Completeness axiom''':
  *   - every non-empty subset of `this` with an upper bound has a least upper bound.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * // You can abstract over complete fields by parameterizing a class or
  * // function with a subtype of CompleteField with Singleton. Type elements
  * // with the #Element type projection of your CompleteField type parameter.
  * def testCompleteFieldOperations[F <: CompleteField with Singleton](a: F#Element, b: F#Element, c: F#Element) {
  *   assert(a + b == b + a, "commutativity of addition")
  *   assert((a + b) + c == a + (b + c), "associativity of addition")
  *   assert(a * b == b * a, "commutativity of multiplication")
  *   assert((a * b) * c == a * (b * c), "associativity of multiplication")
  *   assert(a * (b + c) == (a * b) + (a * c), "distributivity of multiplication over addition")
  * }
  * 
  * // Alternatively, functions can use path-dependent types of a CompleteField parameter.
  * def testCompleteFieldIdentities(F: CompleteField)(a: F.Element) {
  *   import F._
  *   assert(zero + a == a, "existence of additive identity")
  *   assert(a + (-a) == zero, "existence of additive inverse")
  *   assert(unit != zero && unit * a == a, "existence of multiplicative identity")
  *   assert(a * a.inverse == unit, "existence of multiplicative inverse")
  * }
  * }}}
  * 
  * @define space   complete field
  */
trait CompleteField extends Field {
  trait Value extends Any with super.Value {
    override def + (that: Element): Element
    
    override def unary_- : Element
    
    override def - (that: Element): Element
    
    override def * (that: Element): Element
    
    override def inverse: Element
    
    override def / (that: Element): Element
    
    /** Returns the exponentiation of this $element raised to the power of another $element. */
    def pow(that: Element): Element
    
    /** Returns the square root of this $element. */
    def sqrt: Element
  }
  
  override type Element <: Value
  
  override def zero: Element
  
  override def unit: Element
}
