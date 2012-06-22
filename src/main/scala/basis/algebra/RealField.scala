/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** A complete, totally ordered abstract field structure. Addition associates
  * and commutes, and multiplication associates, commutes, and distributes over
  * addition. Addition and multiplication both have an identity element, every
  * element has an additive inverse, and every element except zero has a
  * multiplicative inverse. Also, every Cauchy sequence of elements converges.
  * Every `RealField` is also an `OrderedField` and a `CompleteField`. To the
  * extent practicable, the following real field axioms should hold.
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
  *   - 𝑎 * 𝑏 == 𝑏 * 𝑎 for all 𝑎, 𝑏 elements in `this`.
  *   - (𝑎 * 𝑏) * 𝑐 == 𝑎 * (𝑏 * 𝑐) for all elements 𝑎, 𝑏, 𝑐 in `this`.
  *   - `this` has an element `unit` != `zero` such that `unit` * 𝑎 == 𝑎 for every element 𝑎 in `this`.
  *   - if 𝑎 is in `this` and 𝑎 != `zero` then there exists an element 𝑎.`inverse` such that 𝑎 * 𝑎.`inverse` == `unit`.
  * 
  * '''The distributive law''':
  *   - 𝑎 * (𝑏 + 𝑐) == (𝑎 * 𝑏) + (𝑎 * 𝑐) for all elements 𝑎, 𝑏, 𝑐 in `this`.
  * 
  * '''Order axioms''':
  *   - if 𝑎 <= 𝑏 and 𝑏 <= 𝑎 then 𝑎 == 𝑏 for all elements 𝑎, 𝑏 in `this`.
  *   - if 𝑎 <= 𝑏 and 𝑏 <= 𝑐 then 𝑎 <= 𝑐 for all elements 𝑎, 𝑏, 𝑐 in `this`.
  *   - 𝑎 <= 𝑏 or 𝑏 <= 𝑎 for all elements 𝑎, 𝑏 in `this`.
  * 
  * '''Completeness axiom''':
  *   - every non-empty subset of `this` with an upper bound has a least upper bound.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * // You can abstract over real fields by parameterizing a class or
  * // function with a subtype of RealField with Singleton. Type elements
  * // with the #Value type projection of your RealField type parameter.
  * def testRealFieldOperations[R <: RealField with Singleton](a: R#Value, b: R#Value, c: R#Value) {
  *   assert(a + b == b + a, "commutativity of addition")
  *   assert((a + b) + c == a + (b + c), "associativity of addition")
  *   assert(a * b == b * a, "commutativity of multiplication")
  *   assert((a * b) * c == a * (b * c), "associativity of multiplication")
  *   assert(a * (b + c) == (a * b) + (a * c), "distributivity of multiplication over addition")
  *   if (a <= b) assert((a min b) == a, "existence of minima")
  *   if (a <= b) assert((a max b) == b, "existence of maxima")
  * }
  * 
  * // Alternatively, functions can use path-dependent types of a RealField parameter.
  * def testRealFieldIdentities(R: RealField)(a: R.Value, b: R.Value) {
  *   import R._
  *   assert(zero + a == a, "existence of additive identity")
  *   assert(a + (-a) == zero, "existence of additive inverse")
  *   assert(unit != zero && unit * a == a, "existence of multiplicative identity")
  *   assert(a * a.inverse == unit, "existence of multiplicative inverse")
  *   if (a <= b && b <= a) assert(a == b, "antisymmetry of ordering")
  *   if (a <= b && b <= c) assert(a <= c, "transitivity of ordering")
  *   assert(a <= b || b <= a, "totality of ordering")
  * }
  * }}}
  * 
  * @define Structure   `RealField`
  * @define element     value
  */
trait RealField extends OrderedField with CompleteField {
  /** An element of this $Structure. */
  trait Element extends Any with super[OrderedField].Element with super[CompleteField].Element {
    override def + (that: Value): Value
    
    override def unary_- : Value
    
    override def - (that: Value): Value
    
    override def * (that: Value): Value
    
    override def inverse: Value
    
    override def / (that: Value): Value
    
    override def pow(that: Value): Value
    
    override def sqrt: Value
    
    override def abs: Value
    
    override def min(that: Value): Value
    
    override def max(that: Value): Value
    
    override def < (that: Value): Boolean
    
    override def <= (that: Value): Boolean
    
    override def > (that: Value): Boolean
    
    override def >= (that: Value): Boolean
  }
  
  override type Value <: Element
  
  override def zero: Value
  
  override def unit: Value
}
