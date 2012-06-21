/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** An algebraic ring structure. Addition associates and commutes, and
  * multiplication associates and distributes over addition. Addition and
  * multiplication both have an identity element, and every element has an
  * additive inverse. To the extent practicable, the ring axioms should hold.
  * 
  * '''Axioms for addition''':
  *   - if ''a'' is in `this` and ''b'' is in `this`, then their sum ''a'' + ''b'' is in `this`.
  *   - ''a'' + ''b'' == ''b'' + ''a'' for all ''a'', ''b'' in `this`.
  *   - (''a'' + ''b'') + ''c'' == ''a'' + (''b'' + ''c'') for all ''a'', ''b'', ''c'' in `this`.
  *   - `zero` + ''a'' == ''a'' for every ''a'' in `this`.
  *   - to every ''a'' in `this` corresponds an element -''a'' in `this`
  *     such that ''a'' + (-''a'') == `zero`.
  * 
  * '''Axioms for multiplication''':
  *   - if ''a'' is in `this` and ''b'' is in `this`, then their product ''a'' * ''b'' is in `this`.
  *   - (''a'' * ''b'') * ''c'' == ''a'' * (''b'' * ''c'') for all ''a'', ''b'', ''c'' in `this`.
  *   - `unit` != `zero` and `unit` * ''a'' == ''a'' for every ''a'' in `this`.
  * 
  * '''The distributive law''':
  *   - ''a'' * (''b'' + ''c') == ''a'' * ''b'' + ''a'' * ''c'' for all ''a'', ''b'', ''c'' in `this`.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * // You can abstract over rings by parameterizing a class or function with
  * // a subtype of Ring with Singleton. Type elements with the Value type
  * // projection of your Ring type parameter.
  * def testRingOperations[R <: Ring with Singleton](a: R#Value, b: R#Value, c: R#Value) {
  *   assert(a + b == b + a)
  *   assert((a + b) + c == a + (b + c))
  *   assert((a * b) * c == a * (b * c))
  *   assert(a * (b + c) == a * b + a * c)
  * }
  * 
  * // Alternatively, functions can use path-dependent types of a Ring parameter.
  * def testRingIdentities(R: Ring)(a: R.Value) {
  *   import R._
  *   assert(zero + a == a)
  *   assert(a + (-a) == zero)
  *   assert(unit != zero && unit * a == a)
  * }
  * }}}
  * 
  * @define Value   `Ring`
  * @define value   element
  */
trait Ring {
  /** An element of this `Ring`. */
  trait Element extends Any {
    /** Returns the sum of this $value and another $value. */
    def + (that: Value): Value
    
    /** Returns the additive inverse of this $value. */
    def unary_- : Value
    
    /** Returns the difference between this $value and another $value. */
    def - (that: Value): Value
    
    /** Returns the product of this $value multiplied by another $value. */
    def * (that: Value): Value
  }
  
  /** The element type of this $Value. */
  type Value <: Element
  
  /** The additive identity element of this $Value. */
  def zero: Value
  
  /** The multiplicative identity element of this $Value. */
  def unit: Value
}
