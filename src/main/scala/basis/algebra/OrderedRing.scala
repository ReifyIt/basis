/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** A totally ordered algebraic ring structure. Addition associates and
  * commutes, and multiplication associates and distributes over addition.
  * Addition and multiplication both have an identity element, and every
  * element has an additive inverse. To the extent practicable, the ring axioms
  * and order axioms should hold.
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
  * '''Order axioms''':
  *   - if ''a'' <= ''b'' and ''b'' <= ''a'' then ''a'' == ''b'' for all ''a'', ''b'' in `this`.
  *   - if ''a'' <= ''b'' and ''b'' <= ''c'' then ''a'' <= ''c'' for all ''a'', ''b'', ''c'' in `this`.
  *   - ''a'' <= ''b'' or ''b'' <= ''a'' for all ''a'', ''b'' im `this`.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * // You can abstract over ordered rings by parameterizing a class or function
  * // with a subtype of OrderedRing with Singleton. Type elements with the Value
  * // type projection of your OrderedRing type parameter.
  * def testOrderedRingOperations[R <: OrderedRing with Singleton](a: R#Value, b: R#Value, c: R#Value) {
  *   assert(a + b == b + a)
  *   assert((a + b) + c == a + (b + c))
  *   assert((a * b) * c == a * (b * c))
  *   assert(a * (b + c) == a * b + a * c)
  *   if (a < b) assert((a min b) == a)
  *   if (a < b) assert((a max b) == b)
  * }
  * 
  * // Alternatively, functions can use path-dependent types of an OrderedRing parameter.
  * def testOrderedRingIdentities(R: OrderedRing)(a: R.Value, b: R.Value) {
  *   import R._
  *   assert(zero + a == a)
  *   assert(a + (-a) == zero)
  *   assert(unit != zero && unit * a == a)
  *   if (a <= b && b <= a) assert(a == b)
  *   if (a <= b && b <= c) assert(a <= c)
  *   assert(a <= b || b <= a)
  * }
  * }}}
  * 
  * @define Value   `OrderedRing`
  * @define value   element
  */
trait OrderedRing extends Ring {
  /** An element of this `OrderedRing`. */
  trait Element extends Any with super.Element {
    override def + (that: Value): Value
    
    override def unary_- : Value
    
    override def - (that: Value): Value
    
    override def * (that: Value): Value
    
    /** Returns the absolute value of this $value. */
    def abs: Value
    
    /** Returns the lesser value of this $value and another $value. */
    def min(that: Value): Value
    
    /** Returns the greater value of this $value and another $value. */
    def max(that: Value): Value
    
    /** Returns `true` if this $value is strictly less than another $value. */
    def < (that: Value): Boolean
    
    /** Returns `true` if this $value is less than or equal to another $value. */
    def <= (that: Value): Boolean
    
    /** Returns `true` if this $value is strictly greater than another $value. */
    def > (that: Value): Boolean
    
    /** Returns `true` if this $value is greater than or equal to another $value. */
    def >= (that: Value): Boolean
  }
  
  override type Value <: Element
  
  override def zero: Value
  
  override def unit: Value
}
