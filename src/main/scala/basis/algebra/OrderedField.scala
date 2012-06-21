/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** A totally ordered algebraic field structure. Addition associates and
  * commutes, and multiplication associates, commutes, and distributes over
  * addition. Addition and multiplication both have an identity element, every
  * element has an additive inverse, and every element except zero has a
  * multiplicative inverse. Every `OrderedField` is also an `OrderedRing`. To
  * the extent practicable, the field axioms and the order axioms should hold.
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
  *   - ''a'' * ''b'' == ''b'' * ''a'' for all ''a'', ''b'' in `this`.
  *   - (''a'' * ''b'') * ''c'' == ''a'' * (''b'' * ''c'') for all ''a'', ''b'', ''c'' in `this`.
  *   - `unit` != `zero` and `unit` * ''a'' == ''a'' for every ''a'' in `this`.
  *   - if ''a'' is in `this` and ''a'' != `zero` then there exisrs an element
  *     ''a''.`inverse` such that ''a'' * ''a''.`inverse` == `unit`.
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
  * // You can abstract over ordered fields by parameterizing a class or function
  * // with a subtype of OrderedField with Singleton. Type elements with the Value
  * // type projection of your OrderedField type parameter.
  * def testOrderedFieldOperations[F <: OrderedField with Singleton](a: F#Value, b: F#Value, c: F#Value) {
  *   assert(a + b == b + a)
  *   assert((a + b) + c == a + (b + c))
  *   assert(a * b == b * a)
  *   assert((a * b) * c == a * (b * c))
  *   assert(a * (b + c) == a * b + a * c)
  *   if (a < b) assert((a min b) == a)
  *   if (a < b) assert((a max b) == b)
  * }
  * 
  * // Alternatively, functions can use path-dependent types of an OrderedField parameter.
  * def testOrderedFieldIdentities(F: OrderedField)(a: F.Value, b: F.Value) {
  *   import F._
  *   assert(zero + a == a)
  *   assert(a + (-a) == zero)
  *   assert(unit != zero && unit * a == a)
  *   assert(a * a.inverse == unit)
  *   if (a <= b && b <= a) assert(a == b)
  *   if (a <= b && b <= c) assert(a <= c)
  *   assert(a <= b || b <= a)
  * }
  * }}}
  * 
  * @define Value   `OrderedField`
  * @define value   element
  */
trait OrderedField extends OrderedRing with Field {
  /** An element of this `OrderedField`. */
  trait Element extends Any with super[OrderedRing].Element with super[Field].Element {
    override def + (that: Value): Value
    
    override def unary_- : Value
    
    override def - (that: Value): Value
    
    override def * (that: Value): Value
    
    override def inverse: Value
    
    override def / (that: Value): Value
    
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
