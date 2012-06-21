/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** An algebraic field structure. Addition associates and commutes, and
  * multiplication associates, commutes, and distributes over addition.
  * Addition and multiplication both have an identity element, every element
  * has an additive inverse, and every element except zero has a multiplicative
  * inverse. Every `Field` is also a `Ring`. To the extent practicable, the
  * field axioms should hold.
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
  * @author Chris Sachs
  * 
  * @example {{{
  * // You can abstract over fields by parameterizing a class or function with
  * // a subtype of Field with Singleton. Type elements with the Value type
  * // projection of your Field type parameter.
  * def testFieldOperations[F <: Field with Singleton](a: F#Value, b: F#Value, c: F#Value) {
  *   assert(a + b == b + a)
  *   assert((a + b) + c == a + (b + c))
  *   assert(a * b == b * a)
  *   assert((a * b) * c == a * (b * c))
  *   assert(a * (b + c) == a * b + a * c)
  * }
  * 
  * // Alternatively, functions can use path-dependent types of a Field parameter.
  * def testFieldIdentities(F: Field)(a: F.Value) {
  *   import F._
  *   assert(zero + a == a)
  *   assert(a + (-a) == zero)
  *   assert(unit != zero && unit * a == a)
  *   assert(a * a.inverse == unit)
  * }
  * }}}
  * 
  * @define Value   `Field`
  * @define value   element
  */
trait Field extends Ring {
  /** An element of this `Field`. */
  trait Element extends Any with super.Element {
    override def + (that: Value): Value
    
    override def unary_- : Value
    
    override def - (that: Value): Value
    
    override def * (that: Value): Value
    
    /** Returns the multiplicative inverse of this $value. */
    def inverse: Value
    
    /** Returns the quotient of this $value divided by another $value. */
    def / (that: Value): Value
  }
  
  override type Value <: Element
  
  override def zero: Value
  
  override def unit: Value
}
