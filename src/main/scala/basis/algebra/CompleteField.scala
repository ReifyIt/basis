/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** A complete algebraic field structure. Addition associates and commutes,
  * and multiplication associates, commutes, and distributes over addition.
  * Addition and multiplication both have an identity element, every element
  * has an additive inverse, and every element except zero has a multiplicative
  * inverse. ''Completeness'' implies that every cauchy sequence of elements
  * converges. Every `CompleteField` is also a `Field`. To the extent
  * practicable, the complete field axioms should hold.
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
  * '''Completeness axiom''':
  *   - every non-empty subset of `this` with an upper bound has a least upper bound.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * // You can abstract over complete fields by parameterizing a class or function
  * // with a subtype of CompleteField with Singleton. Type elements with the Value
  * // type projection of your CompleteField type parameter.
  * def testCompleteFieldOperations[F <: CompleteField with Singleton](a: F#Value, b: F#Value, c: F#Value) {
  *   assert(a + b == b + a)
  *   assert((a + b) + c == a + (b + c))
  *   assert(a * b == b * a)
  *   assert((a * b) * c == a * (b * c))
  *   assert(a * (b + c) == a * b + a * c)
  *   assert((x * x).sqrt == x)
  * }
  * 
  * // Alternatively, functions can use path-dependent types of a CompleteField parameter.
  * def testCompleteFieldIdentities(F: CompleteField)(a: F.Value) {
  *   import F._
  *   assert(zero + a == a)
  *   assert(a + (-a) == zero)
  *   assert(unit != zero && unit * a == a)
  *   assert(a * a.inverse == unit)
  * }
  * }}}
  * 
  * @define Value   `CompleteField`
  * @define value   element
  */
trait CompleteField extends Field {
  /** An element of this `CompleteField`. */
  trait Element extends Any with super.Element {
    override def + (that: Value): Value
    
    override def unary_- : Value
    
    override def - (that: Value): Value
    
    override def * (that: Value): Value
    
    override def inverse: Value
    
    override def / (that: Value): Value
    
    /** Returns the exponentiation of this $value raised to the power of another $value. */
    def pow(that: Value): Value
    
    /** Returns the square root of this positive $value. */
    def sqrt: Value
  }
  
  override type Value <: Element
  
  override def zero: Value
  
  override def unit: Value
}
