/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** A complete, totally ordered algebraic field structure. Addition associates
  * and commutes, and multiplication associates, commutes, and distributes over
  * addition. Addition and multiplication both have an identity element, every
  * element has an additive inverse, and every element except zero has a
  * multiplicative inverse. Also every cauchy sequence of elements converges.
  * Every `RealField` is also an `OrderedField` and a `CompleteField`. To the
  * extent practicable, the real field axioms should hold.
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
  * '''Completeness axiom''':
  *   - every non-empty subset of `this` with an upper bound has a least upper bound.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * // You can abstract over real fields by parameterizing a class or function
  * // with a subtype of RealField with Singleton. Type elements with the Value
  * // type projection of your RealField type parameter.
  * def testRealFieldOperations[R <: RealField with Singleton](a: R#Value, b: R#Value, c: R#Value) {
  *   assert(a + b == b + a)
  *   assert((a + b) + c == a + (b + c))
  *   assert(a * b == b * a)
  *   assert((a * b) * c == a * (b * c))
  *   assert(a * (b + c) == a * b + a * c)
  *   assert((x * x).sqrt == x)
  *   if (a < b) assert((a min b) == a)
  *   if (a < b) assert((a max b) == b)
  * }
  * 
  * // Alternatively, functions can use path-dependent types of a RealField parameter.
  * def testRealFieldIdentities(R: RealField)(a: R.Value, b: R.Value) {
  *   import R._
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
  * @define Value   `RealField`
  * @define value   value
  */
trait RealField extends OrderedField with CompleteField {
  /** An element of this `RealField`. */
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
