/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** An abstract principal homogeneous space over the additive group of a linear
  * space. Vector addition acts freely and transitively over the point set.
  * To the extent practicable, the following axioms should hold.
  * 
  * '''Axioms''':
  *   - 𝓅 + `zero` == 𝓅 for every point 𝓅 in `this`.
  *   - (𝓅 + 𝐮) + 𝐯 == 𝓅 + (𝐮 + 𝐯) for every point 𝓅 and all vectors 𝐮, 𝐯 in `this`.
  *   - (𝐯: Vector) => 𝓅 + 𝐯 is a bijection for every point 𝓅 in `this`.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * // You can abstract over affine spaces by parameterizing a class or
  * // function with a subtype of AffineSpace with Singleton. Type elements
  * // with the #Point, #Vector and #Scalar type projections of your
  * // AffineSpace type parameter.
  * def testAffineSpaceOperations[A <: AffineSpace[_] with Singleton](
  *     p: A#Point, q: A#Point, u: A#Vector, v: A#Vector) {
  *   assert((p + u) + v == p + (u + v), "associativity of point-vector addition")
  *   assert(p + (-v) == p - v, "existence of point-vector subtraction")
  *   assert((p + v) - (q + v) == p - q, "existence of point-point subtraction")
  * }
  * 
  * // Alternatively, functions can use path-dependent types of an AffineSpace parameter.
  * def testAffineSpaceIdentities(A: AffineSpace[_])(p: A.Point, q: A.Point, v: A.Vector) {
  *   import A._
  *   assert(p + zero == p, "existence of additive identity vector")
  *   if (p + v == q + v) assert(p == v, "uniqueness of points")
  * }
  * }}}
  * 
  * @tparam S   The scalar set of this $space.
  * 
  * @define space   affine space
  */
trait AffineSpace[S <: Ring with Singleton] extends LinearSpace[S] {
  /** A point in this $space.
    * 
    * @define point   point
    * @define vector  vector
    * @define scalar  scalar
    */
  trait Element extends Any {
    /** Returns the sum of this $point and a $vector. */
    def + (vector: Vector): Point
    
    /** Returns the difference between this $point and a $vector. */
    def - (vector: Vector): Point
    
    /** Returns the difference between this $point and another $point. */
    def - (that: Point): Vector
  }
  
  /** The type of points in this $space. */
  type Point <: Element
  
  /** Returns the origin of this $space. */
  def origin: Point
}
