/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** An abstract principal homogeneous space over the additive group of a linear
  * space. Vector addition acts freely and transitively over the point set.
  * Every `AffineSpace` is a subtype of the `LinearSpace` it acts over. to the
  * extent practicable, the following affine space axioms should hold.
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
  * @tparam S   The singleton type of the scalar structure of this $Structure.
  * 
  * @define Structure   `AffineSpace`
  * @define point       point
  * @define vector      vector
  * @define scalar      scalar
  */
trait AffineSpace[S <: Ring with Singleton] extends LinearSpace[S] {
  /** A point element of this $Structure. */
  trait Element extends Any {
    /** Returns the affine sum of this $point and some $vector. */
    def + (vector: Vector): Point
    
    /** Returns the affine difference between this $point and some $vector. */
    def - (vector: Vector): Point
    
    /** Returns the affine difference between this $point and another $point. */
    def - (that: Point): Vector
  }
  
  /** The point element type of this $Structure. */
  type Point <: Element
  
  /** The origin point of this $Structure. */
  def origin: Point
}
