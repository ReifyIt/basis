//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math

/** An abstract vector space over a ring. Vector addition associates and
  * commutes, and scalar multiplication associates and distributes over vector
  * addition and scalar addition. Vector addition and scalar multiplication
  * both have an identity element, and every vector has an additive inverse.
  * To the extent practicable, the following axioms should hold.
  *
  * '''Axioms for vector addition''':
  *   - if 𝐮 and 𝐯 are vectors in `this`, then their sum 𝐮 + 𝐯 is also a vector in `this`.
  *   - 𝐮 + 𝐯 == 𝐯 + 𝐮 for all vectors 𝐮, 𝐯 in `this`.
  *   - (𝐮 + 𝐯) + 𝐰 == 𝐮 + (𝐯 + 𝐰) for all vectors 𝐮, 𝐯, 𝐰 in `this`.
  *   - `this` has a vector `zero` such that `zero` + 𝐯 == 𝐯 for every vector 𝐯 in `this`.
  *   - to every vector 𝐯 in `this` corresponds a vector -𝐯 in `this` such that 𝐯 + (-𝐯) == `zero`.
  *
  * '''Axioms for scalar multiplication''':
  *   - if 𝑎 is a scalar in `this` and 𝐯 is a vector in `this`, then their product 𝑎 *: 𝐯 is also a vector in `this`.
  *   - (𝑎 * 𝑏) *: 𝐯 == 𝑎 *: (𝑏 *: 𝐯) for all scalars 𝑎, 𝑏 and every vector 𝐯 in `this`.
  *   - `Scalar` has an element `unit` such that `unit` *: 𝐯 == 𝐯 for every vector 𝐯 in `this`.
  *
  * '''Distributive laws''':
  *   - 𝑎 *: (𝐮 + 𝐯) == (𝑎 *: 𝐮) + (𝑎 *: 𝐯) for every scalar 𝑎 and all vectors 𝐮, 𝐯 in `this`.
  *   - (𝑎 + 𝑏) *: 𝐯 == (𝑎 *: 𝐯) + (𝑏 *: 𝐯) for all scalars 𝑎, 𝑏 and every vector 𝐯 in `this`.
  *
  * @example {{{
  * // You can abstract over vector spaces by parameterizing a class or
  * // function with a subtype of VectorSpace with Singleton. Type elements
  * // with the #Vector and #Scalar type projections of your VectorSpace
  * // type parameter.
  * def testVectorSpaceOperations[V <: VectorSpace with Singleton]
  *     (a: V#Scalar, b: V#Scalar, u: V#Vector, v: V#Vector, w: V#Vector) {
  *   assert(u + v == v + u, "commutativity of vector addition")
  *   assert((u + v) + w == u + (v + w), "associativity of vector addition")
  *   assert((a * b) *: v == a *: (b *: v), "associativity of scalar multiplication with ring multiplication")
  *   assert(a *: (u + v) == (a *: u) + (a *: v), "distributivity of scalar multiplication over vector addition")
  *   assert((a + b) *: v == (a *: v) + (b *: v), "distributivity of scalar multiplication over ring addition")
  * }
  *
  * // Alternatively, functions can use path-dependent types of a VectorSpace parameter.
  * def testVectorSpaceIdentities(V: VectorSpace)(a: V.Scalar, v: V.Vector): Unit = {
  *   import V._
  *   assert(zero + v == v, "existence of additive identity vector")
  *   assert(v + (-v) == zero, "existence of additive inverse vector")
  *   assert(Scalar.unit *: v == v, "existence of multiplicative identity scalar")
  * }
  * }}}
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Structures
  *
  * @define space   vector space
  */
trait VectorSpace {
  /** The type of vectors in this $space. */
  type Vector <: VectorElement

  /** The type of scalars in this $space. */
  type Scalar = Scalar.Element

  /** Returns the scalar set of this $space. */
  val Scalar: Ring

  /** Returns the additive identity of this $space. */
  def zero: Vector

  /** A vector in this $space.
    *
    * @define vector  vector
    * @define scalar  scalar
    */
  trait VectorElement extends Any {
    /** Returns sum of this $vector and another $vector. */
    def + (that: Vector): Vector

    /** Returns the additive inverse of this $vector. */
    def unary_- : Vector

    /** Returns the difference between this $vector and another $vector. */
    def - (that: Vector): Vector

    /** Returns the product of this $vector times a $scalar on the right. */
    def :* (scalar: Scalar): Vector

    /** Returns the product of this $vector times a $scalar on the left. */
    def *: (scalar: Scalar): Vector

    /** Returns the pointwise product of this $vector times another $vector. */
    def ∘ (that: Vector): Vector
  }
}
