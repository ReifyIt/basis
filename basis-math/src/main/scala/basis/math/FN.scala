/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math

/** An abstract ''N''-dimensional vector space over a ring. Vector addition
  * associates and commutes, and scalar multiplication associates, commutes,
  * and distributes over vector addition and scalar addition. Vector addition
  * and scalar multiplication both have an identity element, and every vector
  * has an additive inverse. Every vector space is an affine space over itself.
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
  * // function with a subtype of FN with Singleton. Type elements with
  * // the #Vector and #Scalar type projections of your FN type parameter.
  * def testVectorSpaceOperations[V <: FN[S] with Singleton, S <: Ring with Singleton]
  *     (a: V#Scalar, b: V#Scalar, u: V#Vector, v: V#Vector, w: V#Vector) {
  *   assert(u + v == v + u, "commutativity of vector addition")
  *   assert((u + v) + w == u + (v + w), "associativity of vector addition")
  *   assert((a * b) *: v == a *: (b *: v), "associativity of scalar multiplication with ring multiplication")
  *   assert(a *: (u + v) == (a *: u) + (a *: v), "distributivity of scalar multiplication over vector addition")
  *   assert((a + b) *: v == (a *: v) + (b *: v), "distributivity of scalar multiplication over ring addition")
  * }
  * 
  * // Alternatively, functions can use path-dependent types of a FN parameter.
  * def testVectorSpaceIdentities(V: FN[_])(a: V.Scalar, v: V.Vector) {
  *   import V._
  *   assert(zero + v == v, "existence of additive identity vector")
  *   assert(v + (-v) == zero, "existence of additive inverse vector")
  *   assert(Scalar.unit *: v == v, "existence of multiplicative identity scalar")
  * }
  * }}}
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * @group    VectorSpaces
  * 
  * @define space   vector space
  */
trait FN extends VectorSpace {
  trait Value extends Any with super.Value {
    /** Returns the number of coordinates in this $vector. */
    def dim: Int = FN.this.dim
    
    /** Returns the coordinate at the given index. */
    def apply(i: Int): Scalar
    
    override def + (that: Vector): Vector = {
      val n = dim
      if (n != that.dim) throw new DimensionException
      val coords = new Array[Scalar](n)
      var i = 0
      while (i < n) {
        coords(i) = this(i) + that(i)
        i += 1
      }
      FN.this.apply(coords)
    }
    
    override def unary_- : Vector = {
      val n = dim
      val coords = new Array[Scalar](n)
      var i = 0
      while (i < n) {
        coords(i) = -this(i)
        i += 1
      }
      FN.this.apply(coords)
    }
    
    override def - (that: Vector): Vector = {
      val n = dim
      if (n != that.dim) throw new DimensionException
      val coords = new Array[Scalar](n)
      var i = 0
      while (i < n) {
        coords(i) = this(i) - that(i)
        i += 1
      }
      FN.this.apply(coords)
    }
    
    override def :* (scalar: Scalar): Vector = {
      val n = dim
      val coords = new Array[Scalar](n)
      var i = 0
      while (i < n) {
        coords(i) = this(i) * scalar
        i += 1
      }
      FN.this.apply(coords)
    }
    
    override def *: (scalar: Scalar): Vector = {
      val n = dim
      val coords = new Array[Scalar](n)
      var i = 0
      while (i < n) {
        coords(i) = scalar * this(i)
        i += 1
      }
      FN.this.apply(coords)
    }
    
    override def ∘ (that: Vector): Vector = {
      val n = dim
      if (n != that.dim) throw new DimensionException
      val coords = new Array[Scalar](n)
      var i = 0
      while (i < n) {
        coords(i) = this(i) * that(i)
        i += 1
      }
      FN.this.apply(coords)
    }
    
    /** Returns the dot product of this $vector and another $vector.
      * The name of this method contains the unicode dot operator (U+22C5). */
    def ⋅ (that: Vector): Scalar = {
      val n = dim
      if (n != that.dim) throw new DimensionException
      var s = Scalar.zero
      var i = 0
      while (i < n) {
        s += this(i) * that(i)
        i += 1
      }
      s
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: Value =>
        val n = dim
        var equal = n == that.dim
        var i = 0
        while (i < n && equal) {
          equal = this(i).equals(that(i))
          i += 1
        }
        equal
      case _ => false
    }
    
    override def hashCode: Int = {
      import scala.util.hashing.MurmurHash3._
      val n = dim
      var h = -1736520349
      var i = 0
      while (i < n) {
        h = mix(h, this(i).hashCode)
        i += 1
      }
      finalizeHash(h, n)
    }
    
    override def toString: String = {
      val n = dim
      val s = new java.lang.StringBuilder(FN.this.toString)
      s.append('(')
      if (n > 0) {
        s.append(this(0))
        var i = 1
        while (i < n) {
          s.append(", ").append(this(i))
          i += 1
        }
      }
      s.append(')')
      s.toString
    }
  }
  
  override type Vector <: Value
  
  implicit def ScalarTag: scala.reflect.ClassTag[Scalar]
  
  /** Returns the dimension of this $space. */
  def dim: Int
  
  override def zero: Vector = {
    val n = dim
    val z = Scalar.zero
    val coords = new Array[Scalar](n)
    var i = 0
    while (i < n) {
      coords(i) = z
      i += 1
    }
    apply(coords)
  }
  
  /** Returns a new vector with the given coordinates. */
  def apply(coords: Array[Scalar]): Vector
}
