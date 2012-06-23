/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra

/** An abstract space of ''N''-dimensional vectors over a ring. Vector addition
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
  * @author Chris Sachs
  * 
  * @example {{{
  * // You can abstract over vector spaces by parameterizing a class or
  * // function with a subtype of VectorSpace with Singleton. Type elements
  * // with the #Vector and #Scalar type projections of your VectorSpace
  * // type parameter.
  * def testVectorSpaceOperations[V <: VectorSpace[S] with Singleton, S <: Ring with Singleton]
  *     (a: V#Scalar, b: V#Scalar, u: V#Vector, v: V#Vector, w: V#Vector) {
  *   assert(u + v == v + u, "commutativity of vector addition")
  *   assert((u + v) + w == u + (v + w), "associativity of vector addition")
  *   assert((a * b) *: v == a *: (b *: v), "associativity of scalar multiplication with ring multiplication")
  *   assert(a *: (u + v) == (a *: u) + (a *: v), "distributivity of scalar multiplication over vector addition")
  *   assert((a + b) *: v == (a *: v) + (b *: v), "distributivity of scalar multiplication over ring addition")
  * }
  * 
  * // Alternatively, functions can use path-dependent types of a VectorSpace parameter.
  * def testVectorSpaceIdentities(V: VectorSpace[_])(a: V.Scalar, v: V.Vector) {
  *   import V._
  *   assert(zero + v == v, "existence of additive identity vector")
  *   assert(v + (-v) == zero, "existence of additive inverse vector")
  *   assert(Scalar.unit *: v == v, "existence of multiplicative identity scalar")
  * }
  * }}}
  * 
  * @tparam S   The scalar set of this $space.
  * 
  * @define space   vector space
  */
trait VectorSpace[S <: Ring with Singleton] extends AffineSpace[S] with LinearSpace[S] {
  /** A vector in this $space.
    * 
    * @define point   $vector
    * @define vector  vector
    * @define scalar  scalar
    */
  trait Element extends Any with super[AffineSpace].Element with super[LinearSpace].Element {
    protected def Vector: VectorSpace.this.type = VectorSpace.this
    
    /** Returns the number of coordinates in this $vector. */
    def N: Int = Vector.N
    
    /** Returns the coordinate of this $vector at the given index. */
    def apply(i: Int): Scalar
    
    override def + (that: Vector): Vector = {
      if (N != that.N) throw new DimensionException
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = (this(i) + that(i)).asInstanceOf[AnyRef]
        i += 1
      }
      Vector(wrapRefArray(coords).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    override def unary_- : Vector = {
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = (-this(i)).asInstanceOf[AnyRef]
        i += 1
      }
      Vector(wrapRefArray(coords).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    override def - (that: Vector): Vector = {
      if (N != that.N) throw new DimensionException
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = (this(i) - that(i)).asInstanceOf[AnyRef]
        i += 1
      }
      Vector(wrapRefArray(coords).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    override def :* (scalar: Scalar): Vector = {
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = (this(i) * scalar).asInstanceOf[AnyRef]
        i += 1
      }
      Vector(wrapRefArray(coords).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    override def *: (scalar: Scalar): Vector = {
      val coords = new Array[AnyRef](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = (scalar * this(i)).asInstanceOf[AnyRef]
        i += 1
      }
      Vector(wrapRefArray(coords).asInstanceOf[Seq[Scalar]]: _*)
    }
    
    /** Returns the dot product of this $vector and another $vector.
      * The name of this method contains the unicode dot operator (U+22C5). */
    def ⋅ (that: Vector): Scalar = {
      if (N != that.N) throw new DimensionException
      var s = Scalar.zero
      var i = 0
      while (i < N) {
        s += this(i) * that(i)
        i += 1
      }
      s
    }
    
    override def equals(other: Any): Boolean = other match {
      case that: Element =>
        var equal = N == that.N
        var i = 0
        while (i < N && equal) {
          equal = this(i).equals(that(i))
          i += 1
        }
        equal
      case _ => false
    }
    
    override def hashCode: Int = {
      import scala.util.hashing.MurmurHash3._
      var h = -1736520349
      var i = 0
      while (i < N) {
        h = mix(h, this(i).##)
        i += 1
      }
      finalizeHash(h, N)
    }
    
    override def toString: String = {
      val s = new StringBuilder(Vector.toString)
      s.append('(')
      if (N > 0) {
        s.append(this(0))
        var i = 1
        while (i < N) {
          s.append(", ").append(this(i))
          i += 1
        }
      }
      s.append(')')
      s.toString
    }
  }
  
  /** The type of points in this $space; equivalent to the type of vectors. */
  override type Point = Vector
  
  override type Vector <: Element
  
  /** Returns the dimension of this $space. */
  def N: Int
  
  /** Returns a new vector with the given coordinates. */
  def apply(coords: Scalar*): Vector
  
  override def zero: Vector = {
    val z = Scalar.zero.asInstanceOf[AnyRef]
    val coords = new Array[AnyRef](N)
    var i = 0
    while (i < coords.length) {
      coords(i) = z
      i += 1
    }
    apply(wrapRefArray(coords).asInstanceOf[Seq[Scalar]]: _*)
  }
  
  override def origin: Point = zero
  
  /** Returns a matrix space that maps another vector space to this $space. */
  def ⨯ (that: VectorSpace[S]): MatrixSpace[that.type, this.type, S] =
    new generic.FMxN[that.type, this.type, S](Scalar)(that, this)
}
