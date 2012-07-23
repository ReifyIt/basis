/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.compute

import basis.algebra._

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
  * @author Chris Sachs
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
  * @tparam S   The set of scalars.
  * 
  * @define space   vector space
  */
trait FN[S <: Ring with Singleton] extends AffineSpace[S] with VectorSpace[S] {
  /** A vector in this $space.
    * 
    * @define point   $vector
    * @define vector  vector
    * @define scalar  scalar
    */
  trait Element extends Any with super[AffineSpace].Element with super[VectorSpace].Element {
    protected def Vector: FN.this.type = FN.this
    
    /** Returns the number of coordinates. */
    def N: Int = Vector.N
    
    /** Returns the coordinate at the given index. */
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
  
  override def origin: Point = zero
  
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
  
  /** Returns a new vector with the given coordinates. */
  def apply(coords: Scalar*): Vector
  
  /** Returns a matrix space that maps this $space to another vector space. */
  def map(that: FN[S]): FMxN[this.type, that.type, S] = FMxN(Scalar)(this: FN.this.type, that)
}

object FN {
  /** Returns an ''N''-dimensional vector space over the given ring. */
  def apply(Scalar: Ring)(N: Int): FN[Scalar.type] = N match {
    case 2 => F2(Scalar)
    case 3 => F3(Scalar)
    case 4 => F4(Scalar)
    case _ => new Space[Scalar.type](Scalar)(N)
  }
  
  /** A generic ''N''-dimensional vector space over a ring.
    * 
    * @tparam S    The set of scalars.
    */
  private final class Space[S <: Ring with Singleton](override val Scalar: S)(override val N: Int) extends FN[S] {
    final class Element(coords: Array[AnyRef]) extends super.Element {
      if (coords.length != Vector.N) throw new DimensionException
      
      override def N: Int = coords.length
      
      override def apply(i: Int): Scalar = coords(i).asInstanceOf[Scalar]
    }
    
    override type Vector = Element
    
    override lazy val zero: Vector = super.zero
    
    override def apply(coords: Scalar*): Vector = new Vector(coords.asInstanceOf[Seq[AnyRef]].toArray[AnyRef])
    
    override def toString: String = "FN"+"("+ Scalar +")"+"("+ N +")"
  }
}
