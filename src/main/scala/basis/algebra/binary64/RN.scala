/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

/** An abstract ''N''-dimensional real vector space.
  * 
  * @author Chris Sachs
  * 
  * @define space   real vector space
  */
trait RN extends FN[Real.type] {
  trait Element extends Any with super.Element {
    override protected def Vector: RN.this.type = RN.this
    
    override def apply(i: Int): Real
    
    override def + (that: Vector): Vector = {
      if (N != that.N) throw new DimensionException
      val coords = new Array[Double](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = this(i).value + that(i).value
        i += 1
      }
      Vector(coords)
    }
    
    override def unary_- : Vector = {
      val coords = new Array[Double](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = -this(i).value
        i += 1
      }
      Vector(coords)
    }
    
    override def - (that: Vector): Vector = {
      if (N != that.N) throw new DimensionException
      val coords = new Array[Double](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = this(i).value - that(i).value
        i += 1
      }
      Vector(coords)
    }
    
    override def :* (scalar: Real): Vector = {
      val coords = new Array[Double](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = this(i).value * scalar.value
        i += 1
      }
      Vector(coords)
    }
    
    override def *: (scalar: Real): Vector = this :* scalar
    
    /** Returns the quotient of this $vector divided by a $scalar. */
    def / (scalar: Real): Vector = {
      val coords = new Array[Double](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = this(i).value / scalar.value
        i += 1
      }
      Vector(coords)
    }
    
    override def ⋅ (that: Vector): Real = {
      if (N != that.N) throw new DimensionException
      var s = 0.0
      var i = 0
      while (i < N) {
        s += this(i).value * that(i).value
        i += 1
      }
      s
    }
    
    /** Returns the Euclidean norm of this $vector. */
    def norm: Real = {
      var s = 0.0
      var i = 0
      while (i < N) {
        s += this(i).value * this(i).value
        i += 1
      }
      new Real(s).sqrt
    }
    
    /** Returns a $vector in the same direction as this $vector but scaled to unit length. */
    def normalized: Vector = this / norm
    
    override def equals(other: Any): Boolean = other match {
      case that: Element =>
        var equal = N == that.N
        var i = 0
        while (i < N && equal) {
          equal = this(i).value == that(i).value
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
  }
  
  override type Vector <: Element
  
  override type Scalar = Real
  
  override def Scalar: Real.type = Real
  
  override def N: Int
  
  override def zero: Vector = apply(new Array[Double](N))
  
  /** Returns a new vector with the given `Double` coordinates. */
  def apply(coords: Array[Double]): Vector
  
  override def apply(coords: Real*): Vector = apply(coords.map(_.toDouble).toArray[Double])
  
  /** Returns a real matrix space that maps this $space to another real vector space. */
  def map(that: RN): RMxN[this.type, that.type] = RMxN(this: RN.this.type, that)
  
  override def map(that: FN[Real.type]): FMxN[this.type, that.type, Real.type] = {
    if (that.isInstanceOf[RN]) map(that.asInstanceOf[RN]).asInstanceOf[FMxN[this.type, that.type, Real.type]]
    else super.map(that)
  }
}

object RN {
  /** Returns an ''N''-dimensional real vector space. */
  def apply(N: Int): RN = N match {
    case 2 => R2
    case 3 => R3
    case 4 => R4
    case _ => new Space(N)
  }
  
  /** An ''N''-dimensional real vector space. */
  private final class Space(override val N: Int) extends RN {
    final class Element(coords: Array[Double]) extends super.Element {
      if (coords.length != Vector.N) throw new DimensionException
      
      override def N: Int = coords.length
      
      override def apply(i: Int): Real = coords(i)
    }
    
    override type Vector = Element
    
    override lazy val zero: Vector = super.zero
    
    override def apply(coords: Array[Double]): Vector = new Vector(coords)
    
    override def toString: String = "RN"+"("+ N + ")"
  }
}
