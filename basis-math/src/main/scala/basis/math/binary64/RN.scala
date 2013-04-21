/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math
package binary64

/** An abstract double-precision floating-point vector space.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * @group    Real
  */
trait RN extends FN {
  trait Value extends Any with super.Value {
    override def apply(i: Int): Scalar
    
    override def + (that: Vector): Vector = {
      val n = dim
      if (n != that.dim) throw new DimensionException
      val coords = new Array[Double](n)
      var i = 0
      while (i < n) {
        coords(i) = this(i).value + that(i).value
        i += 1
      }
      RN.this.apply(coords)
    }
    
    override def unary_- : Vector = {
      val n = dim
      val coords = new Array[Double](n)
      var i = 0
      while (i < n) {
        coords(i) = -this(i).value
        i += 1
      }
      RN.this.apply(coords)
    }
    
    override def - (that: Vector): Vector = {
      val n = dim
      if (n != that.dim) throw new DimensionException
      val coords = new Array[Double](n)
      var i = 0
      while (i < n) {
        coords(i) = this(i).value - that(i).value
        i += 1
      }
      RN.this.apply(coords)
    }
    
    override def :* (scalar: Real): Vector = {
      val n = dim
      val coords = new Array[Double](n)
      var i = 0
      while (i < n) {
        coords(i) = this(i).value * scalar.value
        i += 1
      }
      RN.this.apply(coords)
    }
    
    override def *: (scalar: Scalar): Vector = this :* scalar
    
    /** Returns the quotient of this $vector divided by a $scalar. */
    def / (scalar: Scalar): Vector = {
      val n = dim
      val coords = new Array[Double](n)
      var i = 0
      while (i < n) {
        coords(i) = this(i).value / scalar.value
        i += 1
      }
      RN.this.apply(coords)
    }
    
    override def ⋅ (that: Vector): Scalar = {
      val n = dim
      if (n != that.dim) throw new DimensionException
      var s = 0.0
      var i = 0
      while (i < n) {
        s += this(i).value * that(i).value
        i += 1
      }
      s
    }
    
    /** Returns the Euclidean norm of this $vector. */
    def norm: Scalar = {
      val n = dim
      var s = 0.0
      var i = 0
      while (i < n) {
        s += this(i).value * this(i).value
        i += 1
      }
      new Real(java.lang.Math.sqrt(s))
    }
    
    /** Returns a unit $vector in the direction of this $vector. */
    def normalized: Vector = this / norm
    
    override def equals(other: Any): Boolean = other match {
      case that: Value =>
        val n = dim
        var equal = n == that.dim
        var i = 0
        while (i < n && equal) {
          equal = this(i).value == that(i).value
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
        h = mix(h, this(i).##)
        i += 1
      }
      finalizeHash(h, n)
    }
  }
  
  override type Vector <: Value
  
  override type Scalar = Real
  
  override val Scalar: Real.type
  
  implicit override def ScalarTag: scala.reflect.ClassTag[Real] =
    scala.reflect.ClassTag(Predef.classOf[Real])
  
  override def zero: Vector = apply(new Array[Double](dim))
  
  override def apply(coords: Array[Scalar]): Vector = {
    val n = dim
    if (coords.length != n) throw new DimensionException
    val xs = new Array[Double](n)
    var i = 0
    while (i < n) {
      xs(i) = coords(i).value
      i += 1
    }
    apply(xs)
  }
  
  /** Returns a new vector with the given `Double` coordinates. */
  def apply(coords: Array[Double]): Vector
}
