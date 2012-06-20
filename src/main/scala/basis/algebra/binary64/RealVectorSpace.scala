/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

trait RealVectorSpace extends VectorSpace[Real.type] {
  trait Element extends Any with super.Element {
    override protected def Vector: RealVectorSpace.this.type = RealVectorSpace.this
    
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
    
    def norm: Real = {
      var s = 0.0
      var i = 0
      while (i < N) {
        s += this(i).value * this(i).value
        i += 1
      }
      new Real(s).sqrt
    }
    
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
  
  override def Scalar = Real
  
  override def N: Int
  
  override def apply(coords: TraversableOnce[Real]): Vector
  
  def apply(coords: Array[Double]): Vector
  
  override def zero: Vector = apply(new Array[Double](N))
  
  override def ⨯ (that: VectorSpace[Real.type]): MatrixSpace[that.type, this.type, Real.type] = {
    if (that.isInstanceOf[RealVectorSpace])
      (this ⨯ that.asInstanceOf[RealVectorSpace]).asInstanceOf[MatrixSpace[that.type, this.type, Real.type]]
    else super.⨯(that)
  }
  
  def ⨯ (that: RealVectorSpace): RealMatrixSpace[that.type, this.type] =
    new RMxN[that.type, this.type](that, this)
}
