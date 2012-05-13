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
      import basis.util.MurmurHash._
      var h = -1736520349
      var i = 0
      while (i < N) {
        h = mix(h, this(i).value)
        i += 1
      }
      mash(h)
    }
  }
  
  override type Vector <: Element
  
  override type Scalar = Real
  
  override def Scalar = Real
  
  override def N: Int
  
  override def apply(coords: TraversableOnce[Real]): Vector
  
  def apply(coords: Array[Double]): Vector
  
  override def zero: Vector = apply(new Array[Double](N))
}
