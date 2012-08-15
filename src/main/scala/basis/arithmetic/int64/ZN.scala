/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.arithmetic
package int64

/** An abstract ''N''-dimensional integer module.
  * 
  * @author Chris Sachs
  * 
  * @define space   integer module
  */
trait ZN extends FN[Int64.type] {
  trait Element extends Any with super.Element {
    override protected def Vector: ZN.this.type = ZN.this
    
    override def apply(i: Int): Scalar
    
    override def + (that: Vector): Vector = {
      if (N != that.N) throw new DimensionException
      val coords = new Array[Long](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = this(i).value + that(i).value
        i += 1
      }
      Vector(coords)
    }
    
    override def unary_- : Vector = {
      val coords = new Array[Long](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = -this(i).value
        i += 1
      }
      Vector(coords)
    }
    
    override def - (that: Vector): Vector = {
      if (N != that.N) throw new DimensionException
      val coords = new Array[Long](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = this(i).value - that(i).value
        i += 1
      }
      Vector(coords)
    }
    
    override def :* (scalar: Scalar): Vector = {
      val coords = new Array[Long](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = this(i).value * scalar.value
        i += 1
      }
      Vector(coords)
    }
    
    override def *: (scalar: Scalar): Vector = this :* scalar
    
    override def ⋅ (that: Vector): Scalar = {
      if (N != that.N) throw new DimensionException
      var s = 0L
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
  
  override type Scalar = Int64
  
  override def Scalar: Int64.type = Int64
  
  override def N: Int
  
  override def zero: Vector = apply(new Array[Long](N))
  
  /** Returns a new vector with the given `Long` coordinates. */
  def apply(coords: Array[Long]): Vector
  
  override def apply(coords: Scalar*): Vector = apply(coords.map(_.toLong).toArray[Long])
}

object ZN {
  /** Returns an ''N''-dimensional integer module. */
  def apply(N: Int): ZN = N match {
    case 2 => Z2
    case 3 => Z3
    case _ => new Space(N)
  }
  
  /** An ''N''-dimensional integer module. */
  private final class Space(override val N: Int) extends ZN {
    final class Element(coords: Array[Long]) extends super.Element {
      if (coords.length != Vector.N) throw new DimensionException
      
      override def N: Int = coords.length
      
      override def apply(i: Int): Scalar = coords(i)
    }
    
    override type Vector = Element
    
    override lazy val zero: Vector = super.zero
    
    override def apply(coords: Array[Long]): Vector = new Vector(coords)
    
    override def toString: String = "ZN"+"("+ N + ")"
  }
}
