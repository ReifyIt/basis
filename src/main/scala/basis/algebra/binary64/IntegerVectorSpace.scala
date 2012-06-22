/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary64

/** An abstract ''N''-dimensional coordinate space over the binary64 `Integer` field.
  * 
  * @author Chris Sachs
  * 
  * @define Structure   `IntegerVectorSpace`
  * @define vector      integer vector
  * @define scalar      integer scalar
  */
trait IntegerVectorSpace extends VectorSpace[Integer.type] {
  /** A vector element of this $Structure. */
  trait Element extends Any with super.Element {
    override protected def Vector: IntegerVectorSpace.this.type = IntegerVectorSpace.this
    
    override def apply(i: Int): Integer
    
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
    
    override def :* (scalar: Integer): Vector = {
      val coords = new Array[Long](N)
      var i = 0
      while (i < coords.length) {
        coords(i) = this(i).value * scalar.value
        i += 1
      }
      Vector(coords)
    }
    
    override def *: (scalar: Integer): Vector = this :* scalar
    
    override def ⋅ (that: Vector): Integer = {
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
  
  override type Scalar = Integer
  
  override def Scalar = Integer
  
  override def N: Int
  
  override def apply(coords: Integer*): Vector = apply(coords.map(_.toLong).toArray[Long])
  
  /** Returns a new $vector with the given `Long` coordinates. */
  def apply(coords: Array[Long]): Vector
  
  override def zero: Vector = apply(new Array[Long](N))
}
