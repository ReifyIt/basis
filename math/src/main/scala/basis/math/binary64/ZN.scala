//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math
package binary64

/** An abstract 64-bit two's complement integer module.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Integral
  *
  * @define space   module
  */
trait ZN extends FN { ZN =>
  override type Vector <: VectorZN

  override type Scalar = Integer

  override val Scalar: Integer.type

  implicit override def ScalarTag: scala.reflect.ClassTag[Integer] =
    scala.reflect.ClassTag(Predef.classOf[Integer])

  override def zero: Vector = apply(new Array[Long](dim))

  override def apply(coords: Array[Scalar]): Vector = {
    val n = dim
    if (coords.length != n) throw new DimensionException
    val xs = new Array[Long](n)
    var i = 0
    while (i < n) {
      xs(i) = coords(i).value
      i += 1
    }
    apply(xs)
  }

  /** Returns a new vector with the given `Long` coordinates. */
  def apply(coords: Array[Long]): Vector

  trait VectorZN extends Any with VectorFN {
    override def apply(i: Int): Scalar

    override def + (that: Vector): Vector = {
      val n = dim
      if (n != that.dim) throw new DimensionException
      val coords = new Array[Long](n)
      var i = 0
      while (i < n) {
        coords(i) = this(i).value + that(i).value
        i += 1
      }
      ZN(coords)
    }

    override def unary_- : Vector = {
      val n = dim
      val coords = new Array[Long](n)
      var i = 0
      while (i < n) {
        coords(i) = -this(i).value
        i += 1
      }
      ZN(coords)
    }

    override def - (that: Vector): Vector = {
      val n = dim
      if (n != that.dim) throw new DimensionException
      val coords = new Array[Long](n)
      var i = 0
      while (i < n) {
        coords(i) = this(i).value - that(i).value
        i += 1
      }
      ZN(coords)
    }

    override def :* (scalar: Scalar): Vector = {
      val n = dim
      val coords = new Array[Long](n)
      var i = 0
      while (i < n) {
        coords(i) = this(i).value * scalar.value
        i += 1
      }
      ZN(coords)
    }

    override def *: (scalar: Scalar): Vector = this :* scalar

    override def ∘ (that: Vector): Vector = {
      val n = dim
      if (n != that.dim) throw new DimensionException
      val coords = new Array[Long](n)
      var i = 0
      while (i < n) {
        coords(i) = this(i).value * that(i).value
        i += 1
      }
      ZN(coords)
    }

    override def ⋅ (that: Vector): Scalar = {
      val n = dim
      if (n != that.dim) throw new DimensionException
      var s = 0L
      var i = 0
      while (i < n) {
        s += this(i).value * that(i).value
        i += 1
      }
      s
    }

    override def equals(other: Any): Boolean = other match {
      case that: VectorZN =>
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
}
