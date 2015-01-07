//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math
package binary64

/** A 4-dimensional double-precision floating-point vector space.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Real
  */
object R4 extends AffineSpace with F4 with RN {
  override type Point = VectorR4

  override type Vector = VectorR4

  override val Vector: R4.type = R4

  override type Scalar = Real

  override val Scalar: Real.type = Real

  override def dim: Int = 4

  override def origin: Vector = zero

  override val zero: Vector =
    new Vector(0.0, 0.0, 0.0, 0.0)

  override def apply(x: Scalar, y: Scalar, z: Scalar, w: Scalar): Vector =
    new Vector(x, y, z, w)

  override def apply(coords: Array[Double]): Vector = {
    if (coords.length != 4) throw new DimensionException
    new Vector(coords(0), coords(1), coords(2), coords(3))
  }

  override def toString: String = "R4"

  final class VectorR4(
      override val x: Scalar,
      override val y: Scalar,
      override val z: Scalar,
      override val w: Scalar)
    extends PointElement
    with VectorF4
    with VectorRN {

    override def dim: Int = 4

    override def apply(i: Int): Scalar = i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => w
      case _ => throw new IndexOutOfBoundsException(i.toString)
    }

    override def + (that: Vector): Vector =
      new Vector(x + that.x, y + that.y, z + that.z, w + that.w)

    override def unary_- : Vector =
      new Vector(-x, -y, -z, -w)

    override def - (that: Vector): Vector =
      new Vector(x - that.x, y - that.y, z - that.z, w - that.w)

    override def :* (scalar: Scalar): Vector =
      new Vector(x * scalar, y * scalar, z * scalar, w * scalar)

    override def *: (scalar: Scalar): Vector = this :* scalar

    override def ∘ (that: Vector): Vector =
      new Vector(x * that.x, y * that.y, z * that.z, w * that.w)

    override def / (scalar: Scalar): Vector =
      new Vector(x / scalar, y / scalar, z / scalar, w / scalar)

    override def ⋅ (that: Vector): Scalar =
      x * that.x + y * that.y + z * that.z + w * that.w

    override def norm: Scalar =
      (x * x + y * y + z * z + w * w).sqrt

    override def normalized: Vector = this / norm
  }
}
