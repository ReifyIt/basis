//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math
package binary64

/** A 3-dimensional 64-bit two's complement integer module.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Integral
  */
object Z3 extends AffineSpace with F3 with ZN {
  override type Point = VectorZ3

  override type Vector = VectorZ3

  override val Vector: Z3.type = Z3

  override type Scalar = Integer

  override val Scalar: Integer.type = Integer

  override def dim: Int = 3

  override def origin: Vector = zero

  override val zero: Vector =
    new Vector(0L, 0L, 0L)

  override def apply(x: Scalar, y: Scalar, z: Scalar): Vector =
    new Vector(x, y, z)

  override def apply(coords: Array[Long]): Vector = {
    if (coords.length != 3) throw new DimensionException
    new Vector(coords(0), coords(1), coords(2))
  }

  override def toString: String = "Z3"

  final class VectorZ3(
      override val x: Scalar,
      override val y: Scalar,
      override val z: Scalar)
    extends PointElement
    with VectorF3
    with VectorZN {

    override def dim: Int = 3

    override def apply(i: Int): Scalar = i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => throw new IndexOutOfBoundsException(i.toString)
    }

    override def + (that: Vector): Vector =
      new Vector(x + that.x, y + that.y, z + that.z)

    override def unary_- : Vector =
      new Vector(-x, -y, -z)

    override def - (that: Vector): Vector =
      new Vector(x - that.x, y - that.y, z - that.z)

    override def :* (scalar: Scalar): Vector =
      new Vector(x * scalar, y * scalar, z * scalar)

    override def *: (scalar: Scalar): Vector = this :* scalar

    override def ∘ (that: Vector): Vector =
      new Vector(x * that.x, y * that.y, z * that.z)

    override def ⋅ (that: Vector): Scalar =
      x * that.x + y * that.y + z * that.z

    override def ⨯ (that: Vector): Vector =
      new Vector(y * that.z + z * that.y,
                 z * that.x + x * that.z,
                 x * that.y + y * that.x)
  }
}
