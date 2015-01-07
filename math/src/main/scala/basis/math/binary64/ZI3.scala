//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math
package binary64

/** A 3-dimensional 64-bit two's complement integer interval module.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Integral
  */
object ZI3 extends AffineSpace with ZIN with F3 {
  override type Point = VectorZI3

  override type Vector = VectorZI3

  override val Vector: ZI3.type = ZI3

  override type Scalar = ZInterval

  override val Scalar: ZInterval.type = ZInterval

  override def origin: Vector = zero

  override val zero: Vector = super.zero

  implicit def degenerate(vector: Z3.Vector): Vector =
    new Vector(vector.x, vector.y, vector.z)

  override def apply(x: Scalar, y: Scalar, z: Scalar): Vector =
    new Vector(x, y, z)

  override def toString: String = "ZI3"

  final class VectorZI3(
      override val x: Scalar,
      override val y: Scalar,
      override val z: Scalar)
    extends PointElement
    with VectorZIN
    with VectorF3 {

    def contains(vector: Z3.Vector): Boolean =
      x.contains(vector.x) && y.contains(vector.y) && z.contains(vector.z)
  }
}
