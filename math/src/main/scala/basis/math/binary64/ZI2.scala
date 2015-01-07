//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math
package binary64

/** A 2-dimensional 64-bit two's complement integer interval module.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Integral
  */
object ZI2 extends AffineSpace with ZIN with F2 {
  override type Point = VectorZI2

  override type Vector = VectorZI2

  override val Vector: ZI2.type = ZI2

  override type Scalar = ZInterval

  override val Scalar: ZInterval.type = ZInterval

  override def origin: Vector = zero

  override val zero: Vector = super.zero

  implicit def degenerate(vector: Z2.Vector): Vector =
    new Vector(vector.x, vector.y)

  override def apply(x: Scalar, y: Scalar): Vector =
    new Vector(x, y)

  override def toString: String = "ZI2"

  final class VectorZI2(
      override val x: Scalar,
      override val y: Scalar)
    extends PointElement
    with VectorZIN
    with VectorF2 {

    def contains(vector: Z2.Vector): Boolean =
      x.contains(vector.x) && y.contains(vector.y)
  }
}
