//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math
package binary64

/** A 3-dimensional double-precision floating-point interval vector space.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Real
  */
object RI3 extends AffineSpace with RIN with F3 {
  override type Point = VectorRI3

  override type Vector = VectorRI3

  override val Vector: RI3.type = RI3

  override type Scalar = RInterval

  override val Scalar: RInterval.type = RInterval

  override def origin: Vector = zero

  override val zero: Vector = super.zero

  implicit def coerce(that: ZI3.Vector): Vector =
    new Vector(that.x, that.y, that.z)

  implicit def degenerate(vector: R3.Vector): Vector =
    new Vector(vector.x, vector.y, vector.z)

  override def apply(x: Scalar, y: Scalar, z: Scalar): Vector =
    new Vector(x, y, z)

  override def toString: String = "RI3"

  final class VectorRI3(
      override val x: Scalar,
      override val y: Scalar,
      override val z: Scalar)
    extends PointElement
    with VectorRIN
    with VectorF3 {

    def contains(vector: R3.Vector): Boolean =
      x.contains(vector.x) && y.contains(vector.y) && z.contains(vector.z)
  }
}
