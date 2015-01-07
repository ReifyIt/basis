//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math
package binary64

/** A 2-dimensional double-precision floating-point interval vector space.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Real
  */
object RI2 extends AffineSpace with RIN with F2 {
  override type Point = VectorRI2

  override type Vector = VectorRI2

  override val Vector: RI2.type = RI2

  override type Scalar = RInterval

  override val Scalar: RInterval.type = RInterval

  override def origin: Vector = zero

  override val zero: Vector = super.zero

  implicit def coerce(that: ZI2.Vector): Vector =
    new Vector(that.x, that.y)

  implicit def degenerate(vector: R2.Vector): Vector =
    new Vector(vector.x, vector.y)

  override def apply(x: Scalar, y: Scalar): Vector =
    new Vector(x, y)

  override def toString: String = "RI2"

  final class VectorRI2(
      override val x: Scalar,
      override val y: Scalar)
    extends PointElement
    with VectorRIN
    with VectorF2 {

    def contains(vector: R2.Vector): Boolean =
      x.contains(vector.x) && y.contains(vector.y)
  }
}
