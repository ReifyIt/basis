//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math
package binary64

/** A closed interval of double-precision floating-point value.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Real
  */
final class RInterval(
    override val lower: Real,
    override val upper: Real)
  extends RInterval.IntervalFieldElement {

  override def contains(value: Real): Boolean =
    lower <= value && value <= upper

  override def + (that: RInterval): RInterval =
    new RInterval(lower + that.lower, upper + that.upper)

  override def unary_- : RInterval =
    new RInterval(-upper, -lower)

  override def - (that: RInterval): RInterval =
    new RInterval(lower - that.upper, upper - that.lower)

  override def * (that: RInterval): RInterval = {
    val ll = lower * that.lower
    val lu = lower * that.upper
    val ul = upper * that.lower
    val uu = upper * that.upper
    new RInterval(
      ll min lu min ul min uu,
      ll max lu max ul max uu)
  }

  override def inverse: RInterval = {
    if (contains(0.0)) throw new ArithmeticException("reciprocal of interval spanning 0")
    new RInterval(1.0 / upper, 1.0 / lower)
  }

  override def / (that: RInterval): RInterval = {
    if (that.contains(0.0)) throw new ArithmeticException("division by interval spanning 0")
    val lowerInverse = 1.0 / that.upper
    val upperInverse = 1.0 / that.lower
    val ll = lower * lowerInverse
    val lu = lower * upperInverse
    val ul = upper * lowerInverse
    val uu = upper * upperInverse
    new RInterval(
      ll min lu min ul min uu,
      ll max lu max ul max uu)
  }
}

/** A closed interval field of double-precision floating-point values.
  * @group Real */
object RInterval extends IntervalField {
  override type Interval = RInterval

  override type Scalar = Real

  override val Scalar: Real.type = Real

  override val zero: RInterval =
    new RInterval(0.0, 0.0)

  override val unit: RInterval =
    new RInterval(1.0, 1.0)

  implicit def coerce(that: ZInterval): RInterval =
    new RInterval(that.lower, that.upper)

  implicit override def degenerate(value: Real): RInterval =
    new RInterval(value, value)

  override def apply(lower: Real, upper: Real): RInterval =
    new RInterval(lower, upper)

  override def toString: String = "RInterval"
}
