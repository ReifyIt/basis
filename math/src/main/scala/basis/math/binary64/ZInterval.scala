//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math
package binary64

/** A closed interval of 64-bit two's complement integer values.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Integral
  */
final class ZInterval(
    override val lower: Integer,
    override val upper: Integer)
  extends ZInterval.IntervalRingElement {

  override def contains(value: Integer): Boolean =
    lower <= value && value <= upper

  override def + (that: ZInterval): ZInterval =
    new ZInterval(lower + that.lower, upper + that.upper)

  override def unary_- : ZInterval =
    new ZInterval(-upper, -lower)

  override def - (that: ZInterval): ZInterval =
    new ZInterval(lower - that.upper, upper - that.lower)

  override def * (that: ZInterval): ZInterval = {
    val ll = lower * that.lower
    val lu = lower * that.upper
    val ul = upper * that.lower
    val uu = upper * that.upper
    new ZInterval(
      ll min lu min ul min uu,
      ll max lu max ul max uu)
  }
}

/** A closed interval ring of 64-bit two's complement integer values.
  * @group Integral */
object ZInterval extends IntervalRing {
  override type Interval = ZInterval

  override type Scalar = Integer

  override val Scalar: Integer.type = Integer

  override val zero: ZInterval =
    new ZInterval(0L, 0L)

  override val unit: ZInterval =
    new ZInterval(1L, 1L)

  implicit override def degenerate(value: Integer): ZInterval =
    new ZInterval(value, value)

  override def apply(lower: Integer, upper: Integer): ZInterval =
    new ZInterval(lower, upper)

  override def toString: String = "ZInterval"
}
