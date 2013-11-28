//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.math

/** A field of closed sets between two elements of an ordered field.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Structures
  *
  * @define space   interval field
  */
trait IntervalField extends IntervalRing with Field { IntervalField =>
  /** The type of elements in this $space; equivalent to the type of intervals. */
  override type Element = Interval

  override type Interval <: IntervalFieldElement

  override val Scalar: OrderedField

  override def zero: Interval = degenerate(Scalar.zero)

  override def unit: Interval = degenerate(Scalar.unit)

  implicit override def degenerate(value: Scalar): Interval = apply(value, value)

  override def apply(lower: Scalar, upper: Scalar): Interval

  override def unapply(interval: Interval): Option[(Scalar, Scalar)] =
    Some((interval.lower, interval.upper))

  /** A closed set of elements between two points of an ordered field.
    *
    * @define element   $interval
    * @define interval  interval
    */
  trait IntervalFieldElement extends Any with IntervalRingElement with FieldElement {
    override def lower: Scalar

    override def upper: Scalar

    override def contains(value: Scalar): Boolean =
      lower <= value && value <= upper

    override def + (that: Interval): Interval =
      IntervalField(lower + that.lower, upper + that.upper)

    override def unary_- : Interval =
      IntervalField(-upper, -lower)

    override def - (that: Interval): Interval =
      IntervalField(lower - that.upper, upper - that.lower)

    override def * (that: Interval): Interval = {
      val ll = lower * that.lower
      val lu = lower * that.upper
      val ul = upper * that.lower
      val uu = upper * that.upper
      IntervalField(
        ll min lu min ul min uu,
        ll max lu max ul max uu)
    }

    override def inverse: Interval = {
      if (contains(Scalar.zero)) throw new ArithmeticException("reciprocal of interval spanning 0")
      val unit = Scalar.unit
      IntervalField(unit / upper, unit / lower)
    }

    override def / (that: Interval): Interval = {
      if (that.contains(Scalar.zero)) throw new ArithmeticException("division by interval spanning 0")
      val unit = Scalar.unit
      val lowerInverse = unit / that.upper
      val upperInverse = unit / that.lower
      val ll = lower * lowerInverse
      val lu = lower * upperInverse
      val ul = upper * lowerInverse
      val uu = upper * upperInverse
      IntervalField(
        ll min lu min ul min uu,
        ll max lu max ul max uu)
    }
  }
}
