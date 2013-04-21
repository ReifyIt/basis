/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math
package binary64

/** A closed interval of double-precision floating-point value.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Real
  */
final class RealInterval(
    override val lower: Real,
    override val upper: Real)
  extends RealInterval.Value {
  
  override def contains(value: Real): Boolean =
    lower <= value && value <= upper
  
  override def + (that: RealInterval): RealInterval =
    new RealInterval(lower + that.lower, upper + that.upper)
  
  override def unary_- : RealInterval =
    new RealInterval(-upper, -lower)
  
  override def - (that: RealInterval): RealInterval =
    new RealInterval(lower - that.upper, upper - that.lower)
  
  override def * (that: RealInterval): RealInterval = {
    val ll = lower * that.lower
    val lu = lower * that.upper
    val ul = upper * that.lower
    val uu = upper * that.upper
    new RealInterval(
      ll min lu min ul min uu,
      ll max lu max ul max uu)
  }
  
  override def inverse: RealInterval = {
    if (contains(0.0)) throw new ArithmeticException("reciprocal of interval spanning 0")
    new RealInterval(1.0 / upper, 1.0 / lower)
  }
  
  override def / (that: RealInterval): RealInterval = {
    if (that.contains(0.0)) throw new ArithmeticException("division by interval spanning 0")
    val lowerInverse = 1.0 / that.upper
    val upperInverse = 1.0 / that.lower
    val ll = lower * lowerInverse
    val lu = lower * upperInverse
    val ul = upper * lowerInverse
    val uu = upper * upperInverse
    new RealInterval(
      ll min lu min ul min uu,
      ll max lu max ul max uu)
  }
}

/** A closed interval field of double-precision floating-point values.
  * @group Real */
object RealInterval extends IntervalField {
  override type Interval = RealInterval
  
  override type Scalar = Real
  
  override val Scalar: Real.type = Real
  
  override val zero: RealInterval =
    new RealInterval(0.0, 0.0)
  
  override val unit: RealInterval =
    new RealInterval(1.0, 1.0)
  
  implicit override def degenerate(value: Real): RealInterval =
    new RealInterval(value, value)
  
  override def apply(lower: Real, upper: Real): RealInterval =
    new RealInterval(lower, upper)
  
  override def toString: String = "RealInterval"
}
