/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.math
package binary64

/** A closed interval of 64-bit two's complement integer values.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Integral
  */
final class IntegerInterval(
    override val lower: Integer,
    override val upper: Integer)
  extends IntegerInterval.Value {
  
  override def contains(value: Integer): Boolean =
    lower <= value && value <= upper
  
  override def + (that: IntegerInterval): IntegerInterval =
    new IntegerInterval(lower + that.lower, upper + that.upper)
  
  override def unary_- : IntegerInterval =
    new IntegerInterval(-upper, -lower)
  
  override def - (that: IntegerInterval): IntegerInterval =
    new IntegerInterval(lower - that.upper, upper - that.lower)
  
  override def * (that: IntegerInterval): IntegerInterval = {
    val ll = lower * that.lower
    val lu = lower * that.upper
    val ul = upper * that.lower
    val uu = upper * that.upper
    new IntegerInterval(
      ll min lu min ul min uu,
      ll max lu max ul max uu)
  }
}

/** A closed interval ring of 64-bit two's complement integer values.
  * @group Integral */
object IntegerInterval extends IntervalRing {
  override type Interval = IntegerInterval
  
  override type Scalar = Integer
  
  override val Scalar: Integer.type = Integer
  
  override val zero: IntegerInterval =
    new IntegerInterval(0L, 0L)
  
  override val unit: IntegerInterval =
    new IntegerInterval(1L, 1L)
  
  implicit override def degenerate(value: Integer): IntegerInterval =
    new IntegerInterval(value, value)
  
  override def apply(lower: Integer, upper: Integer): IntegerInterval =
    new IntegerInterval(lower, upper)
  
  override def toString: String = "IntegerInterval"
}
