/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary

import language.implicitConversions

/** A field of arbitrary-precision floating point values. Floating point values
  * consist of an `Integer` ''significand'' multipled by the field's `radix`
  * times some ''exponent'', along with a relative error represented as an
  * `Integer` scaled by the same exponent. Floating point fields define a
  * standard `precision` indicating the number of digits (in the radix of the
  * field) to approximate exact values to when performing inexact operations.
  * 
  * The `binary` and `decimal` algebra packages each provide a default `Real`
  * field in base-2 and base-10, respectively, along with vector and matrix
  * spaces over that field.
  * 
  * @note Special thanks to the developers of [[http://jscience.org/ JScience]]
  *       for providing a model arbitrary-precision arithmetic library for the JVM.
  * 
  * @author Chris Sachs
  */
class FloatingPoint(val radix: Int)(val precision: Int) extends RealField {
  final class Element private[algebra]
      (val significand: Integer,
       val error: Integer,
       val exponent: Int)
    extends super.Element {
    
    override def + (that: Value): Value = {
      if (this == NaN || that == NaN) NaN
      else if (this.exponent > that.exponent) that + this
      else {
        val scale = that.exponent - this.exponent
        val significand = Integer.alloc
        Integer.scale(that.significand, radix, scale, significand)
        Integer.add(this.significand, significand, significand)
        val error = Integer.alloc
        Integer.scale(that.error, radix, scale, error)
        Integer.add(this.error, error, error)
        normalize(significand, error, this.exponent)
      }
    }
    
    override def unary_- : Value = apply(-significand, error, exponent)
    
    override def - (that: Value): Value = this + -that
    
    override def * (that: Value): Value = {
      val exponent = this.exponent.toLong + that.exponent.toLong
      if (this == NaN || that == NaN ||
          exponent < Int.MinValue || exponent > Int.MaxValue) NaN
      else {
        val minA = this.significand - this.error
        val maxA = this.significand + this.error
        val minB = that.significand - that.error
        val maxB = that.significand + that.error
        val (min, max) =
          if (minA > -maxA) {
            if (minB > -maxB) (minA * minB, maxA * maxB)
            else              (maxA * minB, minA * maxB)
          }
          else {
            if (minB > -maxB) (minA * maxB, maxA * minB)
            else              (maxA * maxB, minA * minB)
          }
        val significand = Integer.alloc
        Integer.add(min, max, significand)
        Integer.shiftRight(significand, 1, significand)
        val error = max - min
        normalize(significand, error, exponent.toInt)
      }
    }
    
    override def inverse: Value = {
      if (this == zero || this == NaN) NaN
      else if (this.error == Integer.zero) inexact.inverse
      else {
        val lower = this.significand - this.error
        val upper = this.significand + this.error
        if (lower < Integer.zero && upper > Integer.zero) NaN
        else {
          val digits = math.max(lower.length(radix), upper.length(radix))
          val exponent = -this.exponent.toLong - (2L * digits.toLong)
          if (exponent < Int.MinValue || exponent > Int.MaxValue) NaN
          else {
            val min = FloatingPoint.this.inverse(upper, 2 * digits)
            val max = FloatingPoint.this.inverse(lower, 2 * digits)
            val significand = Integer.alloc
            Integer.add(min, max, significand)
            Integer.shiftRight(significand, 1, significand)
            val error = Integer.alloc
            Integer.subtract(max, min, error)
            Integer.add(error, 1L, error)
            normalize(significand, error, exponent.toInt)
          }
        }
      }
    }
    
    override def / (that: Value): Value = this * that.inverse
    
    override def pow(that: Value): Value = sys.error("not implemented")
    
    override def sqrt: Value = {
      if (this == NaN) NaN
      else if (this == zero) zero
      else if (error == Integer.zero) inexact.sqrt
      else {
        val lower = this.significand - this.error
        val upper = this.significand + this.error
        if (lower.sign < 0) NaN
        else {
          val exponent = this.exponent >> 1
          if ((exponent & 1) == 1) { // odd exponent
            Integer.scale(lower, radix, 1, lower)
            Integer.scale(upper, radix, 1, upper)
          }
          val minSqrt = lower.sqrt
          val maxSqrt = upper.sqrt
          Integer.add(maxSqrt, 1L, maxSqrt)
          val sqrt = Integer.add(minSqrt, maxSqrt, minSqrt) // clobbers minSqrt
          Integer.shiftRight(sqrt, 1, sqrt)
          var error = Integer.subtract(maxSqrt, sqrt, maxSqrt) // clobbers maxSqrt
          normalize(sqrt, error, exponent)
        }
      }
    }
    
    override def abs: Value = if (significand.sign > 0) this else -this
    
    override def min(that: Value): Value = if (this <= that) this else that
    
    override def max(that: Value): Value = if (this >= that) this else that
    
    override def < (that: Value): Boolean = compare(this, that) < 0
    
    override def <= (that: Value): Boolean = compare(this, that) <= 0
    
    override def > (that: Value): Boolean = compare(this, that) > 0
    
    override def >= (that: Value): Boolean = compare(this, that) >= 0
    
    override def equals(other: Any): Boolean = other match {
      case that: Element =>
        significand == that.significand && error == that.error && exponent == that.exponent
      case _ => false
    }
    
    override def hashCode: Int = {
      import scala.util.hashing.MurmurHash3._
      finalizeHash(mixLast(mix(mix(-814964470, significand.##), error.##), exponent.##), 3)
    }
    
    override def toString: String = toString(radix)
    
    def toString(radix: Int): String = {
      val s = new java.lang.StringBuilder
      writeString(s, radix)
      s.toString
    }
    
    def toInt: Int = toDouble.toInt
    
    def toLong: Long = toDouble.toLong
    
    def toFloat: Float = toDouble.toFloat
    
    def toDouble: Double = {
      if (this == NaN) Double.NaN
      else {
        val digitLength = significand.length(radix)
        val digitsPerBit = math.log(2) / math.log(radix)
        val scale = digitLength - (63 * digitsPerBit).toInt
        Numeral.mkDouble(significand.scale(radix, -scale).toLong, radix, exponent + scale)
      }
    }
    
    def inexact: Value = {
      val length = significand.length(radix)
      val scale = precision - length + 1
      apply(significand.scale(radix, scale), Integer.unit, exponent - scale)
    }
    
    def exact: Value = {
      val errorDigits = error.length(radix)
      val significand = if (this.significand.sign > 0) Integer(radix / 2) else Integer(-radix / 2)
      Integer.scale(significand, radix, errorDigits - 1, significand)
      Integer.add(this.significand, significand, significand)
      Integer.scale(significand, radix, -errorDigits, significand)
      apply(significand, Integer.zero, exponent + errorDigits)
    }
    
    def writeString(s: Appendable, radix: Int = 10) {
      if (this == NaN) s.append("NaN")
      else if (error == Integer.zero) {
        val writer = new NumeralWriter(s, radix)
        writer.writePositionalNotation(significand, exponent)
      }
      else {
        val x = exact
        val writer = new NumeralWriter(s, radix)
        writer.writeScientificNotation(x.significand, x.exponent)
      }
    }
  }
  
  override type Value = Element
  
  override lazy val zero: Value = apply(Integer.zero, Integer.zero, 0)
  
  override lazy val unit: Value = apply(Integer.unit, Integer.zero, 0)
  
  lazy val NaN: Value = apply(Integer.zero, Integer.unit, Int.MaxValue)
  
  def apply(significand: Integer, error: Integer, exponent: Int): Value =
    new Element(significand, error, exponent)
  
  implicit def apply(value: Int): Value = apply(value.toLong)
  
  implicit def apply(value: Long): Value = apply(Integer(value), Integer.zero, 0)
  
  implicit def apply(value: Float): Value = apply(value.toDouble)
  
  implicit def apply(value: Double): Value = {
    if (value == 0.0) zero
    else if (java.lang.Double.isNaN(value) || java.lang.Double.isInfinite(value)) NaN
    else {
      val sigfigs = if (radix == 2) 52 else (53 * (math.log(2) / math.log(radix))).toInt + 1
      val exponent = Numeral.floorLog(math.abs(value), radix) - sigfigs
      val significand = Numeral.mkLong(value, radix, -exponent)
      val error = Numeral.mkLong(math.ulp(value), radix, -exponent) + 1
      apply(Integer(significand), Integer(error), exponent)
    }
  }
  
  def apply(string: String): Value = {
    val parser = new NumeralReader(string, radix)
    val (significand, error, exponent) = parser.parseFloatingPoint()
    apply(significand, error, exponent)
  }
  
  private[algebra] def normalize(significand: Integer, error: Integer, exponent: Int): Value = {
    val errorDigits = error.length(radix)
    val scale = radix - errorDigits - 1
    if (scale >= 0) apply(significand, error, exponent)
    else {
      Integer.scale(significand, radix, scale, significand)
      Integer.scale(error, radix, scale, error)
      Integer.add(error, 1L, error)
      apply(significand, error, exponent - scale)
    }
  }
  
  private[algebra] def inverse(significand: Integer, digits: Int): Integer = {
    val bitsPerDigit = math.log(radix) / math.log(2)
    val scale = (digits * bitsPerDigit).toInt
    val precision = scale - significand.length + 1
    val reciprocal = significand.inverse(precision)
    Integer.scale(reciprocal, radix, digits, reciprocal)
    Integer.shiftRight(reciprocal, scale + 1, reciprocal)
  }
  
  private[algebra] def compare(x: Value, y: Value): Int = {
    val difference = x - y
    if (difference.significand == Integer.zero) 0 else difference.significand.sign
  }
}
