/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary

import language.implicitConversions

class FloatingPoint(val radix: Int)(val precision: Int) extends RealField {
  final class Element(val significand: Integer, val error: Integer, val exponent: Int) extends super.Element {
    def radix: Int = FloatingPoint.this.radix
    
    override def + (that: Element): Element = {
      if (this == NaN || that == NaN) NaN
      else if (this.exponent > that.exponent) that + this
      else {
        val scale = that.exponent - this.exponent
        val significand = new Integer
        Integer.scale(that.significand, radix, scale, significand)
        Integer.add(this.significand, significand, significand)
        val error = new Integer
        Integer.scale(that.error, radix, scale, error)
        Integer.add(this.error, error, error)
        normalize(significand, error, this.exponent)
      }
    }
    
    override def unary_- : Element = new Element(-significand, error, exponent)
    
    override def - (that: Element): Element = this + -that
    
    override def * (that: Element): Element = {
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
        val significand = min + max
        Integer.shiftRight(significand, 1, significand)
        val error = max - min
        normalize(significand, error, exponent.toInt)
      }
    }
    
    override def inverse: Element = {
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
            val significand = new Integer
            Integer.add(min, max, significand)
            Integer.shiftRight(significand, 1, significand)
            val error = new Integer
            Integer.subtract(max, min, error)
            Integer.add(error, 1L, error)
            normalize(significand, error, exponent.toInt)
          }
        }
      }
    }
    
    override def / (that: Element): Element = this * that.inverse
    
    override def pow(that: Element): Element = sys.error("not implemented")
    
    override def sqrt: Element = {
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
          var minSqrt = lower.sqrt
          var maxSqrt = upper.sqrt
          maxSqrt = Integer.add(maxSqrt, 1L, maxSqrt)
          var sqrt = Integer.add(minSqrt, maxSqrt, minSqrt) // clobber minSqrt
          sqrt = Integer.shiftRight(sqrt, 1, sqrt)
          var error = Integer.subtract(maxSqrt, sqrt, maxSqrt) // clobber maxSqrt
          normalize(sqrt, error, exponent)
        }
      }
    }
    
    override def abs: Element = if (significand.sign > 0) this else -this
    
    override def min(that: Element): Element = if (this <= that) this else that
    
    override def max(that: Element): Element = if (this >= that) this else that
    
    override def < (that: Element): Boolean = compare(this, that) < 0
    
    override def <= (that: Element): Boolean = compare(this, that) <= 0
    
    override def > (that: Element): Boolean = compare(this, that) > 0
    
    override def >= (that: Element): Boolean = compare(this, that) >= 0
    
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
    
    def toDouble: Double = {
      if (this == NaN) Double.NaN
      else {
        val digitLength = significand.length(radix)
        val digitsPerBit = math.log(2) / math.log(radix)
        val scale = digitLength - (63 * digitsPerBit).toInt
        Numeral.mkDouble(significand.scale(radix, -scale).toLong, radix, exponent + scale)
      }
    }
    
    def inexact: Element = {
      val length = significand.length(radix)
      val scale = precision - length + 1
      new Element(significand.scale(radix, scale), Integer.unit, exponent - scale)
    }
    
    def writeString(s: Appendable, radix: Int = 10) {
      if (this == NaN) s.append("NaN")
      else Numeral.writeExponentialNumber(s, radix)(significand, error, this.radix, exponent)
    }
  }
  
  override type Vector = Element
  
  override val zero: Element = new Element(Integer.zero, Integer.zero, 0)
  
  override val unit: Element = new Element(Integer.unit, Integer.zero, 0)
  
  val NaN: Element = new Element(Integer.zero, Integer.unit, Int.MaxValue)
  
  def apply(significand: Integer, error: Integer, exponent: Int): Element =
    new Element(significand, error, exponent)
  
  implicit def apply(value: Int): Element = apply(value.toLong)
  
  implicit def apply(value: Long): Element = new Element(Integer(value), Integer.zero, 0)
  
  implicit def apply(value: Float): Element = apply(value.toDouble)
  
  implicit def apply(value: Double): Element = {
    if (value == 0.0) zero
    else if (java.lang.Double.isNaN(value) || java.lang.Double.isInfinite(value)) NaN
    else {
      val exponent = Numeral.floorLog(math.abs(value), radix) - 18 + 1 // 18 digit significand
      val significand = Numeral.mkLong(value, radix, -exponent)
      val error = Numeral.mkLong(math.ulp(value), radix, -exponent) + 1
      new Element(Integer(significand), Integer(error), exponent)
    }
  }
  
  private[algebra] def normalize(significand: Integer, error: Integer, exponent: Int): Element = {
    val errorDigits = error.length(radix)
    val scale = radix - errorDigits - 1
    if (scale >= 0) new Element(significand, error, exponent)
    else {
      Integer.scale(significand, radix, scale, significand)
      Integer.scale(error, radix, scale, error)
      Integer.add(error, 1L, error)
      new Element(significand, error, exponent - scale)
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
  
  private[algebra] def compare(x: Element, y: Element): Int = {
    val difference = x - y
    if (difference.significand == Integer.zero) 0 else difference.significand.sign
  }
  
  override def toString: String = "FloatingPoint"+"("+ radix +")"+"("+ precision +")"
}
