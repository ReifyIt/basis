/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary

private[algebra] object Numeral {
  /** Return the closest Double value to `a * b^n`. */
  def mkDouble(a: Long, b: Int, n: Int): Double = {
    if (a == 0L) 0.0
    else if (a == Long.MinValue) mkDouble(a / b, b, n + 1)
    else if (a < 0L) -mkDouble(-a, b, n)
    else if (b == 2) {
      val shift = 11 - java.lang.Long.numberOfLeadingZeros(a)
      val mantissa = if (shift > 0) (a >> shift) + ((a >> (shift - 1)) & 1L) else a << -shift
      val exponent = 1023 + 52 + n + shift
      if (mantissa >> 52 != 1L) mkDouble(a >> 1, 2, n + 1)
      else if (exponent <= 54) 0.0 // underflow
      else if (exponent <= 0) mkDouble(a, 2, n + 54) / (1L << 54) // subnormal
      else if (exponent >= 2047) Double.PositiveInfinity // overflow
      else java.lang.Double.longBitsToDouble(
        (mantissa & 0x000FFFFFFFFFFFFFL) | (exponent.toLong << 52))
    }
    else {
      val (mantissa, pow2) = base2(a, b, n)
      mkDouble(mantissa, 2, pow2)
    }
  }
  
  /** Return the closest Long value to `a * b^n`. */
  def mkLong(a: Double, b: Int, n: Int): Long = {
    val bits = java.lang.Double.doubleToLongBits(a)
    val sign = 1 | (bits >> 63).toInt
    val mantissa = bits & 0x000FFFFFFFFFFFFFL
    val biasedExponent = (bits >> 52).toInt & 0x7FF
    if (biasedExponent == 0x7FF) {
      if (mantissa == 0L) throw new ArithmeticException("cannot convert Infinity to Long")
      else throw new ArithmeticException("cannot convert NaN to Long")
    }
    else if (biasedExponent == 0) {
      if (mantissa == 0L) 0L
      else mkLong(a * b, b, n - 1) // subnormal
    }
    else {
      val significand = mantissa | 0x0010000000000000L
      val (m, pow) = base2(significand, b, n)
      val pow2 = pow + (biasedExponent - 1023 - 52)
      if (pow2 > 0) throw new ArithmeticException("overflow")
      else if (pow2 < -63) 0L
      else sign * ((m >> -pow2) + ((m >> -(pow2 + 1)) & 1L))
    }
  }
  
  /** Return the `a * 2^n` closest to `a * b^n`. */
  def base2(a: Long, b: Int, n: Int): (Long, Int) = {
    if (a == 0L) (0L, 0)
    else if (a < 0L) base2(-a, b, n) match {
      case (a, n) => (-a, n)
    }
    else if (b == 2) (a, n)
    else if (n >= 0) {
      val k = (math.log(Int.MaxValue) / math.log(b)).toInt // max b's per multiply
      var powN = n
      var pow2 = 0
      var x3 = a >>> 32
      var x2 = a & Mask32
      var x1 = 0L
      var x0 = 0L
      while (powN > 0) {
        val i = math.min(powN, k)
        val y = math.pow(b, i).toLong
        
        x0 *= y
        x1 *= y
        x2 *= y
        x3 *= y
        
        x1 += x0 >>> 32
        x0 &= Mask32
        x2 += x1 >>> 32
        x1 &= Mask32
        x3 += x2 >>> 32
        x2 &= Mask32
        val carry = x3 >>> 32
        x3 &= Mask32
        
        powN -= i
        if (carry != 0L) {
          x0 = x1
          x1 = x2
          x2 = x3
          x3 = carry
          pow2 += 32
        }
      }
      val shift = java.lang.Long.numberOfLeadingZeros(x3) - 33
      val mantissa =
        if (shift < 0) (x3 << 31) | (x2 >>> 1)
        else (((x3 << 32) | x2) << shift) | (x1 >>> (32 - shift))
      pow2 -= shift
      (mantissa, pow2)
    }
    else {
      val k = (math.log(Int.MaxValue) / math.log(b)).toInt // max b's per divide
      var powN = n
      var pow2 = 0
      var x1 = a
      var x0 = 0L
      while (powN < 0) {
        val i = math.min(-powN, k)
        val y = math.pow(10, i).toLong
        
        var xh = (x1 >>> 32)
        var qh = xh / y
        var r = xh - qh * y
        var xl = (r << 32) | (x1 & Mask32)
        var ql = xl / y
        r = xl - ql * y
        x1 = (qh << 32) | ql
        
        xh = (r << 31) | (x0 >>> 32)
        qh = xh / y
        r = xh - qh * y
        xl = (r << 32) | (x0 & Mask32)
        ql = xl / y
        x0 = (qh << 32) | ql
        
        powN += i
        val shift = java.lang.Long.numberOfLeadingZeros(x1) - 1
        x1 = (x1 << shift) | (x0 >>> (63 - shift))
        x0 = (x0 << shift) & Mask63
        pow2 -= shift
      }
      (x1, pow2)
    }
  }
  
  /** Return the largest power of `base` less than `significand`. */
  def floorLog(significand: Double, base: Int): Int = {
    require(significand > 0.0)
    require(base > 0)
    val bits = java.lang.Double.doubleToLongBits(significand)
    val biasedExponent = (bits >> 52).toInt & 0x7FF
    if (biasedExponent == 0x7FF) throw new ArithmeticException("log of Infinity or NaN")
    val exponent =
      if (biasedExponent != 0) biasedExponent - 1023
      else floorLog(significand * 0x0040000000000000L, base) - 54
    if (base == 2) exponent
    else {
      val guess = (exponent * (math.log(2) / math.log(base))).toInt
      val pow = mkDouble(1, base, guess)
      if (pow <= significand && base * pow > significand) guess
      else if (pow > significand) guess - 1
      else guess + 1
    }
  }
  
  @inline private[this] def Mask63 = 0x7FFFFFFFFFFFFFFFL
  @inline private[this] def Mask32 = 0xFFFFFFFFL
}
