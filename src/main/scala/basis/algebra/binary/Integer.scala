/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary

import language.implicitConversions

final class Integer private[algebra]
    (private[this] var _words: Array[Long],
     private[this] var _size: Int,
     private[this] var _sign: Int)
  extends Integer.Element {
  
  private[algebra] def this() = this(null, 0, 1)
  
  private[algebra] def words: Array[Long] = _words
  
  private[algebra] def words_= (words: Array[Long]): Unit = _words = words
  
  private[algebra] def size: Int = _size
  
  private[algebra] def size_= (size: Int): Unit = _size = size
  
  def sign: Int = _sign
  
  private[algebra] def sign_= (sign: Int): Unit = _sign = sign
  
  override def + (that: Integer): Integer = Integer.add(this, that, new Integer)
  
  override def unary_- : Integer = new Integer(words, size, -sign)
  
  override def - (that: Integer): Integer = Integer.subtract(this, that, new Integer)
  
  override def * (that: Integer): Integer = Integer.multiply(this, that, new Integer)
  
  def inverse(p: Int): Integer = Integer.fixedInverse(this, p)
  
  def /% (that: Integer): (Integer, Integer) = Integer.divide(this, that, new Integer, new Integer)
  
  def / (that: Integer): Integer = (this /% that)._1
  
  def % (that: Integer): Integer = (this /% that)._2
  
  def << (n: Int): Integer = Integer.shiftLeft(this, n, new Integer)
  
  def >> (n: Int): Integer = Integer.shiftRight(this, n, new Integer)
  
  def >>> (n: Int): Integer = Integer.unsignedShiftRight(this, n, new Integer)
  
  def gcd(that: Integer): Integer = Integer.gcd(this, that)
  
  def scale(b: Int, n: Int): Integer = Integer.scale(this, b, n, new Integer)
  
  def sqrt: Integer = Integer.sqrt(this)
  
  override def abs: Integer = if (sign > 0) this else -this
  
  override def min(that: Integer): Integer = if (this <= that) this else that
  
  override def max(that: Integer): Integer = if (this >= that) this else that
  
  override def < (that: Integer): Boolean = Integer.compare(this, that) < 0
  
  override def <= (that: Integer): Boolean = Integer.compare(this, that) <= 0
  
  override def > (that: Integer): Boolean = Integer.compare(this, that) > 0
  
  override def >= (that: Integer): Boolean = Integer.compare(this, that) >= 0
  
  override def equals(other: Any): Boolean = other match {
    case that: Integer =>
      var equal = sign == that.sign && size == that.size
      var i = size - 1
      while (i >= 0 && equal) {
        equal = this(i) == that(i)
        i -= 1
      }
      equal
    case _ => false
  }
  
  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3._
    var h = -672261858
    var i = 0
    while (i < size) {
      h = mix(h, this(i).##)
      i += 1
    }
    finalizeHash(h, size)
  }
  
  override def toString: String = toString(10)
  
  def toString(radix: Int): String = {
    val s = new java.lang.StringBuilder
    writeString(s, radix)
    s.toString
  }
  
  def toInt: Int = toLong.toInt
  
  def toLong: Long = {
    if (size == 1) sign * this(0)
    else sign * ((this(1) << 63) | this(0))
  }
  
  def toFloat: Float = toDouble.toFloat
  
  def toDouble: Double = {
    if (size == 1) (sign * this(0)).toDouble
    else {
      val n = size - 1
      val shift = 63 * n - (63 - java.lang.Long.numberOfLeadingZeros(this(n)))
      Numeral.mkDouble(sign * (this >>> shift)(0), 2, shift)
    }
  }
  
  def toBytes: Array[Byte] = {
    val byteLength = ((length / 8 - 1) + 7) & ~7
    val bytes = new Array[Byte](byteLength)
    var wordIndex = 0
    var bitIndex = 0
    if (sign < 0) {
      var word = this(0) - 1
      var borrow = word >> 63
      word = ~word & Integer.Mask63
      var i = byteLength - 1
      while (i >= 0) {
        if (bitIndex < 63 - 8) {
          bytes(i) = word.toByte
          word >>= 8
        }
        else {
          val bits = word.toByte
          wordIndex += 1
          word = if (wordIndex < size) this(wordIndex) + borrow else borrow
          borrow = word >> 63
          word = ~word & Integer.Mask63
          bitIndex -= 63
          bytes(i) = ((word << -bitIndex) | bits).toByte
          word >>= (8 + bitIndex)
        }
        i -= 1
        bitIndex += 8
      }
    }
    else {
      var word = this(0)
      var i = byteLength - 1
      while (i >= 0) {
        if (bitIndex < 63 - 8) {
          bytes(i) = word.toByte
          word >>= 8
        }
        else {
          val bits = word.toByte
          wordIndex += 1
          word = if (wordIndex < size) this(wordIndex) else 0
          bitIndex -= 63
          bytes(i) = ((word << -bitIndex) | bits).toByte
          word >>= (8 + bitIndex)
        }
        i -= 1
        bitIndex += 8
      }
    }
    bytes
  }
  
  def length: Int = {
    val n = size - 1
    val length2 = 63 * n + 64 - java.lang.Long.numberOfLeadingZeros(this(n))
    if (sign < 0 && isPowerOf2) length2 - 1 else length2
  }
  
  def length(radix: Int): Int = {
    val length2 = length
    if (radix == 2) length2
    else {
      val digitsPerBit = math.log(2) / math.log(radix)
      val minDigits = ((length2 - 1) * digitsPerBit).toInt + 1
      val maxDigits = (length2 * digitsPerBit).toInt + 1
      if (minDigits == maxDigits) minDigits
      else if (Integer.compareAbs(Integer.unit.scale(radix, minDigits), this) > 0) minDigits else minDigits + 1
    }
  }
  
  def lowestSetBit: Int = {
    if (this(0) == 0L) -1
    else {
      var i = 0
      while (this(i) == 0L) i += 1
      var j = 0
      while (((1L << j) & this(i)) == 0L) j += 1
      63 * i + j
    }
  }
  
  def isPowerOf2: Boolean = {
    val n = size - 1
    var i = 0
    while (i < n && this(i) == 0L) i += 1
    i == n && this(i) == java.lang.Long.highestOneBit(this(i))
  }
  
  def writeString(s: Appendable, radix: Int = 10): Unit =
    Numeral.writePositionalNumber(s, radix)(this)
  
  private[algebra] def apply(i: Int): Long = words(i)
  
  private[algebra] def update(i: Int, value: Long): Unit = words(i) = value
  
  private[algebra] def ensureCapacity(capacity: Int) {
    if (words == null || words.length < capacity) {
      var n = capacity - 1
      n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
      n += 1
      val newWords = new Array[Long](n)
      if (words != null) System.arraycopy(words, 0, newWords, 0, size)
      words = newWords
    }
  }
}

object Integer extends OrderedRing {
  override type Vector = Integer
  
  override val zero: Integer = apply(0L)
  
  override val unit: Integer = apply(1L)
  
  implicit def apply(value: Long): Integer = {
    if (value == Long.MinValue) {
      val words = new Array[Long](2)
      val size = 2
      val sign = -1
      words(0) = 0L
      words(1) = 1L
      new Integer(words, size, sign)
    }
    else {
      val words = new Array[Long](1)
      val size = 1
      val sign = 1 | (value >> 63).toInt
      words(0) = sign * value
      new Integer(words, size, sign)
    }
  }
  
  def apply(bytes: Array[Byte]): Integer = {
    val words = new Array[Long](((bytes.length * 8 + 1) / 63) + 1)
    val sign = 1 | (bytes(0) >> 7)
    var wordIndex = 0
    var bitIndex = 0
    var i = bytes.length - 1
    while (i >= 0) {
      val bits = (if (sign < 0) ~bytes(i) else bytes(i)) & Mask8
      if (bitIndex < 63 - 8) words(wordIndex) |= bits << bitIndex
      else {
        words(wordIndex) |= (bits << bitIndex) & Mask63
        bitIndex -= 63
        wordIndex += 1
        words(wordIndex) = bits >> -bitIndex
      }
      i -= 1
      bitIndex += 8
    }
    wordIndex += 1
    while (wordIndex > 1 && words(wordIndex - 1) == 0L) wordIndex -= 1
    val size = wordIndex
    val result = new Integer(words, size, sign)
    if (sign > 0) result else subtract(result, unit, result)
  }
  
  def apply(string: String, radix: Int = 10): Integer = {
    val parser = new NumeralReader(string, radix)
    parser.parsePositionalNumber()
  }
  
  private[algebra] def copy(u: Integer, w: Integer): w.type = {
    w.ensureCapacity(u.size)
    Array.copy(u.words, 0, w.words, 0, u.size)
    w.size = u.size
    w.sign = u.sign
    w
  }
  
  private[algebra] def add(u: Integer, v: Integer, w: Integer): w.type = {
    if (u.sign != v.sign) subtract(u, -v, w)
    else if (v.size > u.size) add(v, u, w)
    else {
      w.ensureCapacity(u.size + 1)
      var sum = 0L
      var i = 0
      while (i < v.size) {
        sum += u(i) + v(i)
        w(i) = sum & Mask63
        sum >>>= 63 // carry
        i += 1
      }
      while (i < u.size && sum != 0L) {
        sum += u(i)
        w(i) = sum & Mask63
        sum >>>= 63
        i += 1
      }
      if (u eq w) i = u.size
      else while (i < u.size) {
        w(i) = u(i)
        i += 1
      }
      if (sum != 0L) {
        w(i) = sum
        i += 1
      }
      w.size = i
      w.sign = u.sign
      w
    }
  }
  
  private[algebra] def add(u: Integer, y: Long, w: Integer): w.type = {
    if (u.sign != (1 | (y >> 63))) subtract(u, -y, w)
    else {
      w.ensureCapacity(u.size + 1)
      var sum = u(0) + y
      w(0) = sum & Mask63
      sum >>>= 63
      var i = 1
      while (i < u.size && sum != 0L) {
        sum += u(i)
        w(i) = sum & Mask63
        sum >>>= 63
        i += 1
      }
      if (u eq w) i = u.size
      else while (i < u.size) {
        w(i) = u(i)
        i += 1
      }
      if (sum != 0L) {
        w(i) = sum
        i += 1
      }
      w.size = i
      w.sign = u.sign
      w
    }
  }
  
  private[algebra] def negate(w: Integer): w.type = {
    w.sign = -w.sign
    w
  }
  
  private[algebra] def subtract(u: Integer, v: Integer, w: Integer): w.type = {
    if (u.sign != v.sign) add(u, -v, w)
    else if (compareAbs(v, u) > 0) negate(subtract(v, u, w))
    else {
      w.ensureCapacity(u.size)
      var diff = 0L
      var i = 0
      while (i < v.size) {
        diff += u(i) - v(i)
        w(i) = diff & Mask63
        diff >>= 63 // borrow
        i += 1
      }
      while (diff != 0L) {
        diff += u(i)
        w(i) = diff & Mask63
        diff >>= 63 // borrow
        i += 1
      }
      if (u eq w) i = u.size
      else while (i < u.size) {
        w(i) = u(i)
        i += 1
      }
      while (i > 1 && w(i - 1) == 0L) i -= 1
      w.size = i
      w.sign = if (w.size == 1 && w(0) == 0L) 1 else u.sign
      w
    }
  }
  
  private[algebra] def subtract(u: Integer, y: Long, w: Integer): w.type = {
    if (u.sign != (1 | (y >> 63))) add(u, -y, w)
    else if (u.size == 1 && math.abs(y) > math.abs(u(0))) {
      w.ensureCapacity(1)
      w(0) = y - u(0)
      w.size = 1
      w.sign = -u.sign
      w
    }
    else {
      w.ensureCapacity(u.size)
      var diff = u(0) - y
      w(0) = diff & Mask63
      diff >>= 63 // borrow
      var i = 1
      while (diff != 0L) {
        diff += u(i)
        w(i) = diff & Mask63
        diff >>= 63 // borrow
        i += 1
      }
      if (u eq w) i = u.size
      else while (i < u.size) {
        w(i) = u(i)
        i += 1
      }
      while (i > 1 && w(i - 1) == 0L) i -= 1
      w.size = i
      w.sign = if (w.size == 1 && w(0) == 0L) 1 else u.sign
      w
    }
  }
  
  private[algebra] def multiply(u: Integer, v: Integer, w: Integer): w.type = {
    if (v.size > u.size) multiply(v, u, w)
    else {
      w.ensureCapacity(u.size + v.size)
      multiplyAccumulate(u, v(0), w, 0, false)
      var i = 1
      while (i < v.size) {
        multiplyAccumulate(u, v(i), w, i, true)
        i += 1
      }
      w.sign = u.sign * v.sign
      w
    }
  }
  
  private[algebra] def multiply(u: Integer, y: Long, w: Integer): w.type = {
    w.ensureCapacity(u.size + 1)
    multiplyAccumulate(u, y, w, 0, false)
    w
  }
  
  private[algebra] def multiplyAccumulate(
      u: Integer, y: Long, w: Integer,
      shift: Int, accumulate: Boolean): w.type = {
    val yl = y & Mask32
    val yh = y >> 32
    var carry = 0L
    var i = 0
    var j = shift
    while (i < u.size) {
      var z = if (accumulate) w(j) + carry else carry
      carry = z >>> 63
      z &= Mask63
      
      val x = u(i)
      val xl = x & Mask32
      val xh = x >>> 32
      
      val zl = xl * yl
      carry += zl >>> 63
      z += zl & Mask63
      carry += z >>> 63
      z &= Mask63
      
      val zh = xl * yh + xh * yl
      carry += zh >>> 31
      z += (zh << 32) & Mask63
      carry += z >>> 63
      z &= Mask63
      
      carry += (xh * yh) << 1
      w(j) = z
      i += 1
      j += 1
    }
    if (carry != 0L) {
      w(j) = carry
      j += 1
    }
    w.size = j
    w
  }
  
  private[algebra] def fixedInverse(u: Integer, p: Int): Integer = {
    if (p <= 30) {
      val dividend = 1L << (p * 2 + 1)
      val divisor = (u >> (u.length - p - 1))(0)
      apply((u.sign * dividend) / divisor)
    }
    else {
      // Newton iteration: x = 2 * x - x^2 * u
      val x = fixedInverse(u, p / 2 + 1) // estimate
      val padded = x << (p - p / 2 - 1)
      val sum = padded + padded
      val truncated = u >> (u.length - p - 2)
      val product = ((x * x) * truncated) >> (2 * (p / 2 + 2))
      sum - product
    }
  }
  
  private[algebra] def divide(u: Integer, v: Integer, q: Integer, r: Integer): (q.type, r.type) = {
    if (u.sign < 0 && v.sign < 0) {
      divide(-u, -v, q, r)
      (q, negate(r))
    }
    else if (v.sign < 0) {
      divide(u, -v, q, r)
      (negate(q), r)
    }
    else if (u.sign < 0) {
      divide(-u, v, q, r)
      (negate(q), negate(r))
    }
    else {
      val n = u.length
      val m = v.length
      val p = n - m + 1
      if (p <= 0) {
        q.ensureCapacity(1)
        q(0) = 0L
        q.size = 1
        q.sign = 1
        (q, copy(u, r))
      }
      else {
        multiply(u, fixedInverse(v, p), q)
        unsignedShiftRight(q, n + 1, q)
        multiply(v, q, r)
        subtract(u, r, r)
        if (r >= v) {
          add(q, unit, q)
          subtract(r, v, r)
        }
        else if (r.sign < 0) {
          subtract(q, unit, q)
          add(r, v, r)
        }
        (q, r)
      }
    }
  }
  
  private[algebra] def divide(u: Integer, y: Int, q: Integer): Long = {
    q.ensureCapacity(u.size)
    var r = 0L
    var i = u.size - 1
    while (i >= 0) {
      val x = u(i)
      
      val xh = (r << 31) | (x >>> 32)
      val qh = xh / y
      r = xh - qh * y
      
      val xl = (r << 32) | (x & Mask32)
      val ql = xl / y
      r = xl - ql * y
      
      q(i) = (qh << 32) | ql
      i -= 1
    }
    q.size = if (u.size > 1 && q(u.size - 1) == 0L) u.size - 1 else u.size
    r
  }
  
  private[algebra] def shiftLeft(u: Integer, n: Int, w: Integer): w.type = {
    if (n < 0) shiftRight(u, -n, w)
    else {
      val wordShift = n / 63
      w.ensureCapacity(u.size + wordShift + 1)
      val leftShift = n - 63 * wordShift
      val rightShift = 63 - leftShift
      var i = u.size - 1
      var j = u.size + wordShift
      var temp = u(i)
      w(j) = temp >>> rightShift
      w.size = if (w(j) != 0L) j + 1 else j
      w.sign = u.sign
      i -= 1
      j -= 1
      while (i >= 0) {
        w(j) = ((temp << leftShift) & Mask63) | (u(i) >>> rightShift)
        temp = u(i)
        i -= 1
        j -= 1
      }
      w(j) = (temp << leftShift) & Mask63
      j -= 1
      while (j >= 0) {
        w(j) = 0L
        j -= 1
      }
      w
    }
  }
  
  private[algebra] def shiftRight(u: Integer, n: Int, w: Integer): w.type = {
    if (u.sign < 0 && n > 0) {
      val wordShift = n / 63
      val bitShift = n % 63
      if (wordShift < u.size) {
        var i = wordShift
        var bitsLost = bitShift != 0 && (u(i) << (64 - bitShift)) != 0L
        i -= 1
        while (i >= 0 && !bitsLost) {
          bitsLost = u(i) != 0L
          i -= 1
        }
        unsignedShiftRight(u, n, w)
        if (bitsLost) subtract(w, unit, w) else w
      }
      else {
        w.ensureCapacity(1)
        w(0) = 1L
        w.size = 1
        w.sign = -1
        w
      }
    }
    else unsignedShiftRight(u, n, w)
  }
  
  private[algebra] def unsignedShiftRight(u: Integer, n: Int, w: Integer): w.type = {
    if (n < 0) shiftLeft(u, -n, w)
    else {
      val wordShift = n / 63
      if (u.size > wordShift) {
        w.ensureCapacity(u.size - wordShift)
        val rightShift = n - 63 * wordShift
        val leftShift = 63 - rightShift
        var i = wordShift
        var j = 0
        var temp = u(i)
        i += 1
        while (i < u.size) {
          w(j) = ((u(i) << leftShift) & Mask63) | (temp >>> rightShift)
          temp = u(i)
          i += 1
          j += 1
        }
        w(j) = temp >>> rightShift
        w.size = if (j == 0 || w(j) != 0L) j + 1 else j
        w.sign = if (w.size == 1 && w(0) == 0L) 1 else u.sign
        w
      }
      else {
        w.ensureCapacity(1)
        w(0) = 0L
        w.size = 1
        w.sign = 1
        w
      }
    }
  }
  
  private[algebra] def gcd(a: Integer, b: Integer): Integer = {
    if (a == zero) b
    else if (b == zero) a
    else {
      var u = copy(a, new Integer)
      var v = copy(b, new Integer)
      u.sign = 1
      v.sign = 1
      while (math.abs(u.size - v.size) > 1 && v != zero) {
        val (q, r) = u /% v
        u = v
        v = r
      }
      val uShift = u.lowestSetBit
      val vShift = v.lowestSetBit
      u >>>= uShift
      while (v != zero) {
        v >>>= v.lowestSetBit
        if (u < v) v -= u
        else {
          u -= v
          val t = u
          u = v
          v = t
        }
        v >>>= 1
      }
      shiftLeft(u, math.min(uShift, vShift), u)
    }
  }
  
  private[algebra] def scale(a: Integer, b: Int, n: Int, w: Integer): w.type = {
    if (b == 2) {
      if (n >= 0) shiftLeft(a, n, w)
      else shiftRight(a, -n, w)
    }
    else if (n >= 0) {
      val k = (math.log(Long.MaxValue) / math.log(b)).toInt // max b's per multiply
      var i = math.min(n, k)
      multiply(a, math.pow(b, i).toLong, w)
      var j = n - i
      while (j > 0) {
        i = math.min(j, k)
        multiply(w, math.pow(b, i).toLong, w)
        j -= i
      }
      w.sign = a.sign
      w
    }
    else {
      val k = (math.log(Int.MaxValue) / math.log(b)).toInt // max b's per divide
      var i = math.min(-n, k)
      divide(a, math.pow(b, i).toInt, w)
      var j = -n - i
      while (j > 0) {
        i = math.min(j, k)
        divide(w, math.pow(b, i).toInt, w)
        j -= i
      }
      w.sign = a.sign
      w
    }
  }
  
  private[algebra] def sqrt(u: Integer): Integer = {
    if (u.sign < 0) throw new ArithmeticException("square root of negative number")
    else if (u == zero || u == unit) u
    else {
      val bitLength = u.length
      val x = u >> ((bitLength >>> 1) + (bitLength & 1)) // estimate
      sqrt(u, x)
    }
  }
  
  private[algebra] def sqrt(u: Integer, x: Integer): Integer = {
    val xn = {
      val n = x + u / x
      shiftRight(n, 1, n)
    }
    if (compareAbs(xn - x, unit) <= 0) {
      if (u / xn < xn) subtract(xn, unit, xn)
      else xn
    }
    else sqrt(u, xn)
  }
  
  private[algebra] def compareAbs(u: Integer, v: Integer): Int = {
    if (u.size > v.size) 1
    else if (u.size < v.size) -1
    else {
      var i = u.size - 1
      while (i > 0 && u(i) == v(i)) i -= 1
      if (u(i) > v(i)) 1
      else if (u(i) < v(i)) -1
      else 0
    }
  }
  
  private[algebra] def compare(u: Integer, v: Integer): Int = {
    if (u.sign != v.sign) u.sign
    else if (u.size > v.size) u.sign
    else if (u.size < v.size) -u.sign
    else {
      var i = u.size - 1
      while (i > 0 && u(i) == v(i)) i -= 1
      if (u(i) > v(i)) u.sign
      else if (u(i) < v(i)) -u.sign
      else 0
    }
  }
  
  @inline private def Mask63 = 0x7FFFFFFFFFFFFFFFL
  @inline private def Mask32 = 0xFFFFFFFFL
  @inline private def Mask8  = 0xFFL
  
  override def toString: String = "Integer"
}
