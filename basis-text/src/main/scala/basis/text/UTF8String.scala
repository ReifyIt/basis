/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

/** An 8-bit Unicode string comprised of a sequence of UTF-8 code units. */
final class UTF8String(val codeUnits: scala.Array[Byte]) extends AnyVal with String {
  override type Self = UTF8String
  
  /** Returns the number of unsigned 8-bit code units in this Unicode string. */
  def size: Int = codeUnits.length
  
  def length: Int = {
    var i = 0
    var k = 0
    val n = codeUnits.length
    while (i < n) {
      i = nextIndex(i)
      k += 1
    }
    k
  }
  
  def apply(index: Int): Char = {
    val n = codeUnits.length
    if (index < 0 || index >= n)
      throw new java.lang.IndexOutOfBoundsException(index.toString)
    new Char({
      val c1 = codeUnits(index) & 0xFF
      if (c1 <= 0x7F) c1 // U+0000..U+007F
      else if (c1 >= 0xC2 && c1 <= 0xF4 && index + 1 < n) {
        val c2 = codeUnits(index + 1) & 0xFF
        if (c1 <= 0xDF && c2 >= 0x80 && c2 <= 0xBF)
          ((c1 & 0x1F) << 6) | (c2 & 0x3F) // U+0080..U+07FF
        else if (index + 2 < n) {
          val c3 = codeUnits(index + 2) & 0xFF
          if ((c1 == 0xE0 &&
               c2 >= 0xA0 && c2 <= 0xBF
            || c1 == 0xED &&
               c2 >= 0x80 && c2 <= 0x9F
            || c1 >= 0xE1 && c1 <= 0xEF &&
               c2 >= 0x80 && c2 <= 0xBF)
            && c3 >= 0x80 && c3 <= 0xBF)
            ((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F) // U+0800..U+FFFF
          else if (index + 3 < n) {
            val c4 = codeUnits(index + 3) & 0xFF
            if ((c1 == 0xF0 &&
                 c2 >= 0x90 && c2 <= 0xBF
              || c1 >= 0xF1 && c1 <= 0xF3 &&
                 c2 >= 0x80 && c2 <= 0xBF
              || c1 == 0xF4 &&
                 c2 >= 0x80 && c2 <= 0x8F)
              && c3 >= 0x80 && c3 <= 0xBF
              && c4 >= 0x80 && c4 <= 0xBF)
              ((c1 & 0x07) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F) // U+10000..U+10FFFF
            else 0xFFFD
          }
          else 0xFFFD
        }
        else 0xFFFD
      }
      else 0xFFFD
    })
  }
  
  def nextIndex(index: Int): Int = {
    val n = codeUnits.length
    if (index < 0 || index >= n)
      throw new java.lang.IndexOutOfBoundsException(index.toString)
    val c1 = codeUnits(index) & 0xFF
    if (c1 <= 0x7F)
      index + 1 // U+0000..U+007F
    else if (c1 >= 0xC2 && c1 <= 0xF4 && index + 1 < n) {
      val c2 = codeUnits(index + 1) & 0xFF
      if (c1 <= 0xDF && c2 >= 0x80 && c2 <= 0xBF)
        index + 2 // U+0080..U+07FF
      else if (index + 2 < n) {
        val c3 = codeUnits(index + 2) & 0xFF
        if ((c1 == 0xE0 &&
             c2 >= 0xA0 && c2 <= 0xBF
          || c1 == 0xED &&
             c2 >= 0x80 && c2 <= 0x9F
          || c1 >= 0xE1 && c1 <= 0xEF &&
             c2 >= 0x80 && c2 <= 0xBF)
          && c3 >= 0x80 && c3 <= 0xBF)
          index + 3 // U+0800..U+FFFF
        else if (index + 3 < n) {
          val c4 = codeUnits(index + 3) & 0xFF
          if ((c1 == 0xF0 &&
               c2 >= 0x90 && c2 <= 0xBF
            || c1 >= 0xF1 && c1 <= 0xF3 &&
               c2 >= 0x80 && c2 <= 0xBF
            || c1 == 0xF4 &&
               c2 >= 0x80 && c2 <= 0x8F)
            && c3 >= 0x80 && c3 <= 0xBF
            && c4 >= 0x80 && c4 <= 0xBF)
            index + 4 // U+10000..U+10FFFF
          else index + 3
        }
        else index + 2
      }
      else index + 1
    }
    else index + 1
  }
  
  /** Returns a copy of this Unicode string. */
  private[text] def copy(size: Int): UTF8String = {
    val newCodeUnits = new scala.Array[Byte](size)
    java.lang.System.arraycopy(codeUnits, 0, newCodeUnits, 0, scala.math.min(codeUnits.length, size))
    new UTF8String(newCodeUnits)
  }
  
  override def iterator: StringIterator = new UTF8Iterator(this, 0)
  
  /** Sequentially applies a function to each code point in this Unicode string.
    * Applies the replacement character U+FFFD in lieu of the maximal subpart of
    * any ill-formed subsequences. */
  override protected def foreach[U](f: Char => U) {
    var i = 0
    val n = size
    while (i < n) {
      f(this(i))
      i = nextIndex(i)
    }
  }
  
  override def toString: java.lang.String = {
    val s = new java.lang.StringBuilder
    var i = 0
    val n = size
    while (i < n) {
      s.appendCodePoint(apply(i).codePoint)
      i = nextIndex(i)
    }
    s.toString
  }
}

/** A factory for 8-bit Unicode strings. */
object UTF8String {
  val empty: UTF8String = new UTF8String(new scala.Array[Byte](0))
  
  def apply(chars: java.lang.CharSequence): UTF8String = {
    val s = new UTF8Buffer
    s.append(chars)
    s.check
  }
}
