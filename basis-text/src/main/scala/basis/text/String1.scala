/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

/** An 8-bit Unicode string comprised of a UTF-8 code unit sequence. */
class String1(val codeUnits: scala.Array[Byte]) extends AnyVal with String {
  override type Self = String1
  
  /** Returns the number of unsigned 8-bit code units in this string. */
  def size: Int = codeUnits.length
  
  /** Counts the number of code points in this string. */
  def length: Int = {
    var i = 0
    var l = 0
    val n = size
    while (i < n) {
      i = nextIndex(i)
      l += 1
    }
    l
  }
  
  /** Returns a decoded character beginning at `index`. Substitutes the
    * replacement character U+FFFD at invalid indexes. */
  def apply(index: Int): Char = {
    val n = codeUnits.length
    if (index < 0 || index >= n)
      throw new java.lang.IndexOutOfBoundsException(index.toString)
    new Char({
      val c1 = codeUnits(index) & 0xFF
      if (c1 <= 0x7F) c1 // U+0000..U+007F
      else if (c1 >= 0xC2 && c1 <= 0xF4 && index + 1 < n) {
        val c2 = codeUnits(index + 1) & 0xFF
        if (c1 <= 0xDF && c2 >= 0x80 && c2 <= 0xBF) // U+0080..U+07FF
          ((c1 & 0x1F) << 6) | (c2 & 0x3F)
        else if (index + 2 < n) {
          val c3 = codeUnits(index + 2) & 0xFF
          if ((c1 == 0xE0 &&
               c2 >= 0xA0 && c2 <= 0xBF
            || c1 == 0xED &&
               c2 >= 0x80 && c2 <= 0x9F
            || c1 >= 0xE1 && c1 <= 0xEF &&
               c2 >= 0x80 && c2 <= 0xBF)
            && c3 >= 0x80 && c3 <= 0xBF) // U+0800..U+FFFF
            ((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F)
          else if (index + 3 < n) {
            val c4 = codeUnits(index + 3) & 0xFF
            if ((c1 == 0xF0 &&
                 c2 >= 0x90 && c2 <= 0xBF
              || c1 >= 0xF1 && c1 <= 0xF3 &&
                 c2 >= 0x80 && c2 <= 0xBF
              || c1 == 0xF4 &&
                 c2 >= 0x80 && c2 <= 0x8F)
              && c3 >= 0x80 && c3 <= 0xBF
              && c4 >= 0x80 && c4 <= 0xBF) // U+10000..U+10FFFF
              ((c1 & 0x07) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F)
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
    if (c1 <= 0x7F) // U+0000..U+007F
      index + 1
    else if (c1 >= 0xC2 && c1 <= 0xF4 && index + 1 < n) {
      val c2 = codeUnits(index + 1) & 0xFF
      if (c1 <= 0xDF && c2 >= 0x80 && c2 <= 0xBF) // U+0080..U+07FF
        index + 2
      else if (index + 2 < n) {
        val c3 = codeUnits(index + 2) & 0xFF
        if ((c1 == 0xE0 &&
             c2 >= 0xA0 && c2 <= 0xBF
          || c1 == 0xED &&
             c2 >= 0x80 && c2 <= 0x9F
          || c1 >= 0xE1 && c1 <= 0xEF &&
             c2 >= 0x80 && c2 <= 0xBF)
          && c3 >= 0x80 && c3 <= 0xBF) // U+0800..U+FFFF
          index + 3
        else if (index + 3 < n) {
          val c4 = codeUnits(index + 3) & 0xFF
          if ((c1 == 0xF0 &&
               c2 >= 0x90 && c2 <= 0xBF
            || c1 >= 0xF1 && c1 <= 0xF3 &&
               c2 >= 0x80 && c2 <= 0xBF
            || c1 == 0xF4 &&
               c2 >= 0x80 && c2 <= 0x8F)
            && c3 >= 0x80 && c3 <= 0xBF
            && c4 >= 0x80 && c4 <= 0xBF) // U+10000..U+10FFFF
            index + 4
          else index + 3
        }
        else index + 2
      }
      else index + 1
    }
    else index + 1
  }
  
  /** Returns a resized copy of this string. */
  private[text] def copy(size: Int): String1 = {
    val newCodeUnits = new scala.Array[Byte](size)
    java.lang.System.arraycopy(codeUnits, 0, newCodeUnits, 0, scala.math.min(codeUnits.length, size))
    new String1(newCodeUnits)
  }
  
  override def iterator: CharIterator = new String1Iterator(this, 0)
  
  /** Sequentially applies a function to each code point in this string.
    * Applies the replacement character U+FFFD in lieu of the maximal subpart
    * of any ill-formed subsequences. */
  override protected def foreach[U](f: Char => U) {
    var i = 0
    var n = codeUnits.length
    while (i < n) f(new Char({
      val c1 = codeUnits(i) & 0xFF
      i += 1
      if (c1 <= 0x7F) c1 // U+0000..U+007F
      else if (c1 >= 0xC2 && c1 <= 0xF4 && i < n) {
        val c2 = codeUnits(i) & 0xFF
        if (c1 <= 0xDF && c2 >= 0x80 && c2 <= 0xBF) { // U+0080..U+07FF
          i += 1
          ((c1 & 0x1F) << 6) | (c2 & 0x3F)
        }
        else if (i < n) {
          i += 1
          val c3 = codeUnits(i) & 0xFF
          if ((c1 == 0xE0 &&
               c2 >= 0xA0 && c2 <= 0xBF
            || c1 == 0xED &&
               c2 >= 0x80 && c2 <= 0x9F
            || c1 >= 0xE1 && c1 <= 0xEF &&
               c2 >= 0x80 && c2 <= 0xBF)
            && c3 >= 0x80 && c3 <= 0xBF) { // U+0800..U+FFFF
            i += 1
            ((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F)
          }
          else if (i < n) {
            i += 1
            val c4 = codeUnits(i) & 0xFF
            if ((c1 == 0xF0 &&
                 c2 >= 0x90 && c2 <= 0xBF
              || c1 >= 0xF1 && c1 <= 0xF3 &&
                 c2 >= 0x80 && c2 <= 0xBF
              || c1 == 0xF4 &&
                 c2 >= 0x80 && c2 <= 0x8F)
              && c3 >= 0x80 && c3 <= 0xBF
              && c4 >= 0x80 && c4 <= 0xBF) { // U+10000..U+10FFFF
              i += 1
              ((c1 & 0x07) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F)
            }
            else 0xFFFD
          }
          else 0xFFFD
        }
        else 0xFFFD
      }
      else 0xFFFD
    }))
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
object String1 {
  val empty: String1 = new String1(new scala.Array[Byte](0))
  
  def apply(chars: java.lang.CharSequence): String1 = {
    val s = new String1Buffer
    s.append(chars)
    s.check
  }
}

private[text] final class String1Iterator
    (string: String1, private[this] var index: Int)
  extends CharIterator {
  
  override def isEmpty: Boolean = index >= string.size
  
  override def head: Char = {
    if (isEmpty) throw new scala.NoSuchElementException("head of empty iterator")
    else string(index)
  }
  
  override def step() {
    if (isEmpty) throw new java.lang.UnsupportedOperationException("empty iterator step")
    else index = string.nextIndex(index)
  }
  
  override def dup: String1Iterator = new String1Iterator(string, index)
}
