/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

/** A 16-bit Unicode string comprised of a UTF-16 code unit sequence. */
class String2(val codeUnits: scala.Array[scala.Char]) extends AnyVal with String {
  override type Self = String2
  
  /** Returns the number of unsigned 16-bit code units in this string. */
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
      val c1 = codeUnits(index)
      if (c1 <= 0xD7FF || c1 >= 0xE000) c1 // U+0000..U+D7FF | U+E000..U+FFFF
      else if (c1 <= 0xDBFF && index + 1 < n) { // c1 >= 0xD800
        val c2 = codeUnits(index + 1)
        if (c2 >= 0xDC00 && c2 <= 0xDFFF) // U+10000..U+10FFFF
          (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
        else 0xFFFD
      }
      else 0xFFFD
    })
  }
  
  def nextIndex(index: Int): Int = {
    val n = codeUnits.length
    if (index < 0 || index >= n)
      throw new java.lang.IndexOutOfBoundsException(index.toString)
    val c1 = codeUnits(index)
    if (c1 <= 0xD7FF || c1 >= 0xE000) // U+0000..U+D7FF | U+E000..U+FFFF
      index + 1
    else if (c1 <= 0xDBFF && index + 1 < n) { // c1 >= 0xD800
      val c2 = codeUnits(index + 1)
      if (c2 >= 0xDC00 && c2 <= 0xDFFF) // U+10000..U+10FFFF
        index + 2
      else index + 1
    }
    else index + 1
  }
  
  /** Returns a resized copy of this string. */
  private[text] def copy(size: Int): String2 = {
    val newCodeUnits = new scala.Array[scala.Char](size)
    java.lang.System.arraycopy(codeUnits, 0, newCodeUnits, 0, scala.math.min(codeUnits.length, size))
    new String2(newCodeUnits)
  }
  
  override def iterator: CharIterator = new String2Iterator(this, 0)
  
  /** Sequentially applies a function to each code point in this string.
    * Applies the replacement character U+FFFD in lieu of unpaired surrogates. */
  override protected def foreach[U](f: Char => U) {
    var i = 0
    val n = codeUnits.length
    while (i < n) f(new Char({
      val c1 = codeUnits(i).toInt
      i += 1
      if (c1 <= 0xD7FF || c1 >= 0xE000) c1 // U+0000..U+D7FF | U+E000..U+FFFF
      else if (c1 <= 0xDBFF && i < n) { // c1 >= 0xD800
        val c2 = codeUnits(i).toInt
        if (c2 >= 0xDC00 && c2 <= 0xDFFF) { // U+10000..U+10FFFF
          i += 1
          (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
        }
        else 0xFFFD
      }
      else 0xFFFD
    }))
  }
  
  override def toString: java.lang.String = {
    val s = new java.lang.StringBuilder
    val iter = iterator
    while (iter.hasNext) s.appendCodePoint(iter.next().codePoint)
    s.toString
  }
}

/** A factory for 16-bit Unicode strings. */
object String2 {
  val empty: String2 = new String2(new scala.Array[scala.Char](0))
  
  def apply(chars: java.lang.CharSequence): String2 = {
    val s = new String2Buffer
    s.append(chars)
    s.check
  }
}
