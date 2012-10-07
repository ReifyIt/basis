/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._
import basis.util._

/** A 16-bit Unicode string comprised of a sequence of UTF-16 code units. */
final class UTF16String(val codeUnits: scala.Array[scala.Char]) extends AnyVal with String {
  override type Self = UTF16String
  
  /** Returns the number of unsigned 16-bit code units in this Unicode string. */
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
    if (c1 <= 0xD7FF || c1 >= 0xE000)
      index + 1 // U+0000..U+D7FF | U+E000..U+FFFF
    else if (c1 <= 0xDBFF && index + 1 < n) { // c1 >= 0xD800
      val c2 = codeUnits(index + 1)
      if (c2 >= 0xDC00 && c2 <= 0xDFFF)
        index + 2 // U+10000..U+10FFFF
      else index + 1
    }
    else index + 1
  }
  
  /** Returns a copy of this Unicode string. */
  private[text] def copy(size: Int): UTF16String = {
    val newCodeUnits = new scala.Array[scala.Char](size)
    java.lang.System.arraycopy(codeUnits, 0, newCodeUnits, 0, codeUnits.length min size)
    new UTF16String(newCodeUnits)
  }
  
  override def iterator: StringIterator = new UTF16Iterator(this)
  
  /** Sequentially applies a function to each code point in this Unicode string.
    * Applies the replacement character U+FFFD in lieu of unpaired surrogates. */
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

/** A factory for 16-bit Unicode strings. */
object UTF16String {
  val empty: UTF16String = new UTF16String(new scala.Array[scala.Char](0))
  
  def apply(chars: java.lang.CharSequence): UTF16String = {
    val s = new UTF16Buffer
    s.append(chars)
    s.check
  }
}
