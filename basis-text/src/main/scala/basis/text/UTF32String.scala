/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._
import basis.util._

/** A 32-bit Unicode string comprised of a sequence of UTF-32 code units. */
final class UTF32String(val codeUnits: scala.Array[Int]) extends AnyVal with Array[Char] with String {
  override type Self = UTF32String
  
  override def length: Int = codeUnits.length
  
  override def apply(index: Int): Char = {
    val n = codeUnits.length
    if (index < 0 || index >= n)
      throw new java.lang.IndexOutOfBoundsException(index.toString)
    new Char({
      val c = codeUnits(index)
      if (c >= 0x0000 && c <= 0xD7FF ||
          c >= 0xE000 && c <= 0x10FFFF) c // U+0000..U+D7FF | U+E000..U+10FFFF
      else 0xFFFD
    })
  }
  
  /** Returns a copy of this Unicode string. */
  private[text] def copy(size: Int): UTF32String = {
    val newCodeUnits = new scala.Array[Int](size)
    java.lang.System.arraycopy(codeUnits, 0, newCodeUnits, 0, codeUnits.length min size)
    new UTF32String(newCodeUnits)
  }
  
  override def iterator: StringIterator = new UTF32Iterator(this, 0)
  
  /** Sequentially applies a function to each code point in this Unicode string.
    * Applies the replacement character U+FFFD in lieu of invalid characters. */
  override protected def foreach[U](f: Char => U) {
    var i = 0
    val n = length
    while (i < n) {
      f(this(i))
      i += 1
    }
  }
  
  override def toString: java.lang.String = {
    val s = new java.lang.StringBuilder
    var i = 0
    val n = length
    while (i < n) {
      s.appendCodePoint(apply(i).codePoint)
      i += 1
    }
    s.toString
  }
}

/** A factory for 32-bit Unicode strings. */
object UTF32String {
  val empty: UTF32String = new UTF32String(new scala.Array[Int](0))
  
  def apply(chars: java.lang.CharSequence): UTF32String = {
    val s = new UTF32Buffer
    s.append(chars)
    s.check
  }
}
