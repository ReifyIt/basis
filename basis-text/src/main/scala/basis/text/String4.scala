/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

/** A 32-bit Unicode string comprised of a UTF-32 code unit sequence. */
class String4(val codeUnits: scala.Array[Int]) extends AnyVal with Array[Char] with String {
  override type Self = String4
  
  /** Returns the number of code points in this string. */
  override def length: Int = codeUnits.length
  
  /** Returns the character at `index`. Substitutes the replacement character
    * U+FFFD in lieu of invalid characters. */
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
  
  /** Returns a resized copy of this string. */
  private[text] def copy(size: Int): String4 = {
    val newCodeUnits = new scala.Array[Int](size)
    java.lang.System.arraycopy(codeUnits, 0, newCodeUnits, 0, scala.math.min(codeUnits.length, size))
    new String4(newCodeUnits)
  }
  
  override def iterator: CharIterator = new String4Iterator(this, 0)
  
  /** Sequentially applies a function to each code point in this string.
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
object String4 {
  val empty: String4 = new String4(new scala.Array[Int](0))
  
  def apply(chars: java.lang.CharSequence): String4 = {
    val s = new String4Buffer
    s.append(chars)
    s.check
  }
}
