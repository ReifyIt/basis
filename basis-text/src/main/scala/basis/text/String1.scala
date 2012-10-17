/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

/** An 8-bit Unicode string comprised of a UTF-8 code unit sequence.
  * 
  * @author Chris Sachs
  * 
  * @define collection  string
  */
class String1(val codeUnits: scala.Array[Byte]) extends AnyVal with Rope {
  override type Self = String1
  
  override def isEmpty: Boolean = codeUnits.length == 0
  
  /** Counts the number of code points in this string. */
  override def length: Int = {
    var i = 0
    var l = 0
    val n = size
    while (i < n) {
      i = nextIndex(i)
      l += 1
    }
    l
  }
  
  /** Returns the number of unsigned 8-bit code units in this string. */
  def size: Int = codeUnits.length
  
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
    java.lang.System.arraycopy(codeUnits, 0, newCodeUnits, 0, codeUnits.length min size)
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
  
  override def toString: String = {
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
    s.state
  }
  
  implicit def Buffer: String1Buffer = new String1Buffer
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

/** A buffer for 8-bit Unicode strings in the UTF-8 encoding form.
  * Produces only well-formed code unit sequences. */
final class String1Buffer extends Buffer[Any, Char] with CharBuffer {
  override type State = String1
  
  private[this] var string: String1 = String1.empty
  
  private[this] var aliased: Boolean = true
  
  private[this] var size: Int = 0
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def prepare(size: Int) {
    if (aliased || size > string.size) {
      string = string.copy(expand(16, size))
      aliased = false
    }
  }
  
  override def += (char: Char): this.type = {
    val c = char.codePoint
    val n = size
    if (c >= 0x0000 && c <= 0x007F) { // U+0000..U+007F
      prepare(n + 1)
      string.codeUnits(n) = c.toByte
      size = n + 1
    }
    else if (c >= 0x0080 && c <= 0x07FF) { // U+0080..U+07FF
      prepare(n + 2)
      string.codeUnits(n)     = (0xC0 | (c >>> 6)).toByte
      string.codeUnits(n + 1) = (0x80 | (c & 0x3F)).toByte
      size = n + 2
    }
    else if (c >= 0x0800 && c <= 0xFFFF || // U+0800..U+D7FF
             c >= 0xE000 && c <= 0xFFFF) { // U+E000..U+FFFF
      prepare(n + 3)
      string.codeUnits(n)     = (0xE0 | (c  >>> 12)).toByte
      string.codeUnits(n + 1) = (0x80 | ((c >>>  6) & 0x3F)).toByte
      string.codeUnits(n + 2) = (0x80 | (c & 0x3F)).toByte
      size = n + 3
    }
    else if (c >= 0x10000 && c <= 0x10FFFF) { // U+10000..U+10FFFF
      prepare(n + 4)
      string.codeUnits(n)     = (0xF0 | (c  >>> 18)).toByte
      string.codeUnits(n + 1) = (0x80 | ((c >>> 12) & 0x3F)).toByte
      string.codeUnits(n + 2) = (0x80 | ((c >>>  6) & 0x3F)).toByte
      string.codeUnits(n + 3) = (0x80 | (c & 0x3F)).toByte
      size = n + 4
    }
    else { // surrogate or invalid code point
      prepare(n + 3)
      string.codeUnits(n)     = 0xEF.toByte
      string.codeUnits(n + 1) = 0xBF.toByte
      string.codeUnits(n + 2) = 0xBD.toByte
      size = n + 3
    }
    this
  }
  
  override def expect(count: Int): this.type = {
    if (size + count > string.size) {
      string = string.copy(size + count)
      aliased = false
    }
    this
  }
  
  override def state: String1 = {
    if (size != string.size) string = string.copy(size)
    aliased = true
    string
  }
  
  override def clear() {
    string = String1.empty
    aliased = true
    size = 0
  }
}
