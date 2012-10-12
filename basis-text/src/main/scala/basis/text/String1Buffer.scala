/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

/** A buffer for 8-bit Unicode strings in the UTF-8 encoding form.
  * Produces only well-formed code unit sequences. */
final class String1Buffer extends Buffer[String1, Char] with CharBuffer {
  override type State = String1
  
  private[this] var string: String1 = String1.empty
  
  private[this] var aliased: Boolean = true
  
  private[this] var size: Int = 0
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = scala.math.max(base, size) - 1
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
  
  override def check: String1 = {
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
