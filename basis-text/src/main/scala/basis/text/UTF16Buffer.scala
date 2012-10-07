/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._
import basis.util._

/** A buffer for 16-bit Unicode strings in the UTF-16 encoding form.
  * Produces only well-formed code unit sequences. */
final class UTF16Buffer extends StringBuffer[UTF16String] {
  override type State = UTF16String
  
  private[this] var string: UTF16String = UTF16String.empty
  
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
    if ((c >= 0x0000 && c <= 0xD7FF) ||
        (c >= 0xE000 && c <= 0xFFFF)) { // U+0000..U+D7FF | U+E000..U+FFFF
      prepare(n + 1)
      string.codeUnits(n) = c.toChar
      size = n + 1
    }
    else if (c >= 0x10000 && c <= 0x10FFFF) { // U+10000..U+10FFFF
      prepare(n + 2)
      val u = c - 0x10000
      string.codeUnits(n)     = (0xD800 | (u >>> 10)).toChar
      string.codeUnits(n + 1) = (0xDC00 | (u & 0x3FF)).toChar
      size = n + 2
    }
    else { // invalid code point
      prepare(n + 1)
      string.codeUnits(n) = 0xFFFD.toChar
      size = n + 1
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
  
  override def check: UTF16String = {
    if (size != string.size) string = string.copy(size)
    aliased = true
    string
  }
  
  override def clear() {
    string = UTF16String.empty
    aliased = true
    size = 0
  }
}
