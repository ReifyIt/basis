/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

/** A buffer for 32-bit Unicode strings in the UTF-32 encoding form.
  * Produces only well-formed code unit sequences. */
final class UTF32Buffer extends Buffer[UTF32String, Char] with StringBuffer {
  override type State = UTF32String
  
  private[this] var string: UTF32String = UTF32String.empty
  
  private[this] var aliased: Boolean = true
  
  private[this] var size: Int = 0
  
  private[this] def prepare(size: Int) {
    if (aliased || size > string.length) {
      string = string.copy(String.expand(16, size))
      aliased = false
    }
  }
  
  override def += (char: Char): this.type = {
    val c = char.codePoint
    val n = size
    if ((c >= 0x0000 && c <= 0xD7FF) ||
        (c >= 0xE000 && c <= 0x10FFFF)) { // U+0000..U+D7FF | U+E000..U+FFFF
      prepare(n + 1)
      string.codeUnits(n) = c
      size = n + 1
    }
    else { // invalid code point
      prepare(n + 1)
      string.codeUnits(n) = 0xFFFD
      size = n + 1
    }
    this
  }
  
  override def expect(count: Int): this.type = {
    if (size + count > string.length) {
      string = string.copy(size + count)
      aliased = false
    }
    this
  }
  
  override def check: UTF32String = {
    if (size != string.length) string = string.copy(size)
    aliased = true
    string
  }
  
  override def clear() {
    string = UTF32String.empty
    aliased = true
    size = 0
  }
}
