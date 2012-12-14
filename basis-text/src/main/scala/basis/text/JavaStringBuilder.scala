/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis.collections._
import basis.util._

private[text] final class JavaStringBuilder extends StringBuilder[Any] {
  override type State = String
  
  private[this] var codeUnits: Array[scala.Char] = null
  
  private[this] var size: Int = 0
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def resize(size: Int) {
    val newCodeUnits = new Array[scala.Char](size)
    if (codeUnits != null)
      java.lang.System.arraycopy(codeUnits, 0, newCodeUnits, 0, codeUnits.length min size)
    codeUnits = newCodeUnits
  }
  
  private[this] def prepare(size: Int) {
    if (codeUnits == null || size > codeUnits.length) resize(expand(16, size))
  }
  
  override def += (c: Int): this.type = {
    val n = size
    if ((c >= 0x0000 && c <= 0xD7FF) ||
        (c >= 0xE000 && c <= 0xFFFF)) { // U+0000..U+D7FF | U+E000..U+FFFF
      prepare(n + 1)
      codeUnits(n) = c.toChar
      size = n + 1
    }
    else if (c >= 0x10000 && c <= 0x10FFFF) { // U+10000..U+10FFFF
      prepare(n + 2)
      val u = c - 0x10000
      codeUnits(n)     = (0xD800 | (u >>> 10)).toChar
      codeUnits(n + 1) = (0xDC00 | (u & 0x3FF)).toChar
      size = n + 2
    }
    else { // invalid code point
      prepare(n + 1)
      codeUnits(n) = 0xFFFD.toChar
      size = n + 1
    }
    this
  }
  
  override def ++= (elems: Enumerator[Int]): this.type = Predef.???
  
  override def expect(count: Int): this.type = {
    if (codeUnits == null || size + count > codeUnits.length) resize(size + count)
    this
  }
  
  override def state: String = {
    if (codeUnits == null || codeUnits.length == 0) ""
    else new String(codeUnits, 0, size)
  }
  
  override def clear() {
    codeUnits = null
    size = 0
  }
}
