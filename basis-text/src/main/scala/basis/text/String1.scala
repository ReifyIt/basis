//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.text

import basis.collections._
import basis.util._

final class String1(codeUnits: Array[Byte]) extends Equals with Family[String1] with UTF8 {
  override def size: Int = codeUnits.length

  override def get(index: Int): Int = codeUnits(index) & 0xFF
}

object String1 extends StringFactory[String1] {
  override val empty: String1 = new String1(new Array[Byte](0))

  implicit override def Builder(): StringBuilder with State[String1] = new String1Builder

  override def toString: String = "String1"
}

private[text] final class String1Builder extends StringBuilder with State[String1] {
  private[this] var codeUnits: Array[Byte] = _

  private[this] var size: Int = 0

  private[this] var aliased: Boolean = true

  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }

  private[this] def resize(size: Int): Unit = {
    val newCodeUnits = new Array[Byte](size)
    if (codeUnits != null) java.lang.System.arraycopy(codeUnits, 0, newCodeUnits, 0, codeUnits.length min size)
    codeUnits = newCodeUnits
  }

  private[this] def prepare(size: Int): Unit = {
    if (aliased || size > codeUnits.length) {
      resize(expand(16, size))
      aliased = false
    }
  }

  override def append(c: Int): Unit = {
    val n = size
    if (c >= 0x0000 && c <= 0x007F) { // U+0000..U+007F
      prepare(n + 1)
      codeUnits(n) = c.toByte
      size = n + 1
    }
    else if (c >= 0x0080 && c <= 0x07FF) { // U+0080..U+07FF
      prepare(n + 2)
      codeUnits(n)     = (0xC0 | (c >>> 6)).toByte
      codeUnits(n + 1) = (0x80 | (c & 0x3F)).toByte
      size = n + 2
    }
    else if (c >= 0x0800 && c <= 0xFFFF || // U+0800..U+D7FF
             c >= 0xE000 && c <= 0xFFFF) { // U+E000..U+FFFF
      prepare(n + 3)
      codeUnits(n)     = (0xE0 | (c  >>> 12)).toByte
      codeUnits(n + 1) = (0x80 | ((c >>>  6) & 0x3F)).toByte
      codeUnits(n + 2) = (0x80 | (c & 0x3F)).toByte
      size = n + 3
    }
    else if (c >= 0x10000 && c <= 0x10FFFF) { // U+10000..U+10FFFF
      prepare(n + 4)
      codeUnits(n)     = (0xF0 | (c  >>> 18)).toByte
      codeUnits(n + 1) = (0x80 | ((c >>> 12) & 0x3F)).toByte
      codeUnits(n + 2) = (0x80 | ((c >>>  6) & 0x3F)).toByte
      codeUnits(n + 3) = (0x80 | (c & 0x3F)).toByte
      size = n + 4
    }
    else { // surrogate or invalid code point
      prepare(n + 3)
      codeUnits(n)     = 0xEF.toByte
      codeUnits(n + 1) = 0xBF.toByte
      codeUnits(n + 2) = 0xBD.toByte
      size = n + 3
    }
  }

  override def expect(count: Int): this.type = {
    if (codeUnits == null || size + count > codeUnits.length) {
      resize(size + count)
      aliased = false
    }
    this
  }

  override def state: String1 = {
    if (codeUnits == null || size != codeUnits.length) resize(size)
    aliased = true
    new String1(codeUnits)
  }

  override def clear(): Unit = {
    codeUnits = null
    aliased = true
    size = 0
  }

  override def toString: String = "String1"+"."+"Builder"+"()"
}
