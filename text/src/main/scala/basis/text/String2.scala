//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.text

import basis.collections._
import basis.util._

final class String2(codeUnits: Array[Char]) extends Equals with Family[String2] with UTF16 {
  override def size: Int = codeUnits.length

  override def get(index: Int): Int = codeUnits(index)
}

object String2 extends StringFactory[String2] {
  override val empty: String2 = new String2(new Array[Char](0))

  implicit override def Builder(): StringBuilder with State[String2] = new String2Builder

  override def toString: String = "String2"
}

private[text] final class String2Builder extends StringBuilder with State[String2] {
  private[this] var codeUnits: Array[Char] = _

  private[this] var size: Int = 0

  private[this] var aliased: Boolean = true

  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }

  private[this] def resize(size: Int): Unit = {
    val newCodeUnits = new Array[Char](size)
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
  }

  override def expect(count: Int): this.type = {
    if (codeUnits == null || size + count > codeUnits.length) {
      resize(size + count)
      aliased = false
    }
    this
  }

  override def state: String2 = {
    if (codeUnits == null || size != codeUnits.length) resize(size)
    aliased = true
    new String2(codeUnits)
  }

  override def clear(): Unit = {
    codeUnits = null
    aliased = true
    size = 0
  }

  override def toString: String = "String2"+"."+"Builder"+"()"
}
