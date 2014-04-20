//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.text

import basis._
import basis.util._

final class String4(codeUnits: Array[Int]) extends Equals with Family[String4] with UTF32 {
  override def length: Int = codeUnits.length

  override def get(index: Int): Int = codeUnits(index)
}

object String4 extends StringFactory[String4] {
  override val empty: String4 = new String4(new Array[Int](0))

  implicit override def Builder: StringBuilder with State[String4] = new String4Builder

  override def toString: String = "String4"
}

private[text] final class String4Builder extends StringBuilder with State[String4] {
  private[this] var codeUnits: Array[Int] = _

  private[this] var size: Int = 0

  private[this] var aliased: Boolean = true

  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }

  private[this] def resize(size: Int): Unit = {
    val newCodeUnits = new Array[Int](size)
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
        (c >= 0xE000 && c <= 0x10FFFF)) { // U+0000..U+D7FF | U+E000..U+FFFF
      prepare(n + 1)
      codeUnits(n) = c
      size = n + 1
    }
    else { // invalid code point
      prepare(n + 1)
      codeUnits(n) = 0xFFFD
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

  override def state: String4 = {
    if (codeUnits == null || size != codeUnits.length) resize(size)
    aliased = true
    new String4(codeUnits)
  }

  override def clear(): Unit = {
    codeUnits = null
    aliased = true
    size = 0
  }

  override def toString: String = "String4"+"."+"Builder"
}
