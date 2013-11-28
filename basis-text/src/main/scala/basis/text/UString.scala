//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.text

import basis.collections._
import basis.util._

/** Extended `String` operations.
  *
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  */
final class UString(val __ : String) extends AnyVal with Equals with Family[UString] with UTF16 {
  override def size: Int = __.length

  override def get(index: Int): Int = __.charAt(index)

  /** Returns an iterator over the code points of this string. */
  override def iterator: Iterator[Int] = new UStringDecodingIterator(__)

  /** Returns an iterator over the UTF-8 code units that encode this string. */
  override def utf8Iterator: Iterator[Int] = new UTF8EncodingIterator(iterator)

  /** Returns an iterator over the UTF-16 code units that encode this string. */
  override def utf16Iterator: Iterator[Int] = new UStringIterator(__)

  /** Returns an iterator over the UTF-32 code units that encode this string. */
  override def utf32Iterator: Iterator[Int] = new UTF32EncodingIterator(iterator)

  /** Returns the number of UTF-8 code units required to encode this string. */
  override def utf8Length: Int = utf8Iterator.length

  /** Returns the number of UTF-16 code units required to encode this string. */
  override def utf16Length: Int = __.length

  /** Returns the number of UTF-32 code units required to encode this string. */
  override def utf32Length: Int = utf32Iterator.length

  override def toString: String = __
}

object UString extends StringFactory[UString] {
  override def empty: UString = new UString("")

  override def apply(chars: CharSequence): UString = new UString(chars.toString)

  implicit override def Builder(): StringBuilder with State[UString] = new UStringBuilder

  override def toString: String = "UString"
}

private[text] final class UStringBuilder extends StringBuilder with State[UString] {
  private[this] var codeUnits: Array[Char] = null

  private[this] var size: Int = 0

  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }

  private[this] def resize(size: Int): Unit = {
    val newCodeUnits = new Array[Char](size)
    if (codeUnits != null)
      java.lang.System.arraycopy(codeUnits, 0, newCodeUnits, 0, codeUnits.length min size)
    codeUnits = newCodeUnits
  }

  private[this] def prepare(size: Int): Unit = {
    if (codeUnits == null || size > codeUnits.length) resize(expand(16, size))
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
    if (codeUnits == null || size + count > codeUnits.length) resize(size + count)
    this
  }

  override def state: UString = {
    if (codeUnits == null || codeUnits.length == 0) UString.empty
    else new UString(new String(codeUnits, 0, size))
  }

  override def clear(): Unit = {
    codeUnits = null
    size = 0
  }

  override def toString: String = "UString"+"."+"Builder"+"()"
}

private[text] final class UStringIterator(string: String, private[this] var index: Int) extends Iterator[Int] {
  def this(string: String) = this(string, 0)

  override def isEmpty: Boolean = index >= string.length

  override def head: Int = {
    if (index >= string.length) Iterator.empty.head
    string.charAt(index)
  }

  override def step(): Unit = {
    if (index >= string.length) Iterator.empty.step()
    index += 1
  }

  override def dup: Iterator[Int] = new UStringIterator(string, index)
}

private[text] final class UStringDecodingIterator(string: String, private[this] var index: Int) extends Iterator[Int] {
  def this(string: String) = this(string, 0)

  override def isEmpty: Boolean = index >= string.length

  override def head: Int = {
    if (index >= string.length) Iterator.empty.head
    string.codePointAt(index)
  }

  override def step(): Unit = {
    if (index >= string.length) Iterator.empty.step()
    index = string.offsetByCodePoints(index, 1)
  }

  override def dup: Iterator[Int] = new UStringDecodingIterator(string, index)
}
