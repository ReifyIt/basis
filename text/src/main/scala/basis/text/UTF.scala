//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.text

import basis.collections._
import scala.annotation._

trait UTF extends Any with Equals with Family[UTF] with Seq[Int] {
  /** Returns this string, quoted and escaped. */
  def show: String = {
    val builder = UString.Builder()
    show(builder)
    builder.state.toString
  }

  /** Appends this string, quoted and escaped, to the given builder. */
  def show(builder: StringBuilder): Unit = {
    def hexToChar(x: Int): Char = (if (x < 10) '0' + x else 'A' + (x - 10)).toChar
    builder.append('\"')
    val cs = utf16Iterator
    while (!cs.isEmpty) {
      (cs.head: @switch) match {
        case '\b' => builder.append('\\'); builder.append('b')
        case '\t' => builder.append('\\'); builder.append('t')
        case '\n' => builder.append('\\'); builder.append('n')
        case '\f' => builder.append('\\'); builder.append('f')
        case '\r' => builder.append('\\'); builder.append('r')
        case '\"' => builder.append('\\'); builder.append('\"')
        case '\\' => builder.append('\\'); builder.append('\\')
        case c if (c >= 0x0000 && c <= 0x001F) ||
                  (c >= 0x007F && c <= 0x009F) =>
          builder.append('\\'); builder.append('u')
          builder.append(hexToChar(c >>> 12 & 0xF))
          builder.append(hexToChar(c >>>  8 & 0xF))
          builder.append(hexToChar(c >>>  4 & 0xF))
          builder.append(hexToChar(c        & 0xF))
        case c => builder.append(c)
      }
      cs.step()
    }
    builder.append('\"')
  }

  /** Returns an iterator over the code points of this $collection. */
  override def iterator: Iterator[Int]

  /** Returns an iterator over the UTF-8 code units that encode this $collection. */
  def utf8Iterator: Iterator[Int] = new UTF8EncodingIterator(iterator)

  /** Returns an iterator over the UTF-16 code units that encode this $collection. */
  def utf16Iterator: Iterator[Int] = new UTF16EncodingIterator(iterator)

  /** Returns an iterator over the UTF-32 code units that encode this $collection. */
  def utf32Iterator: Iterator[Int] = new UTF32EncodingIterator(iterator)

  /** Returns the number of UTF-8 code units required to encode this $collection. */
  def utf8Length: Int = utf8Iterator.length

  /** Returns the number of UTF-16 code units required to encode this $collection. */
  def utf16Length: Int = utf16Iterator.length

  /** Returns the number of UTF-32 code units required to encode this $collection. */
  def utf32Length: Int = utf32Iterator.length

  /** Returns the number of Modified UTF-8 code units required to encode this
    * string as a cstring; includes the trailing null byte. */
  def cStringLength: Int = {
    val cs = utf8Iterator
    var count = 0
    while (!cs.isEmpty) {
      val c = cs.head
      if (c != 0) count += 1
      else count += 2 // null byte encoded as 0xC0, 0x80
      cs.step()
    }
    count + 1 // sentinel
  }

  /** Returns a Java String equivalent to this $collection. */
  override def toString: String = {
    val s = new java.lang.StringBuilder
    val cs = iterator
    while (!cs.isEmpty) {
      s.appendCodePoint(cs.head)
      cs.step()
    }
    s.toString
  }
}