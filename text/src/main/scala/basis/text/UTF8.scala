//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.text

import basis.collections._
import scala.annotation._

trait UTF8 extends Any with Equals with Family[UTF8] with UTF {
  /** Returns `true` if this $collection doesn't contain any code units.
    * @group Measuring */
  override def isEmpty: Boolean = size == 0

  /** Returns the number of code points in this $collection.
    * @group Measuring */
  override def length: Int = {
    var l = 0
    var i = 0
    val n = size
    while (i < n) {
      l += 1
      i = nextIndex(i)
    }
    l
  }

  /** Returns the number of unsigned 8-bit code units in this $collection.
    * @group Measuring */
  def size: Int

  /** Returns the unsigned 8-bit code unit at `index`.
    * @group Indexing */
  def get(index: Int): Int

  /** Returns the decoded character beginning at `index`. Returns the
    * replacement character U+FFFD at invalid indexes.
    * @group Indexing */
  def apply(index: Int): Int = {
    val n = size
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    val c1 = get(index)
    if (c1 <= 0x7F) c1 // U+0000..U+007F
    else if (c1 >= 0xC2 && c1 <= 0xF4 && index + 1 < n) {
      val c2 = get(index + 1)
      if (c1 <= 0xDF && c2 >= 0x80 && c2 <= 0xBF) // U+0080..U+07FF
        ((c1 & 0x1F) << 6) | (c2 & 0x3F)
      else if (index + 2 < n) {
        val c3 = get(index + 2)
        if ((c1 == 0xE0 &&
             c2 >= 0xA0 && c2 <= 0xBF
          || c1 == 0xED &&
             c2 >= 0x80 && c2 <= 0x9F
          || c1 >= 0xE1 && c1 <= 0xEF &&
             c2 >= 0x80 && c2 <= 0xBF)
          && c3 >= 0x80 && c3 <= 0xBF) // U+0800..U+FFFF
          ((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F)
        else if (index + 3 < n) {
          val c4 = get(index + 3)
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
  }

  /** Returns the index following the valid subsequence, or maximal subpart
    * thereof, at `index`.
    * @group Indexing */
  def nextIndex(index: Int): Int = {
    val n = size
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    val c1 = get(index)
    if (c1 <= 0x7F) // U+0000..U+007F
      index + 1
    else if (c1 >= 0xC2 && c1 <= 0xF4 && index + 1 < n) {
      val c2 = get(index + 1)
      if (c1 <= 0xDF && c2 >= 0x80 && c2 <= 0xBF) // U+0080..U+07FF
        index + 2
      else if (index + 2 < n) {
        val c3 = get(index + 2)
        if ((c1 == 0xE0 &&
             c2 >= 0xA0 && c2 <= 0xBF
          || c1 == 0xED &&
             c2 >= 0x80 && c2 <= 0x9F
          || c1 >= 0xE1 && c1 <= 0xEF &&
             c2 >= 0x80 && c2 <= 0xBF)
          && c3 >= 0x80 && c3 <= 0xBF) // U+0800..U+FFFF
          index + 3
        else if (index + 3 < n) {
          val c4 = get(index + 3)
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

  /** Sequentially applies a function to each code point in this $collection.
    * Applies the replacement character U+FFFD in lieu of the maximal subpart
    * of any ill-formed subsequences. */
  override def traverse(f: Int => Unit): Unit = {
    var i = 0
    val n = size
    while (i < n) f {
      val c1 = get(i)
      i += 1
      if (c1 <= 0x7F) c1 // U+0000..U+007F
      else if (c1 >= 0xC2 && c1 <= 0xF4 && i < n) {
        val c2 = get(i)
        if (c1 <= 0xDF && c2 >= 0x80 && c2 <= 0xBF) { // U+0080..U+07FF
          i += 1
          ((c1 & 0x1F) << 6) | (c2 & 0x3F)
        }
        else if (i < n) {
          i += 1
          val c3 = get(i)
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
            val c4 = get(i)
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
    }
  }

  override def iterator: Iterator[Int] = new UTF8DecodingIterator(this)

  override def utf8Iterator: Iterator[Int] = new UTF8Iterator(this)

  override def utf8Length: Int = size

  /** Returns a Java String equivalent to this $collection. */
  override def toString: String = {
    val s = new java.lang.StringBuilder
    var i = 0
    val n = size
    while (i < n) {
      s.appendCodePoint(this(i))
      i = nextIndex(i)
    }
    s.toString
  }
}

object UTF8 {
  /** Returns a new builder that composes UTF-8 code units into valid Unicode code points. */
  def Builder(self: StringBuilder): Builder[Int] with State[self.State] = new UTF8Builder[self.State](self)

  override def toString: String = "UTF8"
}

private[text] final class UTF8Builder[+Result](self: StringBuilder with State[Result]) extends Builder[Int] with State[Result] {
  private[this] var c1: Int = 0
  private[this] var c2: Int = 0
  private[this] var c3: Int = 0
  private[this] var c4: Int = 0
  private[this] var n: Int = 0

  protected def appendCodeUnit(c: Int): Unit = (n: @switch) match {
    case 0 => c1 = c & 0xFF; n = 1
    case 1 => c2 = c & 0xFF; n = 2
    case 2 => c3 = c & 0xFF; n = 3
    case 3 => c4 = c & 0xFF; n = 4
  }

  protected def appendCodePoint(c: Int): Unit = {
    self.append(c)
    c1 = 0; c2 = 0; c3 = 0; c4 = 0
    n = 0
  }

  override def append(c: Int): Unit = {
    appendCodeUnit(c)
    val c1 = this.c1
    if (c1 <= 0x7F) appendCodePoint(c1) // U+0000..U+007F
    else if (c1 >= 0xC2 && c1 <= 0xF4) {
      if (n >= 1) {
        val c2 = this.c2
        if (c1 <= 0xDF) {
          if (c2 >= 0x80 && c2 <= 0xBF)// U+0080..U+07FF
            appendCodePoint(((c1 & 0x1F) << 6) | (c2 & 0x3F))
          else {
            appendCodePoint(0xFFFD)
            appendCodeUnit(c)
          }
        }
        else if (n >= 2) {
          val c3 = this.c3
          if (c1 == 0xE0 &&
              c2 >= 0xA0 && c2 <= 0xBF
           || c1 == 0xED &&
              c2 >= 0x80 && c2 <= 0x9F
           || c1 >= 0xE1 && c1 <= 0xEF &&
              c2 >= 0x80 && c2 <= 0xBF) {
            if (c3 >= 0x80 && c3 <= 0xBF) // U+0800..U+FFFF
              appendCodePoint(((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F))
            else {
              appendCodePoint(0xFFFD)
              appendCodeUnit(c)
            }
          }
          else if (n >= 3) {
            val c4 = this.c4
            if ((c1 == 0xF0 &&
                 c2 >= 0x90 && c2 <= 0xBF
              || c1 >= 0xF1 && c1 <= 0xF3 &&
                 c2 >= 0x80 && c2 <= 0xBF
              || c1 == 0xF4 &&
                 c2 >= 0x80 && c2 <= 0x8F)
              && c3 >= 0x80 && c3 <= 0xBF) {
              if (c4 >= 0x80 && c4 <= 0xBF) // U+10000..U+10FFFF
                appendCodePoint(((c1 & 0x07) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F))
              else {
                appendCodePoint(0xFFFD)
                appendCodeUnit(c)
              }
            }
            else appendCodePoint(0xFFFD)
          }
        }
      }
    }
    else appendCodePoint(0xFFFD)
  }

  override def expect(count: Int): this.type = this

  override def state: State = self.state

  override def clear(): Unit = {
    self.clear()
    c1 = 0; c2 = 0; c3 = 0; c4 = 0
    n = 0
  }

  override def toString: String = "UTF8"+"."+"Builder"+"("+ self +")"
}

private[text] final class UTF8Iterator(text: UTF8, private[this] var index: Int) extends Iterator[Int] {
  def this(text: UTF8) = this(text, 0)

  override def isEmpty: Boolean = index >= text.size

  override def head: Int = {
    if (index >= text.size) Iterator.empty.head
    text.get(index)
  }

  override def step(): Unit = {
    if (index >= text.size) Iterator.empty.step()
    index += 1
  }

  override def dup: Iterator[Int] = new UTF8Iterator(text, index)
}

private[text] final class UTF8DecodingIterator(text: UTF8, private[this] var index: Int) extends Iterator[Int] {
  def this(text: UTF8) = this(text, 0)

  override def isEmpty: Boolean = index >= text.size

  override def head: Int = {
    if (index >= text.size) Iterator.empty.head
    text(index)
  }

  override def step(): Unit = {
    if (index >= text.size) Iterator.empty.step()
    index = text.nextIndex(index)
  }

  override def dup: Iterator[Int] = new UTF8DecodingIterator(text, index)
}

private[text] final class UTF8EncodingIterator(
    private[this] val self: Iterator[Int],
    private[this] var c1: Int,
    private[this] var c2: Int,
    private[this] var c3: Int,
    private[this] var c4: Int,
    private[this] var i: Int,
    private[this] var n: Int)
  extends Iterator[Int] {

  def this(self: Iterator[Int]) = this(self, 0, 0, 0, 0, 0, 0)

  protected def encode(): Unit = {
    val c = self.head
    self.step()
    i = 0
    if (c >= 0x0000 && c <= 0x007F) { // U+0000..U+007F
      c1 = c
      n = 1
    }
    else if (c >= 0x0080 && c <= 0x07FF) { // U+0080..U+07FF
      c1 = 0xC0 | (c >>> 6)
      c2 = 0x80 | (c & 0x3F)
      n = 2
    }
    else if (c >= 0x0800 && c <= 0xFFFF || // U+0800..U+D7FF
             c >= 0xE000 && c <= 0xFFFF) { // U+E000..U+FFFF
      c1 = 0xE0 | (c  >>> 12)
      c2 = 0x80 | ((c >>>  6) & 0x3F)
      c3 = 0x80 | (c & 0x3F)
      n = 3
    }
    else if (c >= 0x10000 && c <= 0x10FFFF) { // U+10000..U+10FFFF
      c1 = 0xF0 | (c  >>> 18)
      c2 = 0x80 | ((c >>> 12) & 0x3F)
      c3 = 0x80 | ((c >>>  6) & 0x3F)
      c4 = 0x80 | (c & 0x3F)
      n = 4
    }
    else { // surrogate or invalid code point
      c1 = 0xEF
      c2 = 0xBF
      c3 = 0xBD
      n = 3
    }
  }

  override def isEmpty: Boolean = i == n && self.isEmpty

  @tailrec override def head: Int = {
    if (i < n) (i: @switch) match {
      case 0 => c1
      case 1 => c2
      case 2 => c3
      case 3 => c4
    }
    else if (!self.isEmpty) { encode(); head }
    else Iterator.empty.head
  }

  override def step(): Unit = {
    if (i < n) i += 1
    else if (!self.isEmpty) { encode(); i = 1 }
    else Iterator.empty.step()
  }

  override def dup: Iterator[Int] = new UTF8EncodingIterator(self.dup, c1, c2, c3, c4, i, n)
}
