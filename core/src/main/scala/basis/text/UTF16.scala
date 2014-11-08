//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.text

import basis._
import basis.collections._
import scala.annotation._

trait UTF16 extends Any with Equals with Family[UTF16] with UTF {
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

  /** Returns the number of unsigned 16-bit code units in this $collection.
    * @group Measuring */
  def size: Int

  /** Returns the unsigned 16-bit code unit at `index`.
    * @group Indexing */
  def get(index: Int): Int

  /** Returns the decoded character beginning at `index`. Returns the
    * replacement character U+FFFD at invalid indexes.
    * @group Indexing */
  def apply(index: Int): Int = {
    val n = size
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    val c1 = get(index)
    if (c1 <= 0xD7FF || c1 >= 0xE000) c1 // U+0000..U+D7FF | U+E000..U+FFFF
    else if (c1 <= 0xDBFF && index + 1 < n) { // c1 >= 0xD800
      val c2 = get(index + 1)
      if (c2 >= 0xDC00 && c2 <= 0xDFFF) // U+10000..U+10FFFF
        (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
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
    if (c1 <= 0xD7FF || c1 >= 0xE000) // U+0000..U+D7FF | U+E000..U+FFFF
      index + 1
    else if (c1 <= 0xDBFF && index + 1 < n) { // c1 >= 0xD800
      val c2 = get(index + 1)
      if (c2 >= 0xDC00 && c2 <= 0xDFFF) // U+10000..U+10FFFF
        index + 2
      else index + 1
    }
    else index + 1
  }

  /** Sequentially applies a function to each code point in this $collection.
    * Applies the replacement character U+FFFD in lieu of unpaired surrogates. */
  override def traverse(f: Int => Unit): Unit = {
    var i = 0
    val n = size
    while (i < n) f {
      val c1 = get(i)
      i += 1
      if (c1 <= 0xD7FF || c1 >= 0xE000) c1 // U+0000..U+D7FF | U+E000..U+FFFF
      else if (c1 <= 0xDBFF && i < n) { // c1 >= 0xD800
        val c2 = get(i)
        if (c2 >= 0xDC00 && c2 <= 0xDFFF) { // U+10000..U+10FFFF
          i += 1
          (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
        }
        else 0xFFFD
      }
      else 0xFFFD
    }
  }

  override def iterator: Iterator[Int] = new UTF16DecodingIterator(this)

  override def utf16Iterator: Iterator[Int] = new UTF16Iterator(this)

  override def utf16Length: Int = size

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

object UTF16 {
  /** Returns a new builder that composes UTF-16 code units into valid Unicode code points. */
  def Decoder(self: StringBuilder): Builder[Int] with State[self.State] = new UTF16Decoder[self.State](self)

  override def toString: String = "UTF16"
}

private[text] final class UTF16Decoder[+Result](self: StringBuilder with State[Result]) extends Builder[Int] with State[Result] {
  private[this] var c1: Int = 0
  private[this] var c2: Int = 0
  private[this] var n: Int = 0

  protected def appendCodeUnit(c: Int): Unit = n match {
    case 0 => c1 = c & 0xFFFF; n = 1
    case 1 => c2 = c & 0xFFFF; n = 2
  }

  protected def appendCodePoint(c: Int): Unit = {
    self.append(c)
    c1 = 0; c2 = 0
    n = 0
  }

  override def append(c: Int): Unit = {
    appendCodeUnit(c)
    val c1 = this.c1
    if (c1 <= 0xD7FF || c1 >= 0xE000) appendCodePoint(c1) // U+0000..U+D7FF | U+E000..U+FFFF
    else if (c1 <= 0xDBFF) { // c1 >= 0xD800
      if (n >= 1) {
        val c2 = this.c2
        if (c2 >= 0xDC00 && c2 <= 0xDFFF) // U+10000..U+10FFFF
          appendCodePoint((((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000)
        else appendCodePoint(0xFFFD)
      }
    }
    else appendCodePoint(0xFFFD)
  }

  override def expect(count: Int): this.type = this

  override def state: State = self.state

  override def clear(): Unit = {
    self.clear()
    c1 = 0; c2 = 0
    n = 0
  }

  override def toString: String = "UTF16"+"."+"Decoder"+"("+ self +")"
}

private[text] final class UTF16Iterator(text: UTF16, private[this] var index: Int) extends Iterator[Int] {
  def this(text: UTF16) = this(text, 0)

  override def isEmpty: Boolean = index >= text.size

  override def head: Int = {
    if (index >= text.size) Iterator.empty.head
    text.get(index)
  }

  override def step(): Unit = {
    if (index >= text.size) Iterator.empty.step()
    index += 1
  }

  override def dup: Iterator[Int] = new UTF16Iterator(text, index)
}

private[text] final class UTF16DecodingIterator(text: UTF16, private[this] var index: Int) extends Iterator[Int] {
  def this(text: UTF16) = this(text, 0)

  override def isEmpty: Boolean = index >= text.size

  override def head: Int = {
    if (index >= text.size) Iterator.empty.head
    text(index)
  }

  override def step(): Unit = {
    if (index >= text.size) Iterator.empty.step()
    index = text.nextIndex(index)
  }

  override def dup: Iterator[Int] = new UTF16DecodingIterator(text, index)
}

private[text] final class UTF16EncodingIterator(
    private[this] val self: Iterator[Int],
    private[this] var c1: Int,
    private[this] var c2: Int,
    private[this] var i: Int,
    private[this] var n: Int)
  extends Iterator[Int] {

  def this(self: Iterator[Int]) = this(self, 0, 0, 0, 0)

  protected def encode(): Unit = {
    val c = self.head
    self.step()
    i = 0
    if ((c >= 0x0000 && c <= 0xD7FF) ||
        (c >= 0xE000 && c <= 0xFFFF)) { // U+0000..U+D7FF | U+E000..U+FFFF
      c1 = c
      n = 1
    }
    else if (c >= 0x10000 && c <= 0x10FFFF) { // U+10000..U+10FFFF
      val u = c - 0x10000
      c1 = 0xD800 | (u >>> 10)
      c2 = 0xDC00 | (u & 0x3FF)
      n = 2
    }
    else { // invalid code point
      c1 = 0xFFFD
      n = 1
    }
  }

  override def isEmpty: Boolean = i == n && self.isEmpty

  @tailrec override def head: Int = {
    if (i < n) i match {
      case 0 => c1
      case 1 => c2
    }
    else if (!self.isEmpty) { encode(); head }
    else Iterator.empty.head
  }

  override def step(): Unit = {
    if (i < n) i += 1
    else if (!self.isEmpty) { encode(); i = 1 }
    else Iterator.empty.step()
  }

  override def dup: Iterator[Int] = new UTF16EncodingIterator(self.dup, c1, c2, i, n)
}
