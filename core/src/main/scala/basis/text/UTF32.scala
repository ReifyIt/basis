//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.text

import basis._
import basis.collections._

trait UTF32 extends Any with Equals with Family[UTF32] with IndexedSeq[Int] with UTF {
  /** Returns `true` if this $collection doesn't contain any code units.
    * @group Measuring */
  override def isEmpty: Boolean = length == 0

  /** Returns the number of code points in this $collection.
    * @group Measuring */
  override def length: Int

  /** Returns the unsigned 32-bit code unit at `index`.
    * @group Indexing */
  def get(index: Int): Int

  /** Returns the character at `index`. Substitutes the replacement character
    * U+FFFD in lieu of invalid characters. */
  override def apply(index: Int): Int = {
    val n = length
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    val c = get(index)
    if (c >= 0x0000 && c <= 0xD7FF ||
        c >= 0xE000 && c <= 0x10FFFF) c // U+0000..U+D7FF | U+E000..U+10FFFF
    else 0xFFFD
  }

  /** Sequentially applies a function to each code point in this $collection.
    * Applies the replacement character U+FFFD in lieu of invalid characters. */
  override def traverse(f: Int => Unit): Unit = {
    var i = 0
    val n = length
    while (i < n) {
      f(this(i))
      i += 1
    }
  }

  override def iterator: Iterator[Int] = new UTF32Iterator(this)

  override def utf32Iterator: Iterator[Int] = new UTF32Iterator(this)

  override def utf32Length: Int = length

  /** Returns a Java String equivalent to this $collection. */
  override def toString: String = {
    val s = new java.lang.StringBuilder
    var i = 0
    val n = length
    while (i < n) {
      s.appendCodePoint(this(i))
      i += 1
    }
    s.toString
  }
}

object UTF32 {
  /** Returns a new builder that composes UTF-32 code units into valid Unicode code points. */
  def Builder(self: StringBuilder): Builder[Int] with State[self.State] = new UTF32Builder[self.State](self)

  override def toString: String = "UTF32"
}

private[text] final class UTF32Builder[+Result](self: StringBuilder with State[Result]) extends Builder[Int] with State[Result] {
  protected def appendCodePoint(c: Int): Unit = self.append(c)

  override def append(c: Int): Unit = {
    if (c >= 0x0000 && c <= 0xD7FF ||
        c >= 0xE000 && c <= 0x10FFFF) appendCodePoint(c) // U+0000..U+D7FF | U+E000..U+10FFFF
    else appendCodePoint(0xFFFD)
  }

  override def expect(count: Int): this.type = this

  override def state: State = self.state

  override def clear(): Unit = self.clear()

  override def toString: String = "UTF32"+"."+"Builder"+"("+ self +")"
}

private[text] final class UTF32Iterator(text: UTF32, private[this] var index: Int) extends Iterator[Int] {
  def this(text: UTF32) = this(text, 0)

  override def isEmpty: Boolean = index >= text.length

  override def head: Int = {
    if (index >= text.length) Iterator.empty.head
    text(index)
  }

  override def step(): Unit = {
    if (index >= text.length) Iterator.empty.step()
    index += 1
  }

  override def dup: Iterator[Int] = new UTF32Iterator(text, index)
}

private[text] final class UTF32EncodingIterator(self: Iterator[Int]) extends Iterator[Int] {
  override def isEmpty: Boolean = self.isEmpty

  override def head: Int = {
    val c = self.head
    if (c >= 0x0000 && c <= 0xD7FF ||
        c >= 0xE000 && c <= 0x10FFFF) c // U+0000..U+D7FF | U+E000..U+10FFFF
    else 0xFFFD
  }

  override def step(): Unit = self.step()

  override def dup: Iterator[Int] = new UTF32EncodingIterator(self.dup)
}
