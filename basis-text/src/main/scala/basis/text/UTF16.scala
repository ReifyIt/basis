/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.text

import basis.collections._
import basis.util._

/** A UTF-16 code unit sequence.
  * 
  * @groupprio  Quantifying   1
  * @groupprio  Indexing      2
  * @groupprio  Iterating     3
  * @groupprio  Traversing    4
  * @groupprio  Classifying   5
  * 
  * @define collection  text
  */
abstract class UTF16 extends Equals with Family[UTF16] with Seq[Int] {
  /** Returns `true` if this $collection doesn't contain any code units. */
  override def isEmpty: Boolean = size == 0
  
  /** Returns the number of code points in this $collection. */
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
    * @group Quantifying */
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
  
  /** Returns a new iterator over the code points of this $collection. */
  override def iterator: Iterator[Int] = new UTF16Iterator(this, 0)
  
  /** Sequentially applies a function to each code point in this $collection.
    * Applies the replacement character U+FFFD in lieu of unpaired surrogates. */
  protected override def foreach[U](f: Int => U) {
    var i = 0
    val n = size
    while (i < n) f({
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
    }: Int)
  }
  
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

private[text] final class UTF16Iterator(text: UTF16, private[this] var index: Int) extends Iterator[Int] {
  override def isEmpty: Boolean = index >= text.size
  
  override def head: Int = {
    if (index >= text.size) throw new NoSuchElementException("Head of empty iterator.")
    text(index)
  }
  
  override def step() {
    if (index >= text.size) throw new UnsupportedOperationException("Empty iterator step.")
    index = text.nextIndex(index)
  }
  
  override def dup: Iterator[Int] = new UTF16Iterator(text, index)
}
