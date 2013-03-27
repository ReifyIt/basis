/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.text

import basis.collections._
import basis.util._

import scala.annotation.switch

/** A UTF-16 code unit sequence.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * @group    Unicode
  * 
  * @groupprio  Measuring     1
  * @groupprio  Indexing      2
  * @groupprio  Traversing    3
  * @groupprio  Classifying   4
  * 
  * @define collection  text
  */
abstract class UTF16 extends Equals with Family[UTF16] with Seq[Int] {
  /** Returns `true` if this $collection doesn't contain any code units.
    * @group Measuring */
  def isEmpty: Boolean = size == 0
  
  /** Returns the number of code points in this $collection.
    * @group Measuring */
  def length: Int = {
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
  override def traverse(f: Int => Unit) {
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
  
  /** Returns a new iterator over the code points of this $collection. */
  override def iterator: Iterator[Int] = new UTF16Iterator(this, 0)
  
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

private[text] final class UTF16Builder(val self: Builder[Int]) extends Builder[Int] {
  override type Scope = self.Scope
  
  override type State = self.State
  
  private[this] var c1: Int = 0
  private[this] var c2: Int = 0
  private[this] var n: Int = 0
  
  protected def appendCodeUnit(c: Int): Unit = (n: @switch) match {
    case 0 => c1 = c & 0xFFFF
    case 1 => c2 = c & 0xFFFF
  }
  
  protected def appendCodePoint(c: Int) {
    self.append(c)
    c1 = 0; c2 = 0
    n = 0
  }
  
  override def append(c: Int) {
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
  
  override def clear() {
    self.clear()
    c1 = 0; c2 = 0
    n = 0
  }
  
  override def toString: String = "UTF16Builder"+"("+ self +")"
}
