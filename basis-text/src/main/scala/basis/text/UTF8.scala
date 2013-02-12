/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.text

import basis.collections._
import basis.util._

/** A UTF-8 code unit sequence.
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
abstract class UTF8 extends Equals with Family[UTF8] with Seq[Int] {
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
  
  /** Returns a new iterator over the code points of this $collection. */
  override def iterator: Iterator[Int] = new UTF8Iterator(this, 0)
  
  /** Sequentially applies a function to each code point in this $collection.
    * Applies the replacement character U+FFFD in lieu of the maximal subpart
    * of any ill-formed subsequences. */
  override def traverse(f: Int => Unit) {
    var i = 0
    var n = size
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

private[text] final class UTF8Iterator(text: UTF8, private[this] var index: Int) extends Iterator[Int] {
  override def isEmpty: Boolean = index >= text.size
  
  override def head: Int = {
    if (index >= text.size) throw new NoSuchElementException("Head of empty iterator.")
    text(index)
  }
  
  override def step() {
    if (index >= text.size) throw new UnsupportedOperationException("Empty iterator step.")
    index = text.nextIndex(index)
  }
  
  override def dup: Iterator[Int] = new UTF8Iterator(text, index)
}
