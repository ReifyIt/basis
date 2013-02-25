/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.text

import basis.collections._
import basis.util._

/** A UTF-32 code unit sequence.
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
abstract class UTF32 extends Equals with Family[UTF32] with Index[Int] {
  /** Returns `true` if this $collection doesn't contain any code units.
    * @group Measuring */
  def isEmpty: Boolean = length == 0
  
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
  override def traverse(f: Int => Unit) {
    var i = 0
    val n = length
    while (i < n) {
      f(this(i))
      i += 1
    }
  }
  
  /** Returns a new iterator over the code points of this $collection. */
  override def iterator: Iterator[Int] = new UTF32Iterator(this, 0)
  
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

private[text] final class UTF32Iterator(text: UTF32, private[this] var index: Int) extends Iterator[Int] {
  override def isEmpty: Boolean = index >= text.length
  
  override def head: Int = {
    if (index >= text.length) throw new NoSuchElementException("Head of empty iterator.")
    text(index)
  }
  
  override def step() {
    if (index >= text.length) throw new UnsupportedOperationException("Empty iterator step.")
    index += 1
  }
  
  override def dup: Iterator[Int] = new UTF32Iterator(text, index)
}
