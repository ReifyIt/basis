/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

/** A 32-bit Unicode string comprised of a UTF-32 code unit sequence.
  * 
  * @author Chris Sachs
  * 
  * @define collection  string
  */
class String4(val codeUnits: scala.Array[Int]) extends AnyVal with Rope {
  override type Self = String4
  
  override def isEmpty: Boolean = codeUnits.length == 0
  
  /** Returns the number of code points in this string. */
  override def length: Int = codeUnits.length
  
  /** Returns the character at `index`. Substitutes the replacement character
    * U+FFFD in lieu of invalid characters. */
  def apply(index: Int): Char = {
    val n = codeUnits.length
    if (index < 0 || index >= n)
      throw new java.lang.IndexOutOfBoundsException(index.toString)
    new Char({
      val c = codeUnits(index)
      if (c >= 0x0000 && c <= 0xD7FF ||
          c >= 0xE000 && c <= 0x10FFFF) c // U+0000..U+D7FF | U+E000..U+10FFFF
      else 0xFFFD
    })
  }
  
  /** Returns a resized copy of this string. */
  private[text] def copy(size: Int): String4 = {
    val newCodeUnits = new scala.Array[Int](size)
    java.lang.System.arraycopy(codeUnits, 0, newCodeUnits, 0, scala.math.min(codeUnits.length, size))
    new String4(newCodeUnits)
  }
  
  override def iterator: CharIterator = new String4Iterator(this, 0)
  
  /** Sequentially applies a function to each code point in this string.
    * Applies the replacement character U+FFFD in lieu of invalid characters. */
  protected override def foreach[U](f: Char => U) {
    var i = 0
    val n = length
    while (i < n) {
      f(this(i))
      i += 1
    }
  }
  
  override def toString: String = {
    val s = new java.lang.StringBuilder
    var i = 0
    val n = length
    while (i < n) {
      s.appendCodePoint(apply(i).codePoint)
      i += 1
    }
    s.toString
  }
}

/** A factory for 32-bit Unicode strings. */
object String4 {
  val empty: String4 = new String4(new scala.Array[Int](0))
  
  def apply(chars: java.lang.CharSequence): String4 = {
    val s = new String4.Buffer
    s.append(chars)
    s.state
  }
  
  implicit def Buffer: String4.Buffer = new String4.Buffer
  
  /** A buffer for 32-bit Unicode strings in the UTF-32 encoding form.
    * Produces only well-formed code unit sequences. */
  final class Buffer extends basis.Buffer[Any, Char] with CharBuffer {
    override type State = String4
    
    private[this] var string: String4 = String4.empty
    
    private[this] var aliased: Boolean = true
    
    private[this] var size: Int = 0
    
    private[this] def expand(base: Int, size: Int): Int = {
      var n = scala.math.max(base, size) - 1
      n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
      n + 1
    }
    
    private[this] def prepare(size: Int) {
      if (aliased || size > string.length) {
        string = string.copy(expand(16, size))
        aliased = false
      }
    }
    
    override def += (char: Char): this.type = {
      val c = char.codePoint
      val n = size
      if ((c >= 0x0000 && c <= 0xD7FF) ||
          (c >= 0xE000 && c <= 0x10FFFF)) { // U+0000..U+D7FF | U+E000..U+FFFF
        prepare(n + 1)
        string.codeUnits(n) = c
        size = n + 1
      }
      else { // invalid code point
        prepare(n + 1)
        string.codeUnits(n) = 0xFFFD
        size = n + 1
      }
      this
    }
    
    override def expect(count: Int): this.type = {
      if (size + count > string.length) {
        string = string.copy(size + count)
        aliased = false
      }
      this
    }
    
    override def state: String4 = {
      if (size != string.length) string = string.copy(size)
      aliased = true
      string
    }
    
    override def clear() {
      string = String4.empty
      aliased = true
      size = 0
    }
  }
}

private[text] final class String4Iterator
    (string: String4, private[this] var index: Int)
  extends CharIterator {
  
  override def isEmpty: Boolean = index >= string.length
  
  override def head: Char = {
    if (isEmpty) throw new scala.NoSuchElementException("head of empty iterator")
    else string(index)
  }
  
  override def step() {
    if (isEmpty) throw new java.lang.UnsupportedOperationException("empty iterator step")
    else index += 1
  }
  
  override def dup: String4Iterator = new String4Iterator(string, index)
}
