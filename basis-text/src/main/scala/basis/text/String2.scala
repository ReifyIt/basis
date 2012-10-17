/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

/** A 16-bit Unicode string comprised of a UTF-16 code unit sequence.
  * 
  * @author Chris Sachs
  * 
  * @define collection  string
  */
class String2(val codeUnits: scala.Array[scala.Char]) extends AnyVal with Rope {
  override type Self = String2
  
  override def isEmpty: Boolean = codeUnits.length == 0
  
  /** Counts the number of code points in this string. */
  override def length: Int = {
    var i = 0
    var l = 0
    val n = size
    while (i < n) {
      i = nextIndex(i)
      l += 1
    }
    l
  }
  
  /** Returns the number of unsigned 16-bit code units in this string. */
  def size: Int = codeUnits.length
  
  /** Returns a decoded character beginning at `index`. Substitutes the
    * replacement character U+FFFD at invalid indexes. */
  def apply(index: Int): Char = {
    val n = codeUnits.length
    if (index < 0 || index >= n)
      throw new java.lang.IndexOutOfBoundsException(index.toString)
    new Char({
      val c1 = codeUnits(index)
      if (c1 <= 0xD7FF || c1 >= 0xE000) c1 // U+0000..U+D7FF | U+E000..U+FFFF
      else if (c1 <= 0xDBFF && index + 1 < n) { // c1 >= 0xD800
        val c2 = codeUnits(index + 1)
        if (c2 >= 0xDC00 && c2 <= 0xDFFF) // U+10000..U+10FFFF
          (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
        else 0xFFFD
      }
      else 0xFFFD
    })
  }
  
  def nextIndex(index: Int): Int = {
    val n = codeUnits.length
    if (index < 0 || index >= n)
      throw new java.lang.IndexOutOfBoundsException(index.toString)
    val c1 = codeUnits(index)
    if (c1 <= 0xD7FF || c1 >= 0xE000) // U+0000..U+D7FF | U+E000..U+FFFF
      index + 1
    else if (c1 <= 0xDBFF && index + 1 < n) { // c1 >= 0xD800
      val c2 = codeUnits(index + 1)
      if (c2 >= 0xDC00 && c2 <= 0xDFFF) // U+10000..U+10FFFF
        index + 2
      else index + 1
    }
    else index + 1
  }
  
  /** Returns a resized copy of this string. */
  private[text] def copy(size: Int): String2 = {
    val newCodeUnits = new scala.Array[scala.Char](size)
    java.lang.System.arraycopy(codeUnits, 0, newCodeUnits, 0, scala.math.min(codeUnits.length, size))
    new String2(newCodeUnits)
  }
  
  override def iterator: CharIterator = new String2Iterator(this, 0)
  
  /** Sequentially applies a function to each code point in this string.
    * Applies the replacement character U+FFFD in lieu of unpaired surrogates. */
  protected override def foreach[U](f: Char => U) {
    var i = 0
    val n = codeUnits.length
    while (i < n) f(new Char({
      val c1 = codeUnits(i).toInt
      i += 1
      if (c1 <= 0xD7FF || c1 >= 0xE000) c1 // U+0000..U+D7FF | U+E000..U+FFFF
      else if (c1 <= 0xDBFF && i < n) { // c1 >= 0xD800
        val c2 = codeUnits(i).toInt
        if (c2 >= 0xDC00 && c2 <= 0xDFFF) { // U+10000..U+10FFFF
          i += 1
          (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
        }
        else 0xFFFD
      }
      else 0xFFFD
    }))
  }
  
  override def toString: String = {
    val s = new java.lang.StringBuilder
    var i = 0
    val n = size
    while (i < n) {
      s.appendCodePoint(apply(i).codePoint)
      i = nextIndex(i)
    }
    s.toString
  }
}

/** A factory for 16-bit Unicode strings. */
object String2 {
  val empty: String2 = new String2(new scala.Array[scala.Char](0))
  
  def apply(chars: java.lang.CharSequence): String2 = {
    val s = new String2Buffer
    s.append(chars)
    s.state
  }
  
  implicit def Buffer: String2Buffer = new String2Buffer
}

private[text] final class String2Iterator
    (string: String2, private[this] var index: Int)
  extends CharIterator {
  
  override def isEmpty: Boolean = index >= string.size
  
  override def head: Char = {
    if (isEmpty) throw new scala.NoSuchElementException("head of empty iterator")
    else string(index)
  }
  
  override def step() {
    if (isEmpty) throw new java.lang.UnsupportedOperationException("empty iterator step")
    else index = string.nextIndex(index)
  }
  
  override def dup: String2Iterator = new String2Iterator(string, index)
}

/** A buffer for 16-bit Unicode strings in the UTF-16 encoding form.
  * Produces only well-formed code unit sequences. */
final class String2Buffer extends Buffer[Any, Char] with CharBuffer {
  override type State = String2
  
  private[this] var string: String2 = String2.empty
  
  private[this] var aliased: Boolean = true
  
  private[this] var size: Int = 0
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = scala.math.max(base, size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def prepare(size: Int) {
    if (aliased || size > string.size) {
      string = string.copy(expand(16, size))
      aliased = false
    }
  }
  
  override def += (char: Char): this.type = {
    val c = char.codePoint
    val n = size
    if ((c >= 0x0000 && c <= 0xD7FF) ||
        (c >= 0xE000 && c <= 0xFFFF)) { // U+0000..U+D7FF | U+E000..U+FFFF
      prepare(n + 1)
      string.codeUnits(n) = c.toChar
      size = n + 1
    }
    else if (c >= 0x10000 && c <= 0x10FFFF) { // U+10000..U+10FFFF
      prepare(n + 2)
      val u = c - 0x10000
      string.codeUnits(n)     = (0xD800 | (u >>> 10)).toChar
      string.codeUnits(n + 1) = (0xDC00 | (u & 0x3FF)).toChar
      size = n + 2
    }
    else { // invalid code point
      prepare(n + 1)
      string.codeUnits(n) = 0xFFFD.toChar
      size = n + 1
    }
    this
  }
  
  override def expect(count: Int): this.type = {
    if (size + count > string.size) {
      string = string.copy(size + count)
      aliased = false
    }
    this
  }
  
  override def state: String2 = {
    if (size != string.size) string = string.copy(size)
    aliased = true
    string
  }
  
  override def clear() {
    string = String2.empty
    aliased = true
    size = 0
  }
}
