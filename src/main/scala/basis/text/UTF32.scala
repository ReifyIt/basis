/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis.collection._

/** The Unicode® UTF-32 endocing form.
  * 
  * @author Chris Sachs
  */
object UTF32 extends Unicode {
  /** A 32-bit Unicode string comprised of a sequence of UTF-32 code units. */
  final class String(val codeUnits: Array[Int]) extends AnyVal with Indexed[Int] with Text {
    /** Returns the number of 32-bit code units in this Unicode string. */
    @inline override def length: Int = codeUnits.length
    
    /** Returns the 32-bit code unit at the specified index. */
    @inline override def apply(index: Int): Int = codeUnits(index)
    
    /** Updates a 32-bit code unit of this Unicode string. */
    @inline private[UTF32] def update(index: Int, codeUnit: Int): Unit = codeUnits(index) = codeUnit
    
    /** Returns a copy of this Unicode string. */
    def copy(length: Int): String = {
      val newCodeUnits = new Array[Int](length)
      Array.copy(codeUnits, 0, newCodeUnits, 0, math.min(codeUnits.length, length))
      new String(newCodeUnits)
    }
    
    /** Sequentially applies a function to each code point in this Unicode string.
      * Applies the replacement character U+FFFD in lieu of invalid characters. */
    override def foreach[@specialized(Unit) U](f: Int => U) {
      var i = 0
      val cs = codeUnits
      val n = cs.length
      while (i < n) f({
        val c = cs(i)
        if ((c >= 0x000000 && c <= 0x00D7FF) ||
            (c >= 0x00E000 && c <= 0x10FFFF)) {
          // U+0000..U+D7FF | U+E000..U+10FFFF
          i += 1 // valid code point
          c
        }
        else { // (c >= 0xD800 && c <= 0xDFFF) || c >= 0x110000
          i += 1
          0xFFFD // invalid code point
        }
      }: Int) // ascribe Int to defer boxing for unspecialized functions
    }
    
    override def iterator: StringIterator = new StringIterator(this, 0)
    
    override def toString: java.lang.String = {
      val s = new java.lang.StringBuilder
      foreach(s.appendCodePoint(_))
      s.toString
    }
  }
  
  /** Contains factory methods for 32-bit Unicode strings. */
  object String extends StringFactory {
    override val Empty = new String(new Array[Int](0))
    override def apply(chars: CharSequence): String = {
      val s = new StringBuilder
      s.append(chars)
      s.result
    }
  }
  
  /** A pointer to a location in a UTF-32 string. */
  final class StringIterator(val string: String, private[this] var index: Int) extends TextIterator {
    def offset: Int = index
    
    /** Returns `true` if the current offset represents a valid character. */
    def isValid: Boolean = {
      val i = index
      val cs = string.codeUnits
      val n = cs.length
      if (0 <= i && i < n) {
        val c = cs(i)
        (c >= 0x000000 && c <= 0x00D7FF) ||
        (c >= 0x00E000 && c <= 0x10FFFF)
      }
      else false // out of bounds
    }
    
    /** Decodes the character at the current offset, substituting the
      * replacement character U+FFFD if the offset is unconvertible. */
    def head: Int = {
      val i = index
      val cs = string.codeUnits
      val n = cs.length
      if (i < 0 || i >= n) throw new NoSuchElementException("head of empty string iterator")
      val c = cs(i)
      if ((c >= 0x000000 && c <= 0x00D7FF) ||
          (c >= 0x00E000 && c <= 0x10FFFF)) {
        // U+0000..U+D7FF | U+E000..U+10FFFF
        c
      }
      else 0xFFFD // unconvertible offset
    }
    
    override def hasNext: Boolean = 0 <= index && index < string.length
    
    override def next(): Int = {
      var i = index
      val cs = string.codeUnits
      val n = cs.length
      val c = if (0 <= i && i < n) {
        val c = cs(i)
        if ((c >= 0x000000 && c <= 0x00D7FF) ||
            (c >= 0x00E000 && c <= 0x10FFFF)) {
          // U+0000..U+D7FF | U+E000..U+10FFFF
          i += 1 // valid code point
          c
        }
        else { // (c >= 0xD800 && c <= 0xDFFF) || c >= 0x110000
          i += 1
          0xFFFD // invalid code point
        }
      }
      else throw new NoSuchElementException("empty string iterator")
      index = i
      c
    }
  }
  
  /** A builder for 32-bit Unicode strings in the UTF-32 encoding form.
    * Produces only well-formed code unit sequences. */
  final class StringBuilder extends super.StringBuilder {
    private[this] var string: String = String.Empty
    private[this] var aliased: Boolean = true
    private[this] var length: Int = 0
    
    private[this] def prepare(size: Int): Unit = if (aliased || size > string.length) {
      string = string.copy(Collector.nextSize(16, size))
      aliased = false
    }
    
    override def expect(count: Int): Unit = if (length + count > string.length) {
      string = string.copy(length + count)
      aliased = false
    }
    
    override def += (codePoint: Int) {
      val n = length
      if ((codePoint >= 0x000000 && codePoint <= 0x00D7FF) ||
          (codePoint >= 0x00E000 && codePoint <= 0x10FFFF)) {
        // U+0000..U+D7FF | U+E000..U+FFFF
        prepare(n + 1)
        string(n) = codePoint
        length = n + 1
      }
      else { // invalid code point; encode the replacement character U+FFFD
        prepare(n + 1)
        string(n) = 0xFFFD
        length = n + 1
      }
    }
    
    override def result: String = {
      if (length != string.length) string = string.copy(length)
      aliased = true
      string
    }
    
    override def clear() {
      string = String.Empty
      aliased = true
      length = 0
    }
  }
  
  /** Returns a new 32-bit Unicode string builder. */
  implicit def StringBuilder: StringBuilder = new StringBuilder
}
