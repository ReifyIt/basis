/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis.collection._

/** The Unicode® UTF-16 endocing form.
  * 
  * @author Chris Sachs
  */
object UTF16 extends Unicode {
  /** A 16-bit Unicode string comprised of a sequence of UTF-16 code units. */
  final class String(val codeUnits: Array[Char]) extends AnyVal with Text {
    /** Returns the number of unsigned 16-bit code units in this Unicode string. */
    @inline override def length: Int = codeUnits.length
    
    /** Returns the unsigned 16-bit code unit at the specified index;
      * '''DOES NOT''' decode surrogate pairs at that index. */
    @inline override def apply(index: Int): Int = codeUnits(index)
    
    /** Updates an unsigned 16-bit code unit of this Unicode string. */
    @inline private[UTF16] def update(index: Int, codeUnit: Int): Unit = codeUnits(index) = codeUnit.toChar
    
    /** Returns a copy of this Unicode string. */
    def copy(length: Int): String = {
      val newCodeUnits = new Array[Char](length)
      Array.copy(codeUnits, 0, newCodeUnits, 0, math.min(codeUnits.length, length))
      new String(newCodeUnits)
    }
    
    /** Sequentially applies a function to each code point in this Unicode string.
      * Applies the replacement character U+FFFD in lieu of unpaired surrogates. */
    override def foreach[@specialized(Unit) U](f: Int => U) {
      var i = 0
      val cs = codeUnits
      val n = cs.length
      while (i < n) f({
        val c1 = cs(i)
        if (c1 <= 0xD7FF || c1 >= 0xE000) { // c1 >= 0 && c1 <= 0x10000
          // U+0000..U+D7FF | U+E000..U+FFFF
          i += 1 // valid code point
          c1
        }
        else if (c1 <= 0xDBFF) { // c1 >= 0xD800
          // U+10000..U+10FFFF
          i += 1 // valid low surrogate
          if (i < n) {
            val c2 = cs(i)
            if (c2 >= 0xDC00 && c2 <= 0xDFFF) {
              i += 1 // valid high surrogate
              (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
            }
            else 0xFFFD // unpaired low surrogate
          }
          else 0xFFFD // missing high surrogate
        }
        else { // c1 >= 0xDC00 && c1 <= 0xDFFF
          i += 1
          0xFFFD // unpaird high surrogate
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
  
  /** Contains factory methods for 16-bit Unicode strings. */
  object String extends StringFactory {
    override val Empty = new String(new Array[Char](0))
    override def apply(chars: CharSequence): String = {
      val s = new StringBuilder
      s.append(chars)
      s.result
    }
  }
  
  /** A pointer to a location in a UTF-32 string. */
  final class StringIterator(val string: String, private[this] var index: Int) extends TextIterator {
    def offset: Int = index
    
    /** Returns `true` if the current offset begins a valid character. */
    def isValid: Boolean = {
      val i = index
      val cs = string.codeUnits
      val n = cs.length
      if (0 <= i && i < n) {
        val c1 = cs(i)
        if (c1 <= 0xD7FF || c1 >= 0xE000) { // c1 >= 0 && c1 <= 0x10000
          // U+0000..U+D7FF | U+E000..U+FFFF
          true
        }
        else if (i + 1 < n && c1 <= 0xDBFF) { // c1 >= 0xD800
          // U+10000..U+10FFFF
          val c2 = cs(i + 1)
          c2 >= 0xDC00 && c2 <= 0xDFFF
        }
        else false // unconvertible offset
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
      val c1 = cs(i)
      if (c1 <= 0xD7FF || c1 >= 0xE000) { // c1 >= 0 && c1 <= 0x10000
        // U+0000..U+D7FF | U+E000..U+FFFF
        c1
      }
      else if (i + 1 < n && c1 <= 0xDBFF) { // c1 >= 0xD800
        // U+10000..U+10FFFF
        val c2 = cs(i + 1)
        if (c2 >= 0xDC00 && c2 <= 0xDFFF)
          (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
        else 0xFFFD // unpaired low surrogate
      }
      else 0xFFFD // unconvertible offset
    }
    
    override def hasNext: Boolean = 0 <= index && index < string.length
    
    override def next(): Int = {
      var i = index
      val cs = string.codeUnits
      val n = cs.length
      val c = if (0 <= i && i < n) {
        val c1 = cs(i)
        if (c1 <= 0xD7FF || c1 >= 0xE000) { // c1 >= 0 && c1 <= 0x10000
          // U+0000..U+D7FF | U+E000..U+FFFF
          i += 1 // valid code point
          c1
        }
        else if (c1 <= 0xDBFF) { // c1 >= 0xD800
          // U+10000..U+10FFFF
          i += 1 // valid low surrogate
          if (i < n) {
            val c2 = cs(i)
            if (c2 >= 0xDC00 && c2 <= 0xDFFF) {
              i += 1 // valid high surrogate
              (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
            }
            else 0xFFFD // unpaired low surrogate
          }
          else 0xFFFD // missing high surrogate
        }
        else { // c1 >= 0xDC00 && c1 <= 0xDFFF
          i += 1
          0xFFFD // unpaird high surrogate
        }
      }
      else throw new NoSuchElementException("empty string iterator")
      index = i
      c
    }
  }
  
  /** A builder for 16-bit Unicode strings in the UTF-16 encoding form.
    * Produces only well-formed code unit sequences. */
  final class StringBuilder extends super.StringBuilder {
    private[this] var string: String = String.Empty
    private[this] var aliased: Boolean = true
    private[this] var length: Int = 0
    
    private[this] def prepare(size: Int): Unit = if (aliased || size > string.length) {
      string = string.copy(Make.nextSize(16, size))
      aliased = false
    }
    
    override def expect(count: Int): Unit = if (length + count > string.length) {
      string = string.copy(length + count)
      aliased = false
    }
    
    override def += (codePoint: Int) {
      val n = length
      if ((codePoint >= 0x0000 && codePoint <= 0xD7FF) ||
          (codePoint >= 0xE000 && codePoint <= 0xFFFF)) {
        // U+0000..U+D7FF | U+E000..U+FFFF
        prepare(n + 1)
        string(n) = codePoint
        length = n + 1
      }
      else if (codePoint >= 0x10000 && codePoint <= 0x10FFFF) {
        // U+10000..U+10FFFF; encode a surrogate pair
        prepare(n + 2)
        val u = codePoint - 0x10000
        string(n)     = 0xD800 | (u >>> 10)
        string(n + 1) = 0xDC00 | (u & 0x3FF)
        length = n + 2
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
  
  /** Returns a new 16-bit Unicode string builder. */
  implicit def StringBuilder: StringBuilder = new StringBuilder
}
