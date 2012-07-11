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
object UTF16 extends Encoding {
  /** A Unicode 16-bit string comprised of a sequence of UTF-16 code units. */
  final class String(val codeUnits: Array[Char]) extends AnyVal with Text {
    @inline override def length: Int = codeUnits.length
    @inline override def apply(index: Int): Int = codeUnits(index)
    @inline private[UTF16] def update(index: Int, codeUnit: Int): Unit = codeUnits(index) = codeUnit.toChar
    def copy(length: Int): String = {
      val newCodeUnits = new Array[Char](length)
      Array.copy(codeUnits, 0, newCodeUnits, 0, math.min(codeUnits.length, length))
      new String(newCodeUnits)
    }
  }
  
  /** Contains factory methods for Unicode 16-bit strings. */
  object String extends StringFactory {
    override val Empty = new String(new Array[Char](0))
    override def apply(chars: CharSequence): String = {
      val s = new StringBuilder
      s.append(chars)
      s.result
    }
  }
  
  /** Returns a new Unicode 16-bit string builder. */
  implicit def StringBuilder: StringBuilder = new StringBuilder
  
  /** A UTF-16 code unit sequence. */
  trait Text extends Any with super.Text {
    /** Returns the number of unsigned 16-bit code units in this Unicode string. */
    override def length: Int
    
    /** Returns the unsigned 16-bit code unit at the specified index;
      * '''DOES NOT''' decode surrogate pairs at that index. */
    override def apply(index: Int): Int
    
    /** Sequentially applies a function to each code point of this Unicode string.
      * Applies the replacement character U+FFFD in lieu of unpaired surrogates. */
    override def foreach[@specialized(Unit) U](f: Int => U) {
      var i = 0
      val n = length
      while (i < n) f({
        val c1 = apply(i)
        if (c1 <= 0xD7FF || c1 >= 0xE000) { // c1 >= 0 && c1 <= 0x10000
          // U+0000..U+D7FF | U+E000..U+FFFF
          i += 1 // valid code point
          c1
        }
        else if (c1 <= 0xDBFF) { // c1 >= 0xD800
          // U+10000..U+10FFFF
          i += 1 // valid low surrogate
          if (i < n) {
            val c2 = apply(i)
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
    
    override def iterator: TextIterator = new TextIterator(this, 0)
    
    override def toString: java.lang.String = {
      val s = new java.lang.StringBuilder
      foreach(s.appendCodePoint(_))
      s.toString
    }
  }
  
  /** A pointer to a location in some UTF-16 text. */
  class TextIterator(string: Text, private[this] var index: Int) extends super.TextIterator {
    def offset: Int = index
    
    /** Returns `true` if the current offset begins a valid character. */
    def isValid: Boolean = {
      val i = index
      val n = string.length
      if (0 <= i && i < n) {
        val c1 = string(index)
        if (c1 <= 0xD7FF || c1 >= 0xE000) { // c1 >= 0 && c1 <= 0x10000
          // U+0000..U+D7FF | U+E000..U+FFFF
          true
        }
        else if (i + 1 < n && c1 <= 0xDBFF) { // c1 >= 0xD800
          // U+10000..U+10FFFF
          val c2 = string(i + 1)
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
      val n = string.length
      if (i < 0 || i >= n) throw new IndexOutOfBoundsException(i.toString)
      val c1 = string(i)
      if (c1 <= 0xD7FF || c1 >= 0xE000) { // c1 >= 0 && c1 <= 0x10000
        // U+0000..U+D7FF | U+E000..U+FFFF
        c1
      }
      else if (i + 1 < n && c1 <= 0xDBFF) { // c1 >= 0xD800
        // U+10000..U+10FFFF
        val c2 = string(i + 1)
        if (c2 >= 0xDC00 && c2 <= 0xDFFF)
          (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
        else 0xFFFD // unpaired low surrogate
      }
      else 0xFFFD // unconvertible offset
    }
    
    override def hasNext: Boolean = 0 <= index && index < string.length
    
    override def next(): Int = {
      val n = string.length
      if (0 <= index && index < n) {
        val c1 = string(index)
        if (c1 <= 0xD7FF || c1 >= 0xE000) { // c1 >= 0 && c1 <= 0x10000
          // U+0000..U+D7FF | U+E000..U+FFFF
          index += 1 // valid code point
          c1
        }
        else if (c1 <= 0xDBFF) { // c1 >= 0xD800
          // U+10000..U+10FFFF
          index += 1 // valid low surrogate
          if (index < n) {
            val c2 = string(index)
            if (c2 >= 0xDC00 && c2 <= 0xDFFF) {
              index += 1 // valid high surrogate
              (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
            }
            else 0xFFFD // unpaired low surrogate
          }
          else 0xFFFD // missing high surrogate
        }
        else { // c1 >= 0xDC00 && c1 <= 0xDFFF
          index += 1
          0xFFFD // unpaird high surrogate
        }
      }
      else Iterator.Empty.next() // end of sequence
    }
  }
  
  /** A builder for Unicode 16-bit strings in the UTF-16 encoding form.
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
}
