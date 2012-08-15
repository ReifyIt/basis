/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.encoding
package utf8

/** An 8-bit Unicode string comprised of a sequence of UTF-8 code units. */
final class String(val codeUnits: Array[Byte]) extends AnyVal with Rope {
  override type Kind = String
  
  /** Returns the number of unsigned 8-bit code units in this Unicode string. */
  @inline override def size: Int = codeUnits.length
  
  /** Returns the unsigned 8-bit code unit at the specified index;
    * '''DOES NOT''' decode a character at that index. */
  @inline override def get(index: Int): Int = codeUnits(index) & 0xFF
  
  /** Returns a copy of this Unicode string. */
  private[utf8] def copy(size: Int): String = {
    val newCodeUnits = new Array[Byte](size)
    System.arraycopy(codeUnits, 0, newCodeUnits, 0, math.min(codeUnits.length, size))
    new String(newCodeUnits)
  }
  
  override def length: Int = {
    var i = 0
    var k = 0
    val n = codeUnits.length
    while (i < n) {
      i = advance(i)
      k += 1
    }
    k
  }
  
  override def apply(index: Int): Int = {
    val n = codeUnits.length
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    val c1 = codeUnits(index) & 0xFF
    if (c1 <= 0x7F) c1 // U+0000..U+007F
    else if (c1 >= 0xC2 && c1 <= 0xF4 && index + 1 < n) {
      val c2 = codeUnits(index + 1) & 0xFF
      if (c1 <= 0xDF && c2 >= 0x80 && c2 <= 0xBF)
        ((c1 & 0x1F) << 6) | (c2 & 0x3F) // U+0080..U+07FF
      else if (index + 2 < n) {
        val c3 = codeUnits(index + 2) & 0xFF
        if ((c1 == 0xE0 &&
             c2 >= 0xA0 && c2 <= 0xBF
          || c1 == 0xED &&
             c2 >= 0x80 && c2 <= 0x9F
          || c1 >= 0xE1 && c1 <= 0xEF &&
             c2 >= 0x80 && c2 <= 0xBF)
          && c3 >= 0x80 && c3 <= 0xBF)
          ((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F) // U+0800..U+FFFF
        else if (index + 3 < n) {
          val c4 = codeUnits(index + 3) & 0xFF
          if ((c1 == 0xF0 &&
               c2 >= 0x90 && c2 <= 0xBF
            || c1 >= 0xF1 && c1 <= 0xF3 &&
               c2 >= 0x80 && c2 <= 0xBF
            || c1 == 0xF4 &&
               c2 >= 0x80 && c2 <= 0x8F)
            && c3 >= 0x80 && c3 <= 0xBF
            && c4 >= 0x80 && c4 <= 0xBF)
            ((c1 & 0x07) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F) // U+10000..U+10FFFF
          else 0xFFFD
        }
        else 0xFFFD
      }
      else 0xFFFD
    }
    else 0xFFFD
  }
  
  override def advance(index: Int): Int = {
    val n = codeUnits.length
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    val c1 = codeUnits(index) & 0xFF
    if (c1 <= 0x7F)
      index + 1 // U+0000..U+007F
    else if (c1 >= 0xC2 && c1 <= 0xF4 && index + 1 < n) {
      val c2 = codeUnits(index + 1) & 0xFF
      if (c1 <= 0xDF && c2 >= 0x80 && c2 <= 0xBF)
        index + 2 // U+0080..U+07FF
      else if (index + 2 < n) {
        val c3 = codeUnits(index + 2) & 0xFF
        if ((c1 == 0xE0 &&
             c2 >= 0xA0 && c2 <= 0xBF
          || c1 == 0xED &&
             c2 >= 0x80 && c2 <= 0x9F
          || c1 >= 0xE1 && c1 <= 0xEF &&
             c2 >= 0x80 && c2 <= 0xBF)
          && c3 >= 0x80 && c3 <= 0xBF)
          index + 3 // U+0800..U+FFFF
        else if (index + 3 < n) {
          val c4 = codeUnits(index + 3) & 0xFF
          if ((c1 == 0xF0 &&
               c2 >= 0x90 && c2 <= 0xBF
            || c1 >= 0xF1 && c1 <= 0xF3 &&
               c2 >= 0x80 && c2 <= 0xBF
            || c1 == 0xF4 &&
               c2 >= 0x80 && c2 <= 0x8F)
            && c3 >= 0x80 && c3 <= 0xBF
            && c4 >= 0x80 && c4 <= 0xBF)
            index + 4 // U+10000..U+10FFFF
          else index + 3
        }
        else index + 2
      }
      else index + 1
    }
    else index + 1
  }
  
  /** Sequentially applies a function to each code point in this Unicode string.
    * Applies the replacement character U+FFFD in lieu of the maximal subpart of
    * any ill-formed subsequences. */
  @inline override def foreach[U](f: Int => U) {
    var i = 0
    val n = size
    while (i < n) {
      f(apply(i))
      i = advance(i)
    }
  }
  
  override def iterator: String.Iterator = new String.Iterator(this, 0)
  
  override def toString: java.lang.String = {
    val s = new java.lang.StringBuilder
    foreach(s.appendCodePoint(_))
    s.toString
  }
}

/** Contains factory methods for 8-bit Unicode strings. */
object String {
  val Empty: String = new String(new Array[Byte](0))
  
  def apply(chars: CharSequence): String = {
    val s = new String.Builder
    s.append(chars)
    s.result
  }
  
  /** Returns a new 8-bit Unicode string builder. */
  implicit def Builder: String.Builder = new String.Builder
  
  /** A pointer to a location in a UTF-8 string. */
  final class Iterator(val string: String, private[this] var index: Int) extends Text.Iterator {
    def offset: Int = index
    
    /** Returns `true` if the current offset begins a valid character. */
    def isValid: Boolean = {
      val c = string(index)
      c >= 0 && c <= 0x10FFFF
    }
    
    /** Decodes the character at the current offset, substituting the
      * replacement character U+FFFD if the offset is unconvertible. */
    def head: Int = string(index)
    
    override def hasNext: Boolean = 0 <= index && index < string.size
    
    override def next(): Int = {
      val c = string(index)
      index = string.advance(index)
      c
    }
  }
  
  /** A builder for 8-bit Unicode strings in the UTF-8 encoding form.
    * Produces only well-formed code unit sequences. */
  final class Builder extends Text.Builder {
    override type Result = String
    
    private[this] var string: String = String.Empty
    
    private[this] var aliased: Boolean = true
    
    private[this] var size: Int = 0
    
    private[this] def prepare(size: Int) {
      if (aliased || size > string.size) {
        string = string.copy(basis.collection.Builder.expand(16, size))
        aliased = false
      }
    }
    
    override def expect(count: Int) {
      if (size + count > string.size) {
        string = string.copy(size + count)
        aliased = false
      }
    }
    
    override def += (codePoint: Int) {
      val n = size
      if (codePoint >= 0x0000 && codePoint <= 0x007F) { // U+0000..U+007F
        prepare(n + 1)
        string.codeUnits(n) = codePoint.toByte
        size = n + 1
      }
      else if (codePoint >= 0x0080 && codePoint <= 0x07FF) { // U+0080..U+07FF
        prepare(n + 2)
        string.codeUnits(n)     = (0xC0 | (codePoint >>> 6)).toByte
        string.codeUnits(n + 1) = (0x80 | (codePoint & 0x3F)).toByte
        size = n + 2
      }
      else if (codePoint >= 0x0800 && codePoint <= 0xFFFF || // U+0800..U+D7FF
               codePoint >= 0xE000 && codePoint <= 0xFFFF) { // U+E000..U+FFFF
        prepare(n + 3)
        string.codeUnits(n)     = (0xE0 | (codePoint  >>> 12)).toByte
        string.codeUnits(n + 1) = (0x80 | ((codePoint >>>  6) & 0x3F)).toByte
        string.codeUnits(n + 2) = (0x80 | (codePoint & 0x3F)).toByte
        size = n + 3
      }
      else if (codePoint >= 0x10000 && codePoint <= 0x10FFFF) { // U+10000..U+10FFFF
        prepare(n + 4)
        string.codeUnits(n)     = (0xF0 | (codePoint  >>> 18)).toByte
        string.codeUnits(n + 1) = (0x80 | ((codePoint >>> 12) & 0x3F)).toByte
        string.codeUnits(n + 2) = (0x80 | ((codePoint >>>  6) & 0x3F)).toByte
        string.codeUnits(n + 3) = (0x80 | (codePoint & 0x3F)).toByte
        size = n + 4
      }
      else { // surrogate or invalid code point
        prepare(n + 3)
        string.codeUnits(n)     = 0xEF.toByte
        string.codeUnits(n + 1) = 0xBF.toByte
        string.codeUnits(n + 2) = 0xBD.toByte
        size = n + 3
      }
    }
    
    override def result: String = {
      if (size != string.size) string = string.copy(size)
      aliased = true
      string
    }
    
    override def clear() {
      string = String.Empty
      aliased = true
      size = 0
    }
  }
}
