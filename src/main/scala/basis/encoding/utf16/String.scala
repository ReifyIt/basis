/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.encoding
package utf16

/** A 16-bit Unicode string comprised of a sequence of UTF-16 code units. */
final class String(val codeUnits: Array[Char]) extends AnyVal with Rope {
  override type Kind = String
  
  /** Returns the number of unsigned 16-bit code units in this Unicode string. */
  @inline override def size: Int = codeUnits.length
  
  /** Returns the unsigned 16-bit code unit at the specified index;
    * '''DOES NOT''' decode surrogate pairs at that index. */
  @inline override def get(index: Int): Int = codeUnits(index)
  
  /** Returns a copy of this Unicode string. */
  private[utf16] def copy(size: Int): String = {
    val newCodeUnits = new Array[Char](size)
    Array.copy(codeUnits, 0, newCodeUnits, 0, math.min(codeUnits.length, size))
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
    val c1 = codeUnits(index)
    if (c1 <= 0xD7FF || c1 >= 0xE000) c1 // U+0000..U+D7FF | U+E000..U+FFFF
    else if (c1 <= 0xDBFF && index + 1 < n) { // c1 >= 0xD800
      val c2 = codeUnits(index + 1)
      if (c2 >= 0xDC00 && c2 <= 0xDFFF) // U+10000..U+10FFFF
        (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
      else 0xFFFD
    }
    else 0xFFFD
  }
  
  override def advance(index: Int): Int = {
    val n = codeUnits.length
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    val c1 = codeUnits(index)
    if (c1 <= 0xD7FF || c1 >= 0xE000)
      index + 1 // U+0000..U+D7FF | U+E000..U+FFFF
    else if (c1 <= 0xDBFF && index + 1 < n) { // c1 >= 0xD800
      val c2 = codeUnits(index + 1)
      if (c2 >= 0xDC00 && c2 <= 0xDFFF)
        index + 2 // U+10000..U+10FFFF
      else index + 1
    }
    else index + 1
  }
  
  /** Sequentially applies a function to each code point in this Unicode string.
    * Applies the replacement character U+FFFD in lieu of unpaired surrogates. */
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

/** Contains factory methods for 16-bit Unicode strings. */
object String {
  val Empty: String = new String(new Array[Char](0))
  
  def apply(chars: CharSequence): String = {
    val s = new String.Builder
    s.append(chars)
    s.result
  }
  
  /** Returns a new 16-bit Unicode string builder. */
  implicit def Builder: String.Builder = new String.Builder
  
  /** A pointer to a location in a UTF-16 string. */
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
  
  /** A builder for 16-bit Unicode strings in the UTF-16 encoding form.
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
      if ((codePoint >= 0x0000 && codePoint <= 0xD7FF) ||
          (codePoint >= 0xE000 && codePoint <= 0xFFFF)) { // U+0000..U+D7FF | U+E000..U+FFFF
        prepare(n + 1)
        string.codeUnits(n) = codePoint.toChar
        size = n + 1
      }
      else if (codePoint >= 0x10000 && codePoint <= 0x10FFFF) { // U+10000..U+10FFFF
        prepare(n + 2)
        val u = codePoint - 0x10000
        string.codeUnits(n)     = (0xD800 | (u >>> 10)).toChar
        string.codeUnits(n + 1) = (0xDC00 | (u & 0x3FF)).toChar
        size = n + 2
      }
      else { // invalid code point
        prepare(n + 1)
        string.codeUnits(n) = 0xFFFD.toChar
        size = n + 1
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
