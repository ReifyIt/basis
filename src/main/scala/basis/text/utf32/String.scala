/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text
package utf32

/** A 32-bit Unicode string comprised of a sequence of UTF-32 code units. */
final class String(val codeUnits: Array[Int]) extends AnyVal with CodeSeq with Rope {
  override type Kind = String
  
  /** Returns the number of 32-bit code units in this Unicode string. */
  @inline override def size: Int = codeUnits.length
  
  /** Returns the 32-bit code unit at the specified index. */
  @inline override def get(index: Int): Int = codeUnits(index)
  
  /** Returns a copy of this Unicode string. */
  private[utf32] def copy(size: Int): String = {
    val newCodeUnits = new Array[Int](size)
    Array.copy(codeUnits, 0, newCodeUnits, 0, math.min(codeUnits.length, size))
    new String(newCodeUnits)
  }
  
  @inline override def length: Int = codeUnits.length
  
  override def apply(index: Int): Char = {
    val n = codeUnits.length
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    new Char {
      val c = codeUnits(index)
      if (c >= 0x0000 && c <= 0xD7FF ||
          c >= 0xE000 && c <= 0x10FFFF) c // U+0000..U+D7FF | U+E000..U+10FFFF
      else 0x4010FFFF
    }
  }
  
  override def advance(index: Int): Int = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException(index.toString)
    index + 1
  }
  
  /** Sequentially applies a function to each code point in this Unicode string.
    * Applies the replacement character U+FFFD in lieu of invalid characters. */
  @inline override def foreach[U](f: Char => U) {
    var i = 0
    val n = size
    while (i < n) {
      f(apply(i))
      i += 1
    }
  }
  
  override def iterator: String.Iterator = new String.Iterator(this, 0)
  
  override def toString: java.lang.String = {
    val s = new java.lang.StringBuilder
    foreach(s.appendCodePoint(_))
    s.toString
  }
}

/** Contains factory methods for 32-bit Unicode strings. */
object String {
  val Empty: String = new String(new Array[Int](0))
  
  def apply(chars: CharSequence): String = {
    val s = new String.Builder
    s.append(chars)
    s.result
  }
  
  /** Returns a new 32-bit Unicode string builder. */
  implicit def Builder: String.Builder = new String.Builder
  
  /** A pointer to a location in a UTF-32 string. */
  final class Iterator(val string: String, private[this] var index: Int) extends Text.Iterator {
    def offset: Int = index
    
    /** Returns `true` if the current offset begins a valid character. */
    def isValid: Boolean = {
      val c = string(index)
      c >= 0 && c <= 0x10FFFF
    }
    
    /** Decodes the character at the current offset, substituting the
      * replacement character U+FFFD if the offset is unconvertible. */
    def head: Char = string(index)
    
    override def hasNext: Boolean = 0 <= index && index < string.size
    
    override def next(): Char = {
      val c = string(index)
      index += 1
      c
    }
  }
  
  /** A builder for 32-bit Unicode strings in the UTF-32 encoding form.
    * Produces only well-formed code unit sequences. */
  final class Builder extends Text.Builder {
    override type Result = String
    
    private[this] var string: String = String.Empty
    
    private[this] var aliased: Boolean = true
    
    private[this] var size: Int = 0
    
    private[this] def prepare(size: Int): Unit = if (aliased || size > string.size) {
      string = string.copy(basis.collection.Builder.expand(16, size))
      aliased = false
    }
    
    override def expect(count: Int): Unit = if (size + count > string.size) {
      string = string.copy(size + count)
      aliased = false
    }
    
    override def += (codePoint: Char) {
      val n = size
      if ((codePoint >= 0x0000 && codePoint <= 0xD7FF) ||
          (codePoint >= 0xE000 && codePoint <= 0x10FFFF)) { // U+0000..U+D7FF | U+E000..U+FFFF
        prepare(n + 1)
        string.codeUnits(n) = codePoint
        size = n + 1
      }
      else { // invalid code point
        prepare(n + 1)
        string.codeUnits(n) = 0xFFFD
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
