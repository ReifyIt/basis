/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis.collection._

/** A Unicode® endocing form.
  * 
  * @author Chris Sachs
  */
trait Unicode {
  /** A Unicode string in this enocoding form. */
  type String <: Text
  
  /** Contains factory methods for Unicode strings. */
  def String: StringFactory
  
  /** Returns a new Unicode string builder. */
  implicit def StringBuilder: StringBuilder
  
  /** A Unicode code unit sequence in this encoding form. */
  trait Text extends Any with Iterated[Int] {
    override type Scope <: Unicode.this.type
    
    /** Returns the number of code units in the Unicode text. */
    def length: Int
    
    /** Returns the code unit at the specified index;
      * '''DOES NOT''' decode a character at that index. */
    def apply(index: Int): Int
    
    /** Sequentially applies a function to each code point in the Unicode text.
      * Implementations define the handling of ill-formed code unit sequences. */
    override def foreach[@specialized(Unit) U](f: Int => U): Unit
    
    /** Returns an iterator over the code points of the Unicode text. */
    override def iterator: TextIterator
  }
  
  /** A pointer to a location in some Unicode text. */
  trait TextIterator extends Any with Iterator[Int]
  
  /** A factory for Unicode strings. */
  trait StringFactory {
    /** Returns the empty Unicode string. */
    def Empty: String
    
    /** Returns a new Unicode string comprised of the valid code points in the
      * given UTF-16 character sequence. Substitutes the replacement character
      * U+FFFD for any unpaired surrogates. */
    def apply(chars: CharSequence): String
  }
  
  /** A builder for well-formed Unicode strings. */
  trait StringBuilder extends Collector[Unicode.this.type, Int] {
    override type Product = String
    
    /** Tells the builder to expect `count` code units. */
    override def expect(count: Int): Unit
    
    /** Appends a Unicode scalar value to the builder. A scalar value consists
      * of any valid code point except the high and low surrogates, U+D800..U+DBFF
      * and U+DC00..U+DFFF, respectively. If `codePoint` does not lie in either
      * the range U+0000..U+D7FF or U+E000..U+10FFFF, inclusive, then the
      * replacement character U+FFFD is appended in its place. */
    override def += (codePoint: Int): Unit
    
    /** Appends a UTF-16 character sequence to the builder. */
    def append(chars: CharSequence) {
      val n = chars.length
      var i = 0
      while (i < n) this += {
        val c1 = chars.charAt(i)
        if (c1 <= 0xD7FF || c1 >= 0xE000) { // c1 >= 0 && c1 <= 0x10000
          // U+0000..U+D7FF | U+E000..U+FFFF
          i += 1 // valid code point
          c1
        }
        else if (c1 <= 0xDBFF) { // c1 >= 0xD800
          // U+10000..U+10FFFF
          i += 1 // valid low surrogate
          if (i < n) {
            val c2 = chars.charAt(i)
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
          0xFFFD // unpaired high surrogate
        }
      }
    }
    
    /** Returns a string containing the accumulated characters. */
    override def result: String
    
    /** Resets the builder to its empty state. */
    override def clear(): Unit
  }
}
