/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.encoding

trait Text extends Any with basis.collection.Seq[Int] {
  override type Kind <: Text
  
  def size: Int
  
  def apply(index: Int): Int
  
  def advance(index: Int): Int
  
  def length: Int = {
    var i = 0
    var k = 0
    val n = size
    while (i < n) {
      i = advance(i)
      k += 1
    }
    k
  }
  
  override def foreach[U](f: Int => U) {
    var i = 0
    val n = size
    while (i < n) {
      f(apply(i))
      i = advance(i)
    }
  }
}

object Text {
  import scala.language.implicitConversions
  
  @inline implicit def TextOps(self: Text): TextOps[self.Kind] =
    new TextOps[self.Kind](self)
  
  trait Iterator extends Any with basis.collection.Iterator[Int] {
    override def hasNext: Boolean
    override def next(): Int
  }
  
  trait Builder extends Any with basis.collection.Builder[Any, Int] {
    /** Tells the builder to expect `count` code points. */
    override def expect(count: Int): Unit
    
    /** Appends a Unicode scalar value to the builder. A scalar value consists
      * of any valid code point except the high and low surrogates, U+D800..U+DBFF
      * and U+DC00..U+DFFF, respectively. If `codePoint` does not lie in either
      * the range U+0000..U+D7FF or U+E000..U+10FFFF, inclusive, then the
      * replacement character U+FFFD is appended in its place. */
    override def += (codePoint: Int): Unit
    
    /** Appends a UTF-16 character sequence to this builder. */
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
  }
}
