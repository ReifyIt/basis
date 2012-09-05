/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text
package utf16

trait CodeSeq extends Any with CharSeq {
  override type Kind <: CodeSeq
  
  override def size: Int
  
  def get(index: Int): Int
  
  override def apply(index: Int): Char = {
    val n = size
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    new Char {
      val c1 = get(index)
      if (c1 <= 0xD7FF || c1 >= 0xE000) c1 // U+0000..U+D7FF | U+E000..U+FFFF
      else if (c1 <= 0xDBFF && index + 1 < n) { // c1 >= 0xD800
        val c2 = get(index + 1)
        if (c2 >= 0xDC00 && c2 <= 0xDFFF) // U+10000..U+10FFFF
          (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
        else 0x4010FFFF
      }
      else 0x4010FFFF
    }
  }
  
  override def advance(index: Int): Int = {
    val n = size
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    val c1 = get(index)
    if (c1 <= 0xD7FF || c1 >= 0xE000)
      index + 1 // U+0000..U+D7FF | U+E000..U+FFFF
    else if (c1 <= 0xDBFF && index + 1 < n) { // c1 >= 0xD800
      val c2 = get(index + 1)
      if (c2 >= 0xDC00 && c2 <= 0xDFFF)
        index + 2 // U+10000..U+10FFFF
      else index + 1
    }
    else index + 1
  }
}
