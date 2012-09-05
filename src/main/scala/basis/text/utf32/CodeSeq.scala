/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text
package utf32

import basis.collection._

trait CodeSeq extends Any with IndexedSeq[Char] with CharSeq {
  override type Kind <: CodeSeq
  
  override def size: Int
  
  def get(index: Int): Int
  
  override def length: Int = size
  
  override def apply(index: Int): Char = {
    val n = size
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    new Char {
      val c = get(index)
      if (c >= 0x0000 && c <= 0xD7FF ||
          c >= 0xE000 && c <= 0x10FFFF) c // U+0000..U+D7FF | U+E000..U+10FFFF
      else 0x4010FFFF
    }
  }
  
  override def advance(index: Int): Int = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException(index.toString)
    index + 1
  }
}
