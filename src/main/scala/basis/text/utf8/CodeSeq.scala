/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text
package utf8

trait CodeSeq extends Any with CharSeq {
  override type Kind <: CodeSeq
  
  override def size: Int
  
  def get(index: Int): Int
  
  override def apply(index: Int): Char = {
    val n = size
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    new Char {
      val c1 = get(index)
      if (c1 <= 0x7F) c1 // U+0000..U+007F
      else if (c1 >= 0xC2 && c1 <= 0xF4 && index + 1 < n) {
        val c2 = get(index + 1)
        if (c1 <= 0xDF && c2 >= 0x80 && c2 <= 0xBF)
          ((c1 & 0x1F) << 6) | (c2 & 0x3F) // U+0080..U+07FF
        else if (index + 2 < n) {
          val c3 = get(index + 2)
          if ((c1 == 0xE0 &&
               c2 >= 0xA0 && c2 <= 0xBF
            || c1 == 0xED &&
               c2 >= 0x80 && c2 <= 0x9F
            || c1 >= 0xE1 && c1 <= 0xEF &&
               c2 >= 0x80 && c2 <= 0xBF)
            && c3 >= 0x80 && c3 <= 0xBF)
            ((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F) // U+0800..U+FFFF
          else if (index + 3 < n) {
            val c4 = get(index + 3)
            if ((c1 == 0xF0 &&
                 c2 >= 0x90 && c2 <= 0xBF
              || c1 >= 0xF1 && c1 <= 0xF3 &&
                 c2 >= 0x80 && c2 <= 0xBF
              || c1 == 0xF4 &&
                 c2 >= 0x80 && c2 <= 0x8F)
              && c3 >= 0x80 && c3 <= 0xBF
              && c4 >= 0x80 && c4 <= 0xBF)
              ((c1 & 0x07) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F) // U+10000..U+10FFFF
            else 0x1010FFFF
          }
          else 0x2010FFFF
        }
        else 0x4010FFFF
      }
      else 0x4010FFFF
    }
  }
  
  override def advance(index: Int): Int = {
    val n = size
    if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
    val c1 = get(index)
    if (c1 <= 0x7F)
      index + 1 // U+0000..U+007F
    else if (c1 >= 0xC2 && c1 <= 0xF4 && index + 1 < n) {
      val c2 = get(index + 1)
      if (c1 <= 0xDF && c2 >= 0x80 && c2 <= 0xBF)
        index + 2 // U+0080..U+07FF
      else if (index + 2 < n) {
        val c3 = get(index + 2)
        if ((c1 == 0xE0 &&
             c2 >= 0xA0 && c2 <= 0xBF
          || c1 == 0xED &&
             c2 >= 0x80 && c2 <= 0x9F
          || c1 >= 0xE1 && c1 <= 0xEF &&
             c2 >= 0x80 && c2 <= 0xBF)
          && c3 >= 0x80 && c3 <= 0xBF)
          index + 3 // U+0800..U+FFFF
        else if (index + 3 < n) {
          val c4 = get(index + 3)
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
}
