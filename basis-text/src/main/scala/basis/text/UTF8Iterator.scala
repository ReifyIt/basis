/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

private[text] final class UTF8Iterator
    (string: UTF8String, private[this] var index: Int)
  extends StringIterator {
  
  override def hasNext: Boolean = index < string.codeUnits.length
  
  override def next(): Char = {
    val cs = string.codeUnits
    val n = cs.length
    if (index >= n) throw new scala.NoSuchElementException("empty string iterator")
    new Char({
      val c1 = cs(index) & 0xFF
      if (c1 <= 0x7F) {
        index += 1
        c1 // U+0000..U+007F
      }
      else if (c1 >= 0xC2 && c1 <= 0xF4 && index + 1 < n) {
        val c2 = cs(index + 1) & 0xFF
        if (c1 <= 0xDF && c2 >= 0x80 && c2 <= 0xBF) {
          index += 2
          ((c1 & 0x1F) << 6) | (c2 & 0x3F) // U+0080..U+07FF
        }
        else if (index + 2 < n) {
          val c3 = cs(index + 2) & 0xFF
          if ((c1 == 0xE0 &&
               c2 >= 0xA0 && c2 <= 0xBF
            || c1 == 0xED &&
               c2 >= 0x80 && c2 <= 0x9F
            || c1 >= 0xE1 && c1 <= 0xEF &&
               c2 >= 0x80 && c2 <= 0xBF)
            && c3 >= 0x80 && c3 <= 0xBF) {
            index += 3
            ((c1 & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F) // U+0800..U+FFFF
          }
          else if (index + 3 < n) {
            val c4 = cs(index + 3) & 0xFF
            if ((c1 == 0xF0 &&
                 c2 >= 0x90 && c2 <= 0xBF
              || c1 >= 0xF1 && c1 <= 0xF3 &&
                 c2 >= 0x80 && c2 <= 0xBF
              || c1 == 0xF4 &&
                 c2 >= 0x80 && c2 <= 0x8F)
              && c3 >= 0x80 && c3 <= 0xBF
              && c4 >= 0x80 && c4 <= 0xBF) {
              index += 4
              ((c1 & 0x07) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F) // U+10000..U+10FFFF
            }
            else { index += 3; 0xFFFD }
          }
          else { index += 2; 0xFFFD }
        }
        else { index += 1; 0xFFFD }
      }
      else { index += 1; 0xFFFD }
    })
  }
}
