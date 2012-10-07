/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

private[text] final class JavaStringIterator
    (string: java.lang.String, private[this] var index: Int)
  extends StringIterator {
  
  override def hasNext: Boolean = index < string.length
  
  override def next(): Char = {
    val n = string.length
    if (index >= n) throw new scala.NoSuchElementException("empty string iterator")
    new Char({
      val c1 = string.charAt(index)
      if (c1 <= 0xD7FF || c1 >= 0xE000) {
        index += 1
        c1 // U+0000..U+D7FF | U+E000..U+FFFF
      }
      else if (c1 <= 0xDBFF && index + 1 < n) { // c1 >= 0xD800
        val c2 = string.charAt(index + 1)
        if (c2 >= 0xDC00 && c2 <= 0xDFFF) {
          index += 2
          (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000 // U+10000..U+10FFFF
        }
        else { index += 1; 0xFFFD }
      }
      else { index += 1; 0xFFFD }
    })
  }
}
