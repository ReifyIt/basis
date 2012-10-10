/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

private[text] final class String2Iterator
    (string: String2, private[this] var index: Int)
  extends CharIterator {
  
  override def hasNext: Boolean = index < string.codeUnits.length
  
  override def next(): Char = {
    val cs = string.codeUnits
    val n = cs.length
    if (index >= n) throw new scala.NoSuchElementException("empty iterator")
    new Char({
      val c1 = cs(index).toInt
      index += 1
      if (c1 <= 0xD7FF || c1 >= 0xE000) c1 // U+0000..U+D7FF | U+E000..U+FFFF
      else if (c1 <= 0xDBFF && index < n) { // c1 >= 0xD800
        val c2 = cs(index).toInt
        if (c2 >= 0xDC00 && c2 <= 0xDFFF) { // U+10000..U+10FFFF
          index += 1
          (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
        }
        else 0xFFFD
      }
      else 0xFFFD
    })
  }
}
