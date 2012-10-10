/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

private[text] final class String4Iterator
    (string: String4, private[this] var index: Int)
  extends CharIterator {
  
  override def hasNext: Boolean = index < string.codeUnits.length
  
  override def next(): Char = {
    val cs = string.codeUnits
    val n = cs.length
    if (index >= n) throw new scala.NoSuchElementException("empty string iterator")
    new Char({
      val c = cs(index)
      index += 1
      if (c >= 0x0000 && c <= 0xD7FF ||
          c >= 0xE000 && c <= 0x10FFFF) c // U+0000..U+D7FF | U+E000..U+10FFFF
      else 0xFFFD
    })
  }
}
