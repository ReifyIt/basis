/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

private[text] final class UTF16Iterator(string: UTF16String) extends StringIterator {
  private[this] var index: Int = 0
  
  override def hasNext: Boolean = index < string.size
  
  override def next(): Char = {
    if (index >= string.size) throw new scala.NoSuchElementException("empty iterator")
    val c = string(index)
    index = string.nextIndex(index)
    c
  }
}
