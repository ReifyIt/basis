/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

private[text] final class UTF32Iterator(string: UTF32String) extends StringIterator {
  private[this] var index: Int = 0
  
  override def hasNext: Boolean = index < string.length
  
  override def next(): Char = {
    if (index >= string.length) throw new scala.NoSuchElementException("empty iterator")
    val c = string(index)
    index += 1
    c
  }
}
