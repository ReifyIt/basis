/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

private[text] final class JavaStringBuffer extends StringBuffer[String] {
  override type State = java.lang.String
  
  private[this] var builder = new java.lang.StringBuilder
  
  override def += (c: Char): this.type = {
    builder.appendCodePoint(c.codePoint)
    this
  }
  
  override def expect(count: Int): this.type = {
    builder.ensureCapacity(builder.length + count)
    this
  }
  
  override def check: java.lang.String = builder.toString
  
  override def clear(): Unit = builder.setLength(0)
}
