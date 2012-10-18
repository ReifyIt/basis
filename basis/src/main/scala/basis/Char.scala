/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** A Unicode® code point. */
class Char(val codePoint: Int) extends AnyVal {
  def isValid: Boolean = codePoint >= 0 && codePoint <= 0x10FFFF
}

/** `Char` implicit conversions and implicit type class implementations. */
object Char extends Hash[Char] with Show[Char] {
  implicit def apply(codePoint: Int): Char = new Char(codePoint)
  
  override def equal(x: Char, y: Char): Boolean = x.codePoint == y.codePoint
  
  override def hash(x: Char): Int = x.codePoint.##
  
  override def show(x: Char)(buffer: CharBuffer): Unit = buffer += x
  
  implicit def implicitly: this.type = this
}
