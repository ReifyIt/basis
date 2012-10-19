/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

/** A Unicode® code point. */
class Char(val codePoint: Int) extends AnyVal {
  def isValid: Boolean = codePoint >= 0 && codePoint <= 0x10FFFF
}

/** Implicit conversions to `Char`. */
object Char {
  implicit def apply(codePoint: Int): Char = new Char(codePoint)
}
