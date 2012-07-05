/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.unicode

final class UChar(val codePoint: Int) extends AnyVal {
  
}

object UChar {
  import language.implicitConversions
  
  @inline implicit def box(codePoint: Int): UChar = new UChar(codePoint)
  
  @inline implicit def unbox(uchar: UChar): Int = uchar.codePoint
}
