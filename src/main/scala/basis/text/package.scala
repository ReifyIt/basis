/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

package object text {
  import scala.language.implicitConversions
  
  @inline implicit def wrapString(string: java.lang.String): String = new String(string)
  
  @inline implicit def unwrapString(string: String): java.lang.String = string.toString
}
