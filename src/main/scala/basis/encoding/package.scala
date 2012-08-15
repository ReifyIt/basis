/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

package object encoding {
  import scala.language.implicitConversions
  
  @inline implicit def wrapString(string: java.lang.String): JString = new JString(string)
  
  @inline implicit def unwrapString(jstring: JString): java.lang.String = jstring.unbox
}
