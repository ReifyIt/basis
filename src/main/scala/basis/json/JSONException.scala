/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

class JSONException(message: String, cause: Throwable) extends RuntimeException(message) {
  def this(message: String) = this(message, null)
  
  def this(cause: Throwable) = this(null, cause)
  
  def this() = this(null, null)
}
