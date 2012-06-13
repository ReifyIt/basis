/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

/** A JSON parser that consumes sequential strings and interpolates argument
  * values into the gaps in the string parts.
  * 
  * @author Chris Sachs
  */
class JSONInterpolator[-Target <: JSONContext with Singleton]
    (parts: Seq[String], args: Seq[Target#JSValue])
  extends JSONPartParser[Target](parts) {
  
  private[this] final var nextArgIndex: Int = 0
  
  override protected def readJSValue[T <: Target](target: T): T#JSValue = {
    if (lookahead == '\u001A') readChar()
    else syntaxError("Expected interpolated value")
    
    val arg = args(nextArgIndex)
    nextArgIndex += 1
    arg.asInstanceOf[T#JSValue]
  }
}
