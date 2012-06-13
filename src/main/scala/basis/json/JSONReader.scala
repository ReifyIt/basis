/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import java.io.Reader
import java.io.StringReader

/** A JSON parser that consumes a `java.io.Reader`.
  * 
  * @author Chris Sachs
  */
class JSONReader[-Target <: JSONContext](reader: Reader) extends JSONParser[Target] {
  def this(string: String) = this(new StringReader(string))
  
  private[this] final var line: Int = 0
  private[this] final var column: Int = 0
  
  private[this] final var nextChar: Int = reader.read()
  
  override protected final def lookahead: Int = nextChar
  
  override protected final def readChar(): Char = {
    val c = nextChar
    if (c < 0) syntaxError("Unexpected end of input")
    nextChar = reader.read()
    if (c == '\n' || (c == '\r' && nextChar != '\n')) { line += 1; column = 0 }
    else column += 1
    c.toChar
  }
  
  override protected def syntaxError(message: String): Nothing =
    throw new JSONException(message +" at line "+ line +", column "+ column)
}
