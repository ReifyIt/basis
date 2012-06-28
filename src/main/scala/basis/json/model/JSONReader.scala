/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json
package model

/** A JSON parser that consumes a `java.io.Reader`.
  * 
  * @author Chris Sachs
  */
class JSONReader(reader: java.io.Reader) extends JSONParser {
  def this(string: String) = this(new java.io.StringReader(string))
  
  private[this] final var line: Int = 1
  private[this] final var column: Int = 1
  
  private[this] final var nextChar: Int = reader.read()
  
  override protected final def lookahead: Int = nextChar
  
  override protected final def readChar(): Char = {
    val c = nextChar
    if (c < 0) throw new JSONException("unexpected end of input")
    nextChar = reader.read()
    if (c == '\n' || (c == '\r' && nextChar != '\n')) { line += 1; column = 1 } else column += 1
    c.toChar
  }
  
  override protected def syntaxError(message: String): Nothing =
    throw new JSONException(message +" at line "+ line +", column "+ column)
}
