/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

/** A JSON parser that consumes sequential strings and optionally interpolates
  * values into the gaps in the string parts.
  * 
  * The parser inserts the unicode character U+001A (SUBSTITUTE) into the
  * input stream in-between string parts. When the `parseJSValue()` method
  * encounters this character it invokes the protected `readJSValue()` method
  * and interpolates the result as if it has just been parsed. By default,
  * `readJSValue()` consumes the substitution charceter and skips whitespace
  * before returning the result of invoking `parseJSValue()`.
  * 
  * @author Chris Sachs
  */
class JSONPartParser[-Target <: JSONContext](parts: Seq[String]) extends JSONParser[Target] {
  private[this] final var part: String = parts(0)
  private[this] final var nextPartIndex: Int = 1
  
  private[this] final var nextChar: Int = 0
  private[this] final var nextCharIndex: Int = 0
  
  processNextChar()
  
  /** Updates `lookahead` with the next character in the input stream. */
  private[this] def processNextChar() {
    if (nextCharIndex < part.length) {
      nextChar = part.charAt(nextCharIndex)
      nextCharIndex += 1
    }
    else if (nextPartIndex < parts.length) {
      nextChar = '\u001A' // substitution
      part = parts(nextPartIndex)
      nextPartIndex += 1
      nextCharIndex = 0
    }
    else nextChar = -1
  }
  
  override protected final def lookahead: Int = nextChar
  
  override protected final def readChar(): Char = {
    val c = nextChar
    if (c < 0) syntaxError("Unexpected end of input")
    processNextChar()
    c.toChar
  }
  
  override protected def syntaxError(message: String): Nothing =
    throw new JSONException(message +" in part "+ (nextPartIndex - 1) +" at index "+ (nextCharIndex - 1))
}
