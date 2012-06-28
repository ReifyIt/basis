/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json
package model

/** A JSON parser that consumes sequential strings and optionally interpolates
  * values into the gaps in the string parts.
  * 
  * The parser inserts the unicode character U+001A (SUBSTITUTE) into the
  * input stream in-between string parts. When the `parseJSONValue()` method
  * encounters this character it invokes the protected `readJSONValue()` method
  * and interpolates the result as if it has just been parsed. By default,
  * `readJSONValue()` consumes the substitution charceter and skips whitespace
  * before returning the result of invoking `parseJSONValue()`.
  * 
  * @author Chris Sachs
  */
class JSONJoiner(parts: Seq[String]) extends JSONParser {
  private[this] final var line: Int = 1
  private[this] final var column: Int = 1
  
  private[this] final var part: String = parts(0)
  private[this] final var nextPartIndex: Int = 1
  
  private[this] final var nextChar: Int = 0
  private[this] final var nextCharIndex: Int = 0
  
  processNextChar()
  
  /** Updates `lookahead` with the next character in the input stream. */
  private[this] final def processNextChar() {
    if (nextCharIndex < part.length) {
      nextChar = part.charAt(nextCharIndex)
      nextCharIndex += 1
    }
    else if (nextPartIndex < parts.length) {
      nextChar = 0x1A // substitution
      part = parts(nextPartIndex)
      nextPartIndex += 1
      nextCharIndex = 0
      line = 1
      column = 1
    }
    else nextChar = -1
  }
  
  override protected final def lookahead: Int = nextChar
  
  override protected final def readChar(): Char = {
    val c = nextChar
    if (c < 0) throw new JSONException("unexpected end of input")
    processNextChar()
    if (c == '\n' || (c == '\r' && nextChar != '\n')) { line += 1; column = 1 } else column += 1
    c.toChar
  }
  
  override protected def syntaxError(message: String): Nothing = {
    val s = new StringBuilder(message)
    s.append(" at line ").append(line).append(", column ").append(column)
    if (parts.length > 1) s.append(" in part ").append(nextPartIndex)
    throw new JSONException(s.toString)
  }
}
