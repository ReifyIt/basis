/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.annotation.switch

/** An abstract parser for JSON text. Implements a fast, non-tokenizing,
  * single-character lookahead recursive descent parsing algorithm.
  * 
  * Parsers directly represent their input character stream. Instances consume
  * this character stream as they parse. The protected methods `lookahead` and
  * `readChar()` implement the character stream.
  * 
  * The `parse_()` methods loosely map to productions in the JSON grammar.
  * These methods do not consume leading whitespace. Call `skipWhitespace()`
  * before parsing top-level JSON values.
  * 
  * @author Chris Sachs
  */
abstract class JSONParser[-Target <: JSONContext] {
  /** Returns the next character in the input stream without consuming it.
    * Returns a negative value to indicate end-of-input. Calls to `lookahead`
    * MUST be idempotent between calls to `readChar()`. */
  protected def lookahead: Int
  
  /** Consumes and returns the next character in the input stream. Returns the
    * value of `lookahead` and fails on end-of-input. */
  protected def readChar(): Char
  
  /** Consumes the substitution character (U+001A) and returns an interpolated
    * JSON value. Fails if the parser is not at a valid substitution point. */
  protected def readJSValue[T <: Target](target: T): T#JSValue = {
    if (lookahead == '\u001A') readChar()
    else syntaxError("Expected substitution")
    
    skipWhitespace()
    parseJSValue(target)
  }
  
  /** Skips zero or more whitespace characters and/or comments. */
  def skipWhitespace() {
    while (lookahead match {
      case ' ' | '\t' | '\n' | '\r' | '/' => true
      case _ => false
    }) {
      if (lookahead == '/') skipComment()
      else readChar()
    }
  }
  
  /** Skips a line or block comment. */
  protected def skipComment() {
    if (lookahead == '/') readChar() else syntaxError("Expected comment")
    if (lookahead == '/') {
      readChar()
      while (lookahead > 0 && lookahead != '\r' && lookahead != '\n') readChar()
      if (lookahead == '\r') readChar()
      if (lookahead == '\n') readChar()
    }
    else if (lookahead == '*') {
      readChar()
      do if (lookahead <= 0) syntaxError("Unclosed comment")
      while (readChar() != '*' || readChar() != '/')
    }
    else syntaxError("Expected comment")
  }
  
  /** Fails if the parser has not consumed all its input. Call this method
    * after parsing your JSON value and after one final `skipWhitespace()`
    * to ensure that no extraneous input remains. Extra input usually
    * indicates an error that the user would like to know about (such as
    * preemptively closing an object or array). */
  def parseEnd(): Unit = if (lookahead >= 0) syntaxError("Unexpected input")
  
  /** Consumes the next character in the input stream and fails if the consumed
    * character doesn't match the given character. */
  protected def parseChar(c: Char) {
    if (readChar() != c) syntaxError("Expected '"+ c +"'")
  }
  
  /** Consumes sequential characters in the input stream matching them in order
    * to the supplied character sequence. Fails if the stream and the character
    * sequence do not match. */
  protected def parseChars(chars: CharSequence) {
    var i = 0
    while (i < chars.length) {
      if (readChar() != chars.charAt(i)) syntaxError("Expected \""+ chars +"\"")
      i += 1
    }
  }
  
  /** Consumes the next hexadecimal character in the input stream and returns its value. */
  private def parseHexDigit(): Int = {
    (readChar(): @switch) match {
      case c @ ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') => c - '0'
      case c @ ('A' | 'B' | 'C' | 'D' | 'E' | 'F') => 10 + (c - 'A')
      case c @ ('a' | 'b' | 'c' | 'd' | 'e' | 'f') => 10 + (c - 'a')
      case _ => syntaxError("Expected hexadecimal digit")
    }
  }
  
  /** Returns a JSON value parsed from the input stream. */
  def parseJSValue[T <: Target](target: T): T#JSValue = {
    (lookahead: @switch) match {
      case '{' => parseJSObject(target)
      case '[' => parseJSArray(target)
      case '"' => parseJSString(target)
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => parseJSNumber(target)
      case 't' => parseJSTrue(target)
      case 'f' => parseJSFalse(target)
      case 'n' => parseJSNull(target)
      case '\u001A' => readJSValue(target)
      case _ => syntaxError("Expected value")
    }
  }
  
  /** Returns a JSON object parsed from the input stream. */
  def parseJSObject[T <: Target](target: T): T#JSObject = {
    val builder = target.JSObjectBuilder
    
    if (lookahead == '{') readChar()
    else syntaxError("Expected object")
    
    skipWhitespace()
    if (lookahead != '}') {
      builder += parseJSField(target)
      skipWhitespace()
    }
    while (lookahead == ',') {
      readChar()
      skipWhitespace()
      builder += parseJSField(target)
      skipWhitespace()
    }
    
    if (lookahead == '}') readChar()
    else syntaxError("Unclosed object")
    
    builder.result
  }
  
  /** Parses an object field from the input stream and adds it to `builder`. */
  protected def parseJSField[T <: Target](target: T): (String, T#JSValue) = {
    if (lookahead == '}') syntaxError("Trailing comma")
    if (lookahead != '\"') syntaxError("Expected field")
    val name = parseString()
    skipWhitespace()
    parseChar(':')
    skipWhitespace()
    val value = parseJSFieldValue(target, name)
    (name, value)
  }
  
  protected def parseJSFieldValue[T <: Target](target: T, name: String): T#JSValue = {
    parseJSValue(target)
  }
  
  /** Returns a JSON array parsed from the input stream. */
  def parseJSArray[T <: Target](target: T): T#JSArray = {
    val builder = target.JSArrayBuilder
    var index = 0
    
    if (lookahead == '[') readChar()
    else syntaxError("Expected array")
    
    skipWhitespace()
    if (lookahead != ']') {
      builder += parseJSArrayValue(target, index)
      index += 1
      skipWhitespace()
    }
    while (lookahead == ',') {
      readChar()
      skipWhitespace()
      if (lookahead == ']') syntaxError("Trailing comma")
      builder += parseJSArrayValue(target, index)
      index += 1
      skipWhitespace()
    }
    
    if (lookahead == ']') readChar()
    else syntaxError("Unclosed array")
    
    builder.result
  }
  
  protected def parseJSArrayValue[T <: Target](target: T, index: Int): T#JSValue = {
    parseJSValue(target)
  }
  
  /** Returns a JSON string parsed from the input stream. */
  def parseJSString[T <: Target](target: T): T#JSString = target.JSString(parseString())
  
  /** Parses and unescapes a quoted JSON string from the input stream.
    * Returns a Scala string. */
  protected def parseString(): String = {
    val s = new java.lang.StringBuilder
    
    if (lookahead == '\"') readChar()
    else syntaxError("Expected string")
    
    while (lookahead > 0 && (lookahead match {
      case '\"' | '\n' | '\r' | '\0' => false
      case _ => true
    })) {
      readChar() match {
        case '\\' =>
          (readChar(): @switch) match {
            case '\"' => s.append('"')
            case '\'' => s.append('\'')
            case '\\' => s.append('\\')
            case '/'  => s.append('/')
            case 'b'  => s.append('\b')
            case 'f'  => s.append('\f')
            case 'n'  => s.append('\n')
            case 'r'  => s.append('\r')
            case 't'  => s.append('\t')
            case 'u'  => s.append(((parseHexDigit() << 12) +
                                   (parseHexDigit() <<  8) +
                                   (parseHexDigit() <<  4) +
                                    parseHexDigit()).toChar)
            case _ => syntaxError("Illegal character escape sequence")
          }
        case c => s.append(c)
      }
    }
    
    if (lookahead == '\"') readChar()
    else syntaxError("Unclosed string")
    
    s.toString
  }
  
  /** Returns a JSON number parsed from the input stream. */
  def parseJSNumber[T <: Target](target: T): T#JSNumber = {
    val s = new java.lang.StringBuilder
    var decimal = false
    
    if (lookahead == '+' || lookahead == '-') s.append(readChar())
    
    if (lookahead == '0') {
      s.append(readChar())
      if (lookahead >= '0' && lookahead <= '9') syntaxError("Leading zero")
    }
    else if (lookahead >= '1' && lookahead <= '9') {
      s.append(readChar())
      while (lookahead >= '0' && lookahead <= '9') s.append(readChar())
    }
    else syntaxError("Expected digit")
    
    if (lookahead == '.') {
      decimal = true
      s.append(readChar())
      if (lookahead >= '0' && lookahead <= '9') s.append(readChar())
      else syntaxError("Expected digit")
      while (lookahead >= '0' && lookahead <= '9') s.append(readChar())
    }
    
    if (lookahead == 'e' || lookahead == 'E') {
      decimal = true
      s.append(readChar())
      if (lookahead == '+' || lookahead == '-') s.append(readChar())
      if (lookahead >= '0' && lookahead <= '9') s.append(readChar())
      else syntaxError("Expected digit")
      while (lookahead >= '0' && lookahead <= '9') s.append(readChar())
    }
    
    if (!decimal) target.JSInteger(s.toString) else target.JSDecimal(s.toString)
  }
  
  /** Returns a JSON boolean parsed from the input stream. */
  def parseJSBoolean[T <: Target](target: T): T#JSBoolean = {
    lookahead match {
      case 't' => parseJSTrue(target)
      case 'f' => parseJSFalse(target)
      case _ => syntaxError("Expected \"true\" or \"false\"")
    }
  }
  
  /** Parses and returns the `true` JSON boolean value. */
  def parseJSTrue[T <: Target](target: T): T#JSBoolean = {
    parseChars("true")
    target.JSTrue
  }
  
  /** Parses and returns the `false` JSON boolean value. */
  def parseJSFalse[T <: Target](target: T): T#JSBoolean = {
    parseChars("false")
    target.JSFalse
  }
  
  /** Parses and returns the `null` JSON boolean value. */
  def parseJSNull[T <: Target](target: T): T#JSNull = {
    parseChars("null")
    target.JSNull
  }
  
  /** Signals a syntax error and throws a `JSONException` with a message.
    * Implementations may suffix location information to `message`. */
  protected def syntaxError(message: String): Nothing =
    throw new JSONException(message)
}
