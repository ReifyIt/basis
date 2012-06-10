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
  * 
  * @constructor Constructs a JSON parser with a `JSONFactory`.
  * @tparam JSON  The singleton type of the `JSONFactory` composed with this parser.
  * @param  json  The `JSONFactory` used by this parser to construct JSON values.
  */
abstract class JSONParser[+JSON <: JSONFactory with Singleton](protected val json: JSON) {
  import json._
  
  /** Returns the next character in the input stream without consuming it.
    * Returns a negative value to indicate end-of-input. Calls to `lookahead`
    * MUST be idempotent between calls to `readChar()`. */
  protected def lookahead: Int
  
  /** Consumes and returns the next character in the input stream. Returns the
    * value of `lookahead` and fails on end-of-input. */
  protected def readChar(): Char
  
  /** Consumes zero or more whitespace characters. */
  def skipWhitespace() {
    while (lookahead match {
      case ' ' | '\t' | '\n' | '\r' => true
      case _ => false
    }) readChar()
  }
  
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
  def parseJSValue(): JSValue = {
    (lookahead: @switch) match {
      case '{' => parseJSObject()
      case '[' => parseJSArray()
      case '"' => parseJSString()
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => parseJSNumber()
      case 't' => parseJSTrue()
      case 'f' => parseJSFalse()
      case 'n' => parseJSNull()
      case _ => syntaxError("Expected value")
    }
  }
  
  /** Returns a JSON object parsed from the input stream. */
  def parseJSObject(): JSObject = {
    val builder = JSObject.newBuilder()
    
    if (lookahead == '{') readChar()
    else syntaxError("Expected object")
    
    skipWhitespace()
    if (lookahead != '}') {
      parseJSField(builder)
      skipWhitespace()
    }
    while (lookahead == ',') {
      readChar()
      skipWhitespace()
      parseJSField(builder)
      skipWhitespace()
    }
    
    if (lookahead == '}') readChar()
    else syntaxError("Unterminated object")
    
    builder.result
  }
  
  /** Parses an object field from the input stream and adds it to `builder`. */
  protected def parseJSField(builder: JSObjectBuilder) {
    if (lookahead != '\"') syntaxError("Expected field")
    val name = parseString()
    skipWhitespace()
    parseChar(':')
    skipWhitespace()
    val value = parseJSValue()
    builder += (name, value)
  }
  
  /** Returns a JSON array parsed from the input stream. */
  def parseJSArray(): JSArray = {
    val builder = JSArray.newBuilder()
    
    if (lookahead == '[') readChar()
    else syntaxError("Expected array")
    
    skipWhitespace()
    if (lookahead != ']') {
      builder += parseJSValue()
      skipWhitespace()
    }
    while (lookahead == ',') {
      readChar()
      skipWhitespace()
      builder += parseJSValue()
      skipWhitespace()
    }
    
    if (lookahead == ']') readChar()
    else syntaxError("Unterminated array")
    
    builder.result
  }
  
  /** Returns a JSON string parsed from the input stream. */
  def parseJSString(): JSString = JSString(parseString())
  
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
            case '/'  => s.append('/')
            case '\'' => s.append('\'')
            case '\\' => s.append('\\')
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
    else syntaxError("Unterminated string")
    
    s.toString
  }
  
  /** Returns a JSON number parsed from the input stream. */
  def parseJSNumber(): JSNumber = {
    val s = new java.lang.StringBuilder
    var decimal = false
    
    if (lookahead == '-') s.append(readChar())
    
    if (lookahead == '0') s.append(readChar())
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
      s.append(readChar())
      if (lookahead == '+' || lookahead == '-') s.append(readChar())
      if (lookahead >= '0' && lookahead <= '9') s.append(readChar())
      else syntaxError("Expected digit")
      while (lookahead >= '0' && lookahead <= '9') s.append(readChar())
    }
    
    if (!decimal) JSInteger(s.toString) else JSDecimal(s.toString)
  }
  
  /** Returns a JSON boolean parsed from the input stream. */
  def parseJSBoolean(): JSBoolean = {
    lookahead match {
      case 't' => parseJSTrue()
      case 'f' => parseJSFalse()
      case _ => syntaxError("Expected \"true\" or \"false\"")
    }
  }
  
  /** Parses and returns the `true` JSON boolean value. */
  def parseJSTrue(): JSBoolean = {
    parseChars("true")
    json.JSTrue
  }
  
  /** Parses and returns the `false` JSON boolean value. */
  def parseJSFalse(): JSBoolean = {
    parseChars("false")
    json.JSFalse
  }
  
  /** Parses and returns the `null` JSON boolean value. */
  def parseJSNull(): JSNull = {
    parseChars("null")
    json.JSNull
  }
  
  /** Signals a syntax error and throws a `JSONException` with a message.
    * Implementations may suffix location information to `message`. */
  protected def syntaxError(message: String): Nothing =
    throw new JSONException(message)
}

/** Contains JSON parser implementations for various input sources. */
object JSONParser {
  /** A JSON parser that consumes a character sequence. */
  class FromCharSequence[+JSON <: JSONFactory with Singleton]
      (json: JSON, cs: CharSequence)
    extends JSONParser[JSON](json) {
    
    private[this] final var line: Int = 0
    private[this] final var column: Int = 0
    
    private[this] final var nextChar: Int = if (cs.length >= 0) cs.charAt(0) else -1
    private[this] final var nextCharIndex: Int = 1
    
    override protected final def lookahead: Int = nextChar
    
    override protected final def readChar(): Char = {
      val c = nextChar
      if (c < 0) syntaxError("Unexpected end of input")
      nextChar = if (nextCharIndex < cs.length) cs.charAt(nextCharIndex) else -1
      if (c == '\n' || (c == '\r' && nextChar != '\n')) { line += 1; column = 0 } else column += 1
      nextCharIndex += 1
      c.toChar
    }
    
    override protected def syntaxError(message: String): Nothing =
      throw new JSONException(message +" at line "+ line +", column "+ column)
  }
  
  /** A JSON parser that consumes a `java.io.Reader`. */
  class FromReader[+JSON <: JSONFactory with Singleton]
      (json: JSON, reader: java.io.Reader)
    extends JSONParser[JSON](json) {
    
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
  
  /** A JSON parser that consumes sequential strings and interpolates JSON
    * values into gaps in the string parts.
    * 
    * The parser inserts the unicode character U+001A (SUBSTITUTE) into the
    * input stream in-between string parts. When the `parseJSValue()` method
    * encounters this character it returns the next JSON value in the `args`
    * sequence as if it has just been parsed.
    */
  class Interpolating[+JSON <: JSONFactory with Singleton]
      (override protected val json: JSON, parts: Seq[String])(args: Seq[JSON#JSValue])
    extends JSONParser[JSON](json) {
    
    import json._
    
    private[this] final var part: String = parts(0)
    private[this] final var nextPartIndex: Int = 1
    
    private[this] final var nextChar: Int = -1
    private[this] final var nextCharIndex: Int = 0
    
    private[this] final var nextArgIndex: Int = 0
    
    processNextChar()
    
    /** Updates `lookahead` with the next character in the input stream. */
    private[this] final def processNextChar() {
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
    
    override protected def lookahead: Int = nextChar
    
    override protected def readChar(): Char = {
      val c = nextChar
      if (c < 0) syntaxError("Unexpected end of input")
      processNextChar()
      c.toChar
    }
    
    /** Consumes the next JSON value in the `args` sequence as well as the
      * substitution character in the input stream. Fails if the parser is not
      * at a substitution point. */
    protected def readJSValue(): JSValue = {
      if (lookahead == '\u001A') readChar()
      else syntaxError("Expected interpolated value")
      
      val arg = args(nextArgIndex)
      nextArgIndex += 1
      arg.asInstanceOf[JSValue]
    }
    
    /** Returns a JSON value parsed from the input stream. Interpolates a JSON
      * value if the parser is at a substitution point. */
    override def parseJSValue(): JSValue = {
      if (lookahead == '\u001A') readJSValue()
      else super.parseJSValue()
    }
    
    override protected def syntaxError(message: String): Nothing =
      throw new JSONException(message +" in part "+ (nextPartIndex - 1) +" at index "+ (nextCharIndex - 1))
  }
}
