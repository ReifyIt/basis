/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json

import scala.annotation.switch

abstract class JSONParser[+JSON <: JSONFactory with Singleton](protected val json: JSON) {
  import json._
  
  protected def lookahead: Int
  
  protected def readChar(): Char
  
  protected def parseChar(c: Char) {
    if (readChar() != c) syntaxError("Expected '"+ c +"'")
  }
  
  protected def parseChars(chars: String) {
    var i = 0
    while (i < chars.length) {
      if (readChar() != chars(i)) syntaxError("Expected \""+ chars +"\"")
      i += 1
    }
  }
  
  private def parseHexDigit(): Int = {
    (readChar(): @switch) match {
      case c @ ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') => c - '0'
      case c @ ('A' | 'B' | 'C' | 'D' | 'E' | 'F') => 10 + (c - 'A')
      case c @ ('a' | 'b' | 'c' | 'd' | 'e' | 'f') => 10 + (c - 'a')
      case _ => syntaxError("Expected hexadecimal digit")
    }
  }
  
  def parseWhitespace() {
    while (lookahead match {
      case ' ' | '\t' | '\n' | '\r' => true
      case _ => false
    }) readChar()
  }
  
  def parseJSValue(): JSValue = {
    (lookahead: @switch) match {
      case '{' => parseJSObject()
      case '[' => parseJSArray()
      case '"' | '\'' => parseJSString()
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => parseJSNumber()
      case 't' => parseJSTrue()
      case 'f' => parseJSFalse()
      case 'n' => parseJSNull()
      case _ => syntaxError("Expected value")
    }
  }
  
  def parseJSObject(): JSObject = {
    val builder = JSObject.newBuilder()
    
    if (lookahead == '{') readChar()
    else syntaxError("Expected object")
    
    parseWhitespace()
    if (lookahead != '}') {
      parseJSField(builder)
      parseWhitespace()
    }
    while (lookahead == ',') {
      readChar()
      parseWhitespace()
      parseJSField(builder)
      parseWhitespace()
    }
    
    if (lookahead == '}') readChar()
    else syntaxError("Unterminated object")
    
    builder.result
  }
  
  protected def parseJSField(builder: JSObjectBuilder) {
    if (lookahead != '\"') syntaxError("Expected field")
    val name = parseString()
    parseWhitespace()
    parseChar(':')
    parseWhitespace()
    val value = parseJSValue()
    builder += (name, value)
  }
  
  def parseJSArray(): JSArray = {
    val builder = JSArray.newBuilder()
    
    if (lookahead == '[') readChar()
    else syntaxError("Expected array")
    
    parseWhitespace()
    if (lookahead != ']') {
      builder += parseJSValue()
      parseWhitespace()
    }
    while (lookahead == ',') {
      readChar()
      parseWhitespace()
      builder += parseJSValue()
      parseWhitespace()
    }
    
    if (lookahead == ']') readChar()
    else syntaxError("Unterminated array")
    
    builder.result
  }
  
  def parseJSString(): JSString = JSString(parseString())
  
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
  
  def parseJSBoolean(): JSBoolean = {
    lookahead match {
      case 't' => parseJSTrue()
      case 'f' => parseJSFalse()
      case _ => syntaxError("Expected \"true\" or \"false\"")
    }
  }
  
  def parseJSTrue(): JSBoolean = {
    parseChars("true")
    json.JSTrue
  }
  
  def parseJSFalse(): JSBoolean = {
    parseChars("false")
    json.JSFalse
  }
  
  def parseJSNull(): JSNull = {
    parseChars("null")
    json.JSNull
  }
  
  protected def syntaxError(message: String): Nothing =
    throw new JSONException(message)
}

object JSONParser {
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
    
    protected def readJSValue(): JSValue = {
      if (lookahead == '\u001A') readChar()
      else syntaxError("Expected interpolated value")
      
      val arg = args(nextArgIndex)
      nextArgIndex += 1
      arg.asInstanceOf[JSValue]
    }
    
    override def parseJSValue(): JSValue = {
      if (lookahead == '\u001A') readJSValue()
      else super.parseJSValue()
    }
    
    override protected def syntaxError(message: String): Nothing =
      throw new JSONException(message +" in part "+ (nextPartIndex - 1) +" at index "+ (nextCharIndex - 1))
  }
}
