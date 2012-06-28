/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.json
package model

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
abstract class JSONParser {
  /** Returns the next character in the input stream without consuming it.
    * Returns a negative value to indicate end-of-input. Calls to `lookahead`
    * MUST be idempotent between calls to `readChar()`. */
  protected def lookahead: Int
  
  /** Consumes and returns the next character in the input stream. Returns the
    * value of `lookahead` and fails on end-of-input. */
  protected def readChar(): Char
  
  /** Skips zero or more whitespace characters and/or comments. */
  def skipWhitespace() {
    while (true) lookahead match {
      case ' ' | '\t' | '\n' | '\r' => readChar()
      case '/' => skipComment()
      case _ => return
    }
  }
  
  /** Skips a line or block comment. */
  protected def skipComment() {
    parseChar('/', specifyError("expected '/' to start a comment"))
    if (lookahead == '/') {
      readChar()
      while (lookahead >= 0 && lookahead != '\r' && lookahead != '\n') readChar()
      if (lookahead == '\r') readChar()
      if (lookahead == '\n') readChar()
    }
    else if (lookahead == '*') {
      readChar()
      do {
        while (lookahead >= 0 && readChar() != '*') ()
      } while (lookahead >= 0 && lookahead != '/')
      parseChar('/', specifyError("expected \"*/\" to end a block comment"))
    }
    else syntaxError("expected comment")
  }
  
  /** Consumes and returns the next character in the input stream after
    * matching it to the given character; fails with `errorMessage` if the
    * stream does not match the character. */
  @inline protected final def parseChar(c: Char, errorMessage: => String): Char =
    if (lookahead == c) readChar() else syntaxError(errorMessage)
  
  /** Consumes and returns sequential characters from the input stream after
    * matching them to the given string; fails with `errorMessage` if the
    * stream does not match the string. */
  @inline protected final def parseChars(cs: String, errorMessage: => String): String = {
    var i = 0
    while (i < cs.length) {
      if (lookahead == cs.charAt(i)) readChar() else syntaxError(errorMessage)
      i += 1
    }
    cs
  }
  
  /** Consumes the next hexadecimal character in the input stream and returns its value. */
  private def parseHexDigit(): Int = {
    val c = lookahead
    val x =
      if (c >= '0' && c <= '9') c - '0'
      else if (c >= 'A' && c <= 'F') 10 + (c - 'A')
      else if (c >= 'a' && c <= 'f') 10 + (c - 'a')
      else syntaxError(specifyError("expected hexadecimal digit"))
    readChar()
    x
  }
  
  /** Fails if the parser has not consumed all its input. Call this method
    * after parsing your JSON value and after one final `skipWhitespace()`
    * to ensure that no extraneous input remains. Extra input usually
    * indicates an error that the user would like to know about (such as
    * preemptively closing an object or array). */
  def parseEnd(): Unit = if (lookahead >= 0) syntaxError(specifyError("expected end of input"))
  
  /** Returns a JSON value parsed from the input stream. */
  def parseJSONValue(model: JSONModel): model.Value = {
    (lookahead: @switch) match {
      case '{' => parseJSONObject(model)
      case '[' => parseJSONArray(model)
      case '"' => parseJSONString(model)
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => parseJSONNumber(model)
      case 't' => parseChars("true", "expected \"true\""); model.True
      case 'f' => parseChars("false", "expected \"false\""); model.False
      case 'n' => parseChars("null", "expected \"null\""); model.Null
      case 0x1A => readJSONValue(model)
      case _ => syntaxError("expected value")
    }
  }
  
  /** Returns a named JSON value in some field parsed from the input stream. */
  protected def parseJSONValueWithName(model: JSONObjectModel, name: String): model.Value = {
    (lookahead: @switch) match {
      case '{' => parseJSONObjectWithName(model, name)
      case '[' => parseJSONArrayWithName(model, name)
      case '"' => parseJSONStringWithName(model, name)
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => parseJSONNumberWithName(model, name)
      case 't' => parseChars("true", "expected \"true\""); model.TrueForName(name)
      case 'f' => parseChars("false", "expected \"false\""); model.FalseForName(name)
      case 'n' => parseChars("null", "expected \"null\""); model.NullForName(name)
      case 0x1A => readJSONValueWithName(model, name)
      case _ => syntaxError("expected value")
    }
  }
  
  /** Returns an indexed JSON value in some array parsed from the input stream. */
  protected def parseJSONValueWithIndex(model: JSONArrayModel, index: Int): model.Value = {
    (lookahead: @switch) match {
      case '{' => parseJSONObjectWithIndex(model, index)
      case '[' => parseJSONArrayWithIndex(model, index)
      case '"' => parseJSONStringWithIndex(model, index)
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => parseJSONNumberWithIndex(model, index)
      case 't' => parseChars("true", "expected \"true\""); model.TrueForIndex(index)
      case 'f' => parseChars("false", "expected \"false\""); model.FalseForIndex(index)
      case 'n' => parseChars("null", "expected \"null\""); model.NullForIndex(index)
      case 0x1A => readJSONValueWithIndex(model, index)
      case _ => syntaxError("expected value")
    }
  }
  
  /** Consumes the substitution character (U+001A) and returns an interpolated
    * JSON value. Fails if the parser is not at a valid substitution point. */
  protected def readJSONValue(model: JSONModel): model.Value = {
    parseChar(0x1A.toChar, specifyError("expected substitution"))
    skipWhitespace()
    parseJSONValue(model)
  }
  
  /** Consumes the substitution character (U+001A) and returns an interpolated
    * JSON value in some field. Fails if the parser is not at a valid substitution point. */
  protected def readJSONValueWithName(model: JSONModel, name: String): model.Value = readJSONValue(model)
  
  /** Consumes the substitution character (U+001A) and returns an interpolated
    * JSON value in some array. Fails if the parser is not at a valid substitution point. */
  protected def readJSONValueWithIndex(model: JSONModel, index: Int): model.Value = readJSONValue(model)
  
  /** Returns a JSON object parsed from the input stream. */
  def parseJSONObject(model: JSONModel): model.Object = {
    parseChar('{', specifyError("expected '{' to start an object"))
    parseJSONObjectRest(model.ObjectBuilder)
  }
  
  /** Returns a named JSON object in some field parsed from the input stream. */
  protected def parseJSONObjectWithName(model: JSONObjectModel, name: String): model.Object = {
    parseChar('{', specifyError("expected '{' to start an object"))
    parseJSONObjectRest(model.ObjectBuilderForName(name))
  }
  
  /** Returns an indexed JSON object in some array parsed from the input stream. */
  protected def parseJSONObjectWithIndex(model: JSONArrayModel, index: Int): model.Object = {
    parseChar('{', specifyError("expected '{' to start an object"))
    parseJSONObjectRest(model.ObjectBuilderForIndex(index))
  }
  
  /** Returns a JSON object parsed from the input stream after the opening `'{'`. */
  protected def parseJSONObjectRest[T](builder: JSONObjectBuilder[T]): T = {
    skipWhitespace()
    if (lookahead != '}') {
      parseJSONField(builder)
      skipWhitespace()
    }
    while (lookahead == ',') {
      readChar()
      skipWhitespace()
      parseJSONField(builder)
      skipWhitespace()
    }
    parseChar('}', specifyError("expected '}' to end an object"))
    builder.result
  }
  
  /** Parses an object field from the input stream and adds it to `builder`. */
  protected def parseJSONField(builder: JSONObjectBuilder[_]) {
    if (lookahead == '}') syntaxError("expected field, but found '}'; object has a trailing comma")
    val name = parseJSONName()
    skipWhitespace()
    parseChar(':', specifyError("expected ':' after field name"))
    skipWhitespace()
    val value = parseJSONValueWithName(builder, name)
    builder += (name, value)
  }
  
  /** Returns a field name parsed from the input stream. */
  protected def parseJSONName(): String = {
    if (lookahead != '\"') syntaxError(specifyError("expected quoted field name"))
    parseString()
  }
  
  /** Returns a JSON array parsed from the input stream. */
  def parseJSONArray(model: JSONModel): model.Array = {
    parseChar('[', specifyError("expected '[' to start an array"))
    parseJSONArrayRest(model.ArrayBuilder)
  }
  
  /** Returns a named JSON array in some field parsed from the input stream. */
  protected def parseJSONArrayWithName(model: JSONObjectModel, name: String): model.Array = {
    parseChar('[', specifyError("expected '[' to start an array"))
    parseJSONArrayRest(model.ArrayBuilderForName(name))
  }
  
  /** Returns an indexed JSON array in some other array parsed from the input stream. */
  protected def parseJSONArrayWithIndex(model: JSONArrayModel, index: Int): model.Array = {
    parseChar('[', specifyError("expected '[' to start an array"))
    parseJSONArrayRest(model.ArrayBuilderForIndex(index))
  }
  
  /** Returns a JSON array parsed from the input stream after the opening `'['`. */
  protected def parseJSONArrayRest[T](builder: JSONArrayBuilder[T]): T = {
    var index = 0
    skipWhitespace()
    if (lookahead != ']') {
      builder += parseJSONValueWithIndex(builder, index)
      index += 1
      skipWhitespace()
    }
    while (lookahead == ',') {
      readChar()
      skipWhitespace()
      if (lookahead == ']') syntaxError("expected value, but found ']'; array has a trailing comma")
      builder += parseJSONValueWithIndex(builder, index)
      index += 1
      skipWhitespace()
    }
    parseChar(']', specifyError("expected ']' to end an array"))
    builder.result
  }
  
  /** Returns a JSON string parsed from the input stream. */
  protected def parseJSONString(model: JSONModel): model.String =
    model.String(parseString())
  
  /** Returns a named JSON string in some field parsed from the input stream. */
  protected def parseJSONStringWithName(model: JSONObjectModel, name: String): model.String =
    model.StringForName(name, parseString()) 
  
  /** Returns an indexed JSON string in some array parsed from the input stream. */
  protected def parseJSONStringWithIndex(model: JSONArrayModel, index: Int): model.String =
    model.StringForIndex(index, parseString())
  
  /** Parses and unescapes a quoted JSON string from the input stream.
    * Returns a Scala string. */
  protected def parseString(): String = {
    val s = new java.lang.StringBuilder
    parseChar('\"', specifyError("expected quoted string"))
    while (lookahead >= 0 && (lookahead match {
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
            case c => syntaxError("illegal character escape: \"\\"+ c.toString +"\"")
          }
        case c => s.append(c)
      }
    }
    parseChar('\"', specifyError("expected close quote to end string"))
    s.toString
  }
  
  /** Returns a JSON number parsed from the input stream. */
  protected def parseJSONNumber(model: JSONModel): model.Number = {
    val s = new java.lang.StringBuilder
    parseJSONIntegerPart(s)
    if (lookahead == '.') {
      parseJSONFractionPart(s)
      if (lookahead == 'E' || lookahead == 'e') parseJSONExponentPart(s)
      model.Decimal(s.toString)
    }
    else if (lookahead == 'E' || lookahead == 'e') {
      parseJSONExponentPart(s)
      model.Decimal(s.toString)
    }
    else model.Integer(s.toString)
  }
  
  /** Returns a named JSON number in some field parsed from the input stream. */
  protected def parseJSONNumberWithName(model: JSONObjectModel, name: String): model.Number = {
    val s = new java.lang.StringBuilder
    parseJSONIntegerPart(s)
    if (lookahead == '.') {
      parseJSONFractionPart(s)
      if (lookahead == 'E' || lookahead == 'e') parseJSONExponentPart(s)
      model.DecimalForName(name, s.toString)
    }
    else if (lookahead == 'E' || lookahead == 'e') {
      parseJSONExponentPart(s)
      model.DecimalForName(name, s.toString)
    }
    else model.IntegerForName(name, s.toString)
  }
  
  /** Returns an indexed JSON number in some array parsed from the input stream. */
  protected def parseJSONNumberWithIndex(model: JSONArrayModel, index: Int): model.Number = {
    val s = new java.lang.StringBuilder
    parseJSONIntegerPart(s)
    if (lookahead == '.') {
      parseJSONFractionPart(s)
      if (lookahead == 'E' || lookahead == 'e') parseJSONExponentPart(s)
      model.DecimalForIndex(index, s.toString)
    }
    else if (lookahead == 'E' || lookahead == 'e') {
      parseJSONExponentPart(s)
      model.DecimalForIndex(index, s.toString)
    }
    else model.IntegerForIndex(index, s.toString)
  }
  
  /** Parses the integer part of a JSON number from the input stream and appends it to `builder`. */
  private def parseJSONIntegerPart(s: java.lang.StringBuilder) {
    if (lookahead == '+' || lookahead == '-') s.append(readChar())
    if (lookahead == '0') {
      s.append(readChar())
      if (lookahead >= '0' && lookahead <= '9') syntaxError("insignificant zero at start of number")
    }
    else if (lookahead >= '1' && lookahead <= '9') {
      s.append(readChar())
      while (lookahead >= '0' && lookahead <= '9') s.append(readChar())
    }
    else syntaxError(specifyError("expected digit"))
  }
  
  /** Parses the fraction part of a JSON number from the input stream and appends it to `builder`. */
  private def parseJSONFractionPart(s: java.lang.StringBuilder) {
    if (lookahead == '.') s.append(readChar()) else syntaxError(specifyError("expected decimal point"))
    if (lookahead >= '0' && lookahead <= '9') s.append(readChar()) else syntaxError(specifyError("expected digit"))
    while (lookahead >= '0' && lookahead <= '9') s.append(readChar())
  }
  
  /** Parses the exponent part of a JSON number from the input stream and appends it to `builder`. */
  private def parseJSONExponentPart(s: java.lang.StringBuilder) {
    if (lookahead == 'e' || lookahead == 'E') s.append(readChar()) else syntaxError(specifyError("expected ('e' | 'E')"))
    if (lookahead == '+' || lookahead == '-') s.append(readChar())
    if (lookahead >= '0' && lookahead <= '9') s.append(readChar()) else syntaxError(specifyError("expected digit"))
    while (lookahead >= '0' && lookahead <= '9') s.append(readChar())
  }
  
  /** Returns `message` with `" but found \$lookahead"` appended to it, where
    * `\$lookahead` describes the next character in the input stream. */
  protected def specifyError(message: String): String = {
    val s = new java.lang.StringBuilder(message)
    s.append(", but found ")
    if (lookahead < 0) s.append("end of input")
    else if (java.lang.Character.isISOControl(lookahead.toChar)) s.append("0x").append(lookahead)
    else s.append('\'').append(lookahead.toChar).append('\'')
    s.toString
  }
  
  /** Signals a syntax error and throws a `JSONException` with a message.
    * Implementations may suffix location information to `message`. */
  protected def syntaxError(message: String): Nothing =
    throw new JSONException(message)
}
