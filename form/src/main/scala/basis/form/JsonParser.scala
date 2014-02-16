//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.collections._
import basis.text._
import scala.annotation._

private[form] abstract class JsonParser {
  /** Returns `true` if the parser has reached the end of input. */
  protected def isEOF: Boolean

  /** Returns–but does not consume–the next input code point. */
  protected def head: Int

  /** Consumes the next input code point. **/
  protected def step(): Unit

  /** Returns a safely printable string representing the given code point. */
  protected def describeCodePoint(c: Int): String = {
    def hexToChar(h: Int): Int = if (h < 10) '0' + h else 'A' + (h - 10)
    val s = new java.lang.StringBuilder("\\u")
    s.append(hexToChar( c         & 0xFF))
    s.append(hexToChar((c >>>  8) & 0xFF))
    s.append(hexToChar((c >>> 16) & 0xFF))
    s.append(hexToChar((c >>> 24) & 0xFF))
    s.toString
  }

  /** Returns `message` with `" but found \$lookahead"` appended to it,
    * where `\$lookahead` describes the next input character. */
  protected def describeInput(message: String): String = {
    val s = new java.lang.StringBuilder(message)
    s.append(", but found ")
    if (isEOF) s.append("end of input")
    else s.append('\'').appendCodePoint(head).append('\'')
    s.toString
  }

  /** Throws a `JsonException` with the given error message. Subclasses
    * may optionally append location information to the message. */
  protected def syntaxError(message: String): Nothing =
    throw new JsonException(message)

  /** Consumes and returns the next character in the input stream. Returns the
    * `head` code point, failing on end of input. */
  protected def next(): Int = { val c = head; step(); c }

  /** Returns `true` if the lookahead code point matches the given code point. */
  protected def lookahead(c: Int): Boolean = !isEOF && head == c

  /** Consumes and returns the next code point in the input stream if it
    * matches the given code point; fails with `errorMessage` if the input
    * does not match the given code point. */
  @inline protected final def parse(c: Int, errorMessage: => String): Int =
    if (lookahead(c)) { step(); c } else syntaxError(errorMessage)

  /** Consumes and returns sequential code points from the input stream if
    * they match the given string; fails with `errorMessage` if the input
    * does not match the string. */
  @inline protected final def parse(cs: String, errorMessage: => String): String = {
    var i = 0
    while (i < cs.length) {
      if (lookahead(cs.codePointAt(i))) step()
      else syntaxError(errorMessage)
      i = cs.offsetByCodePoints(i, 1)
    }
    cs
  }

  /** Fails if the parser has not consumed all its of input. Call this method
    * after parsing a JSON value, and after one final `skipWhitespace()`,
    * to ensure that no extraneous input remains. Extra input usually
    * indicates an error that the user would like to know about (such as
    * preemptively closing an object or array). */
  def parseEOF(): Unit = if (!isEOF) syntaxError(describeInput("expected end of input"))

  /** Consumes either a line or block comment. */
  protected def skipComment(): Unit = {
    parse('/', describeInput("expected '/' at start of comment"))
    if (lookahead('/')) {
      step()
      while (!isEOF && head != '\r' && head != '\n') step()
      if (lookahead('\r')) step()
      if (lookahead('\n')) step()
    }
    else if (lookahead('*')) {
      step()
      do {
        while (!isEOF && { val c = head; step(); c != '*'}) ()
      } while (!isEOF && !lookahead('/'))
      parse('/', describeInput("expected \"*/\" at end of block comment"))
      ()
    }
    else syntaxError("expected comment")
  }

  /** Consumes zero or more whitespace characters and/or comments. */
  def skipWhitespace(): Unit = {
    while (!isEOF) head match {
      case ' ' | '\t' | '\n' | '\r' => step()
      case '/' => skipComment()
      case _ => return
    }
  }

  /** Consumes one or more whitespace characters and any comments. */
  protected def parseWhitespace(): Unit = {
    if (!isEOF) head match {
      case ' ' | '\t' | '\n' | '\r' => step()
      case '/' => skipComment()
      case _ => syntaxError(describeInput("expected whitespace"))
    }
    else syntaxError(describeInput("expected whitespace"))
    skipWhitespace()
  }

  /** Parses a hexadecimal character, returning its integer value. */
  private def parseHexDigit(): Int = {
    val c = head
    val x =
      if (c >= '0' && c <= '9') c - '0'
      else if (c >= 'A' && c <= 'F') 10 + (c - 'A')
      else if (c >= 'a' && c <= 'f') 10 + (c - 'a')
      else syntaxError(describeInput("expected hexadecimal digit"))
    step()
    x
  }

  /** Parses and unescapes a double quoted JSON string. */
  protected def parseDoubleQuotedString(builder: StringBuilder): builder.State = {
    parse('\"', describeInput("expected double quoted string"))
    while (!isEOF && (head match {
      case '\"' | '\n' | '\r' | '\u0000' => false
      case _ => true
    })) next() match {
      case '\\' =>
        (next(): @switch) match {
          case '\"' => builder.append('"')
          case '\'' => builder.append('\'')
          case '\\' => builder.append('\\')
          case '/'  => builder.append('/')
          case 'b'  => builder.append('\b')
          case 'f'  => builder.append('\f')
          case 'n'  => builder.append('\n')
          case 'r'  => builder.append('\r')
          case 't'  => builder.append('\t')
          case 'u'  => builder.append((parseHexDigit() << 12) +
                                      (parseHexDigit() <<  8) +
                                      (parseHexDigit() <<  4) +
                                       parseHexDigit())
          case c => syntaxError("illegal character escape: "+ describeCodePoint(c))
        }
      case c => builder.append(c)
    }
    parse('\"', describeInput("expected close quote at end of string"))
    builder.state
  }

  /** Parses and unescapes a single quoted JSON string. */
  protected def parseSingleQuotedString(builder: StringBuilder): builder.State = {
    parse('\'', describeInput("expected single quoted string"))
    while (!isEOF && (head match {
      case '\'' | '\n' | '\r' | '\u0000' => false
      case _ => true
    })) next() match {
      case '\\' =>
        (next(): @switch) match {
          case '\"' => builder.append('"')
          case '\'' => builder.append('\'')
          case '\\' => builder.append('\\')
          case '/'  => builder.append('/')
          case 'b'  => builder.append('\b')
          case 'f'  => builder.append('\f')
          case 'n'  => builder.append('\n')
          case 'r'  => builder.append('\r')
          case 't'  => builder.append('\t')
          case 'u'  => builder.append((parseHexDigit() << 12) +
                                      (parseHexDigit() <<  8) +
                                      (parseHexDigit() <<  4) +
                                       parseHexDigit())
          case c => syntaxError("illegal character escape: \\u"+ describeCodePoint(c))
        }
      case c => builder.append(c)
    }
    parse('\'', describeInput("expected close quote at end of string"))
    builder.state
  }

  private def isIdentifierStart(c: Int): Boolean = (Character.getType(c) match {
    case Character.UPPERCASE_LETTER |
         Character.LOWERCASE_LETTER |
         Character.TITLECASE_LETTER |
         Character.MODIFIER_LETTER |
         Character.OTHER_LETTER |
         Character.LETTER_NUMBER => true
    case _ => false
  }) || c == '$' || c == '_'

  private def isIdentifierPart(c: Int): Boolean = (Character.getType(c) match {
    case Character.UPPERCASE_LETTER |
         Character.LOWERCASE_LETTER |
         Character.TITLECASE_LETTER |
         Character.MODIFIER_LETTER |
         Character.OTHER_LETTER |
         Character.LETTER_NUMBER |
         Character.NON_SPACING_MARK |
         Character.COMBINING_SPACING_MARK |
         Character.DECIMAL_DIGIT_NUMBER |
         Character.CONNECTOR_PUNCTUATION => true
    case _ => false
  }) || c == '$' || c == '_' || c == '\u200C' || c == '\u200D'

  protected def parseIdentifier(): String = {
    if (!isEOF) {
      val s = new java.lang.StringBuilder
      if (isIdentifierStart(head)) {
        s.appendCodePoint(next())
        while (!isEOF && isIdentifierPart(head)) s.appendCodePoint(next())
      }
      else syntaxError(describeInput("expected identifier start character"))
      s.toString
    }
    else syntaxError(describeInput("expected identifier"))
  }

  /** Returns the next substitute form as if it was parsed directly. */
  protected def substitute(factory: JsonFactory): factory.JsonValue = {
    parse(0x1A, describeInput("expected substitution"))
    factory.JsonUndefined
  }

  /** Parsers a JSON value. */
  def parseValue(factory: JsonFactory): factory.JsonValue = {
    import factory._
    (head: @switch) match {
      case '{' => JsonObjectValue(parseObject(factory)(JsonObjectBuilder()))
      case '[' => JsonArrayValue(parseArray(factory)(JsonArrayBuilder()))
      case '\"' => JsonStringValue(parseDoubleQuotedString(JsonStringBuilder()))
      case '\'' => JsonStringValue(parseSingleQuotedString(JsonStringBuilder()))
      case '+' | '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => parseNumber(factory)
      case 't' => parse("true", "expected \"true\""); JsonTrue
      case 'f' => parse("false", "expected \"false\""); JsonFalse
      case 'n' =>
        step()
        (head: @switch) match {
          case 'e' => parse("ew", "expected \"new\""); parseConstructor(factory)
          case 'u' => parse("ull", "expected \"null\""); JsonNull
          case _ => syntaxError("expected \"new\" or \"null\"")
        }
      case 'u' => parse("undefined", "expected \"undefined\""); JsonUndefined
      case 0x1A => substitute(factory)
      case _ => syntaxError("expected value")
    }
  }

  /** Parses a JSON object. */
  def parseObject(factory: JsonFactory)(builder: Builder[(String, factory.JsonValue)]): builder.State = {
    parse('{', describeInput("expected '{' at start of object"))
    parseObjectRest(factory)(builder)
  }

  /** Parses a JSON object from after the opening `'{'`. */
  protected def parseObjectRest(factory: JsonFactory)(builder: Builder[(String, factory.JsonValue)]): builder.State = {
    skipWhitespace()
    if (!lookahead('}')) {
      parseField(factory)(builder)
      skipWhitespace()
    }
    while (lookahead(',')) {
      step()
      skipWhitespace()
      parseField(factory)(builder)
      skipWhitespace()
    }
    parse('}', describeInput("expected '}' at end of object"))
    builder.state
  }

  /** Parses an object field and appends it to the given builder. */
  protected def parseField(factory: JsonFactory)(builder: Builder[(String, factory.JsonValue)]): Unit = {
    if (lookahead('}')) syntaxError("expected field, but found '}'; object has a trailing comma")
    val name = parseName()
    skipWhitespace()
    parse(':', describeInput("expected ':' after field name"))
    skipWhitespace()
    val value = parseValue(factory)
    builder.append((name, value))
  }

  /** Parses an object field name. */
  protected def parseName(): String = {
    (head: @switch) match {
      case '\"' => parseDoubleQuotedString(UString.Builder()).toString
      case '\'' => parseSingleQuotedString(UString.Builder()).toString
      case _ => parseIdentifier()
    }
  }

  /** Parses a JSON array. */
  def parseArray(factory: JsonFactory)(builder: Builder[factory.JsonValue]): builder.State = {
    parse('[', describeInput("expected '[' at start of array"))
    parseArrayRest(factory)(builder)
  }

  /** Parses a JSON array from after the opening `'['`. */
  protected def parseArrayRest(factory: JsonFactory)(builder: Builder[factory.JsonValue]): builder.State = {
    skipWhitespace()
    if (!lookahead(']')) {
      builder.append(parseValue(factory))
      skipWhitespace()
    }
    while (lookahead(',')) {
      step()
      skipWhitespace()
      if (lookahead(']')) syntaxError("expected value, but found ']'; array has a trailing comma")
      builder.append(parseValue(factory))
      skipWhitespace()
    }
    parse(']', describeInput("expected ']' at end of array"))
    builder.state
  }

  /** Parses a JSON number. */
  protected def parseNumber(factory: JsonFactory): factory.JsonValue = {
    import factory._
    val s = new java.lang.StringBuilder
    parseIntegralPart(s)
    if (lookahead('.')) {
      parseFractionalPart(s)
      if (lookahead('E') || lookahead('e')) parseExponentPart(s)
      JsonNumber(s.toString)
    }
    else if (lookahead('E') || lookahead('e')) {
      parseExponentPart(s)
      JsonNumber(s.toString)
    }
    else JsonInteger(s.toString)
  }

  /** Parses the integral part of a JSON number and appends it to the given string builder. */
  private def parseIntegralPart(s: java.lang.StringBuilder): Unit = {
    if (lookahead('+') || lookahead('-')) s.appendCodePoint(next())
    if (lookahead('0')) {
      s.appendCodePoint(next())
      if (!isEOF && head >= '0' && head <= '9') syntaxError("insignificant zero at start of number")
    }
    else if (!isEOF && head >= '1' && head <= '9') {
      s.appendCodePoint(next())
      while (!isEOF && head >= '0' && head <= '9') s.appendCodePoint(next())
    }
    else syntaxError(describeInput("expected digit"))
  }

  /** Parses the fractional part of a JSON number and appends it to the given string builder. */
  private def parseFractionalPart(s: java.lang.StringBuilder): Unit = {
    if (lookahead('.')) s.appendCodePoint(next()) else syntaxError(describeInput("expected decimal point"))
    if (head >= '0' && head <= '9') s.appendCodePoint(next()) else syntaxError(describeInput("expected digit"))
    while (head >= '0' && head <= '9') s.appendCodePoint(next())
  }

  /** Parses the exponent part of a JSON number and appends it to the given builder. */
  private def parseExponentPart(s: java.lang.StringBuilder): Unit = {
    if (lookahead('e') || lookahead('E')) s.appendCodePoint(next()) else syntaxError(describeInput("expected ('e' | 'E')"))
    if (lookahead('+') || lookahead('-')) s.appendCodePoint(next())
    if (head >= '0' && head <= '9') s.appendCodePoint(next()) else syntaxError(describeInput("expected digit"))
    while (head >= '0' && head <= '9') s.appendCodePoint(next())
  }

  /** Parses a JavaScript constructor with static JSON arguments from after the `new` keyword.  */
  protected def parseConstructor(factory: JsonFactory): factory.JsonValue = {
    import factory._
    parseWhitespace()
    val identifier = parseIdentifier()
    skipWhitespace()
    val arguments = parseArgumentList(factory)(JsonArrayBuilder())
    JsonNew(identifier, arguments)
  }

  /** Parses a JavaScript argument list containing containing static JSON values. */
  private def parseArgumentList(factory: JsonFactory)(builder: Builder[factory.JsonValue]): builder.State = {
    parse('(', describeInput("expected '(' at start of argument list"))
    parseArgumentListRest(factory)(builder)
  }

  /** Parses a JavaScript argument list from after the opening `'('`. */
  private def parseArgumentListRest(factory: JsonFactory)(builder: Builder[factory.JsonValue]): builder.State = {
    skipWhitespace()
    if (!lookahead(')')) {
      builder.append(parseValue(factory))
      skipWhitespace()
    }
    while (lookahead(',')) {
      step()
      skipWhitespace()
      if (lookahead(')')) syntaxError("expected value, but found ')'; argument list has a trailing comma")
      builder.append(parseValue(factory))
      skipWhitespace()
    }
    parse(')', describeInput("expected ')' at end of argument list"))
    builder.state
  }
}
