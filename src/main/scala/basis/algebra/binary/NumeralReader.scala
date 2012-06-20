/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary

import java.io.Reader
import java.io.StringReader

private[algebra]class NumeralReader(reader: Reader, val radix: Int) {
  def this(string: String, radix: Int) = this(new StringReader(string), radix: Int)
  
  assert(2 <= radix && radix <= 36)
  
  private[this] final var index: Int = 0
  
  private[this] final var nextChar: Int = reader.read()
  
  protected final def lookahead: Int = nextChar
  
  protected final def readChar(): Char = {
    val c = nextChar
    if (c < 0) syntaxError("unexpected end of input")
    nextChar = reader.read()
    index += 1
    c.toChar
  }
  
  protected final def charToDigit(c: Int): Int = {
    if (c >= '0' && c < '0' + math.min(radix, 10)) c - '0'
    else if (c >= 'A' && c < 'A' + (radix - 10)) 10 + c - 'A'
    else if (c >= 'a' && c < 'a' + (radix - 10)) 10 + c - 'a'
    else -1
  }
  
  def parsePositionalNumber(): Integer = {
    val significand = Integer(0L)
    val sign = parseWhole(significand)
    significand.sign = if (significand.size == 1 && significand(0) == 0L) 1 else sign
    significand
  }
  
  def parseExponentialNumber(): (Integer, Integer, Int) = {
    val significand = Integer(0L)
    val error = Integer(0L)
    var exponent = 0
    val sign = parseWhole(significand)
    if (lookahead == '.') exponent -= parseFraction(significand)
    if (lookahead == '±') parseError(error)
    if (lookahead == 'E' || lookahead == 'e') exponent += parseExponent()
    significand.sign = if (significand.size == 1 && significand(0) == 0L) 1 else sign
    (significand, error, exponent)
  }
  
  protected def parseWhole(significand: Integer): Int = {
    val sign = lookahead match {
      case '-' => readChar(); -1
      case '+' => readChar(); 1
      case _ => 1
    }
    var digit = charToDigit(lookahead)
    if (digit < 0) syntaxError(s"expected base-$radix digit")
    while (digit >= 0) {
      readChar()
      Integer.multiply(significand, radix, significand)
      Integer.add(significand, digit, significand)
      digit = charToDigit(lookahead)
    }
    sign
  }
  
  protected def parseFraction(significand: Integer): Int = {
    if (lookahead == '.') readChar()
    else syntaxError("expected radix point")
    var fractionDigits = 0
    var digit = charToDigit(lookahead)
    if (digit < 0) syntaxError(s"expected base-$radix digit")
    while (digit >= 0) {
      readChar()
      Integer.multiply(significand, radix, significand)
      Integer.add(significand, digit, significand)
      digit = charToDigit(lookahead)
      fractionDigits += 1
    }
    fractionDigits
  }
  
  protected def parseError(error: Integer) {
    if (lookahead == '±') readChar()
    else syntaxError("expected ± error")
    var digit = charToDigit(lookahead)
    if (digit < 0) syntaxError(s"expected base-$radix digit")
    while (digit >= 0) {
      readChar()
      Integer.multiply(error, radix, error)
      Integer.add(error, digit, error)
      digit = charToDigit(lookahead)
    }
  }
  
  protected def parseExponent(): Int = {
    if (lookahead == 'E' || lookahead == 'e') readChar()
    else syntaxError("expected 'E'")
    val sign = lookahead match {
      case '-' => readChar(); -1
      case '+' => readChar(); 1
      case _ => 1
    }
    var exponent = 0
    var digit = charToDigit(lookahead)
    if (digit < 0) syntaxError(s"expected base-$radix digit")
    while (digit >= 0) {
      readChar()
      exponent = radix * exponent + digit
      digit = charToDigit(lookahead)
    }
    sign * exponent
  }
  
  def syntaxError(message: String): Nothing =
    throw new NumberFormatException(message +" at index "+ index)
}
