/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.algebra
package binary

private[algebra] class NumeralWriter(s: Appendable, val radix: Int) {
  assert(2 <= radix && radix <= 36)
  
  def writeInteger(significand: Integer) {
    writeSignificand(significand)
  }
  
  def writePositionalNotation(significand: Integer, exponent: Int) {
    val digitLength = math.max(1, significand.length(radix))
    val radixPoint =
      if (exponent == 0) 0
      else if (exponent < 0 && (-exponent <= digitLength)) -exponent
      else digitLength - 1
    val e = exponent + radixPoint
    
    writeSignificand(significand, radixPoint)
    if (e != 0) s.append('⨯').append(radix.toString).append('^').append(e.toString)
  }
  
  def writeScientificNotation(significand: Integer, exponent: Int) {
    val digitLength = math.max(1, significand.length(radix))
    val radixPoint =
      if (exponent == 0) 0
      else if (exponent < 0 && (-exponent < digitLength)) -exponent
      else digitLength - 1
    val e = exponent + radixPoint
    
    writeSignificand(significand, radixPoint)
    if (e != 0) s.append('⨯').append(radix.toString).append('^').append(e.toString)
  }
  
  protected def writeSignificand(significand: Integer, radixPoint: Int = 0) {
    val digitLength = math.max(1, significand.length(radix))
    val digits = new Array[Byte](digitLength)
    
    val q = Integer.alloc
    digits(0) = Integer.divide(significand, radix, q).toByte
    var i = 1
    while (q.size > 1 || q(0) != 0L) {
      digits(i) = Integer.divide(q, radix, q).toByte
      i += 1
    }
    
    if (significand.sign < 0) s.append('-')
    if (i == radixPoint) s.append('0').append('.')
    i -= 1
    while (i >= 0) {
      val digit = digits(i)
      assume(0 <= digit && digit < radix)
      s.append((if (digit < 10) '0' + digit else 'A' + digit - 10).toChar)
      if (i > 0 && i == radixPoint) s.append('.')
      i -= 1
    }
  }
}
