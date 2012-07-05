/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.unicode

/** A builder for well-formed Unicode® strings.
  * 
  * @author Chris Sachs
  * 
  * @tparam S   the type of string produced by this builder.
  */
abstract class UStringBuilder[+S] {
  /** Tells this builder to expect `count` code units. */
  def expect(count: Int): this.type
  
  /** Appends a Unicode scalar value to this builder. A scalar value consists
    * of any valid code point except the high and low surrogates, U+D800..U+DBFF
    * and U+DC00..U+DFFF, respectively. If `codePoint` does not lie in either
    * the range U+0000..U+D7FF or U+E000..U+10FFFF, inclusive, then the
    * replacement character U+FFFD is appended in its place. */
  def += (codePoint: Int): this.type
  
  /** Appends a UTF-16 character sequence to this builder. */
  def append(chars: CharSequence): this.type = {
    val n = chars.length
    var i = 0
    while (i < n) this += {
      val c1 = chars.charAt(i)
      if (c1 < 0xD800 || c1 > 0xDFFF) {
        // U+0000..U+D7FF | U+E000..U+FFFF
        i += 1 // valid 1st code unit
        c1
      }
      else if (c1 >= 0xD800 && c1 <= 0xDBFF) {
        // U+D800..U+DBFF; high surrogate
        i += 1 // valid 1st code unit
        if (i < n) {
          val c2 = chars.charAt(i)
          if (c2 >= 0xDC00 && c2 <= 0xDFFF) {
            i += 1 // valid 2nd code unit
            ((c1 & 0x3F) << 10) | (c2 & 0x3F)
          }
          else 0xFFFD // invalid 2nd code unit
        }
        else 0xFFFD // missing 2nd code unit
      }
      else {
        // U+DC00..U+DFFF; unpaired low surrogate
        i += 1 // invalid 1st code unit
        0xFFFD 
      }
    }
    this
  }
  
  /** Returns a string containing all appended characters and resets the
    * builder to the empty state. */
  def result: S
  
  /** Resets this builder to the empty state without collecting its result.
    * A cleared builder may re-use its un-retrieved buffer. */
  def clear(): Unit
}
