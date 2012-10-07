/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.text

import basis._

trait StringBuffer extends Buffer[String, Char] {
  override def += (char: Char): this.type
  
  def append(chars: java.lang.CharSequence): this.type = {
    val n = chars.length
    var i = 0
    while (i < n) this += new Char({
      val c1 = chars.charAt(i)
      if (c1 <= 0xD7FF || c1 >= 0xE000) { // c1 >= 0 && c1 <= 0x10000
        // U+0000..U+D7FF | U+E000..U+FFFF
        i += 1 // valid code point
        c1
      }
      else if (c1 <= 0xDBFF) { // c1 >= 0xD800
        // U+10000..U+10FFFF
        i += 1 // valid low surrogate
        if (i < n) {
          val c2 = chars.charAt(i)
          if (c2 >= 0xDC00 && c2 <= 0xDFFF) {
            i += 1 // valid high surrogate
            (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
          }
          else 0xFFFD // unpaired low surrogate
        }
        else 0xFFFD // missing high surrogate
      }
      else { // c1 >= 0xDC00 && c1 <= 0xDFFF
        i += 1
        0xFFFD // unpaired high surrogate
      }
    })
    this
  }
}
