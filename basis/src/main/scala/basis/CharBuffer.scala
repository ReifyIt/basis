/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** A specialized Unicode® character buffer. */
trait CharBuffer extends Buffer[Nothing, Char] {
  /* FIXME: waiting on SI-6500 */
  //override def += (char: Char): this.type
  
  def append(chars: java.lang.CharSequence): this.type = {
    val n = chars.length
    var i = 0
    while (i < n) this += new Char({
      val c1 = chars.charAt(i).toInt
      i += 1
      if (c1 <= 0xD7FF || c1 >= 0xE000) c1 // U+0000..U+D7FF | U+E000..U+FFFF
      else if (c1 <= 0xDBFF && i < n) { // c1 >= 0xD800
        val c2 = chars.charAt(i).toInt
        if (c2 >= 0xDC00 && c2 <= 0xDFFF) { // U+10000..U+10FFFF
          i += 1
          (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
        }
        else 0xFFFD
      }
      else 0xFFFD
    })
    this
  }
}
