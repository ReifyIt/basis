/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.text

import basis.collections._

/** A builder for Unicode® strings.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Builders
  * 
  * @define collection  string builder
  */
abstract class StringBuilder extends Builder[Int] {
  /** Prepares this $collection to receive a certain number of characters. */
  override def expect(count: Int): this.type
  
  override def append(c: Int): Unit
  
  def append(cs: java.lang.CharSequence) {
    val n = cs.length
    var i = 0
    while (i < n) this += {
      val c1 = cs.charAt(i).toInt
      i += 1
      if (c1 <= 0xD7FF || c1 >= 0xE000) c1 // U+0000..U+D7FF | U+E000..U+FFFF
      else if (c1 <= 0xDBFF && i < n) { // c1 >= 0xD800
        val c2 = cs.charAt(i).toInt
        if (c2 >= 0xDC00 && c2 <= 0xDFFF) { // U+10000..U+10FFFF
          i += 1
          (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000
        }
        else 0xFFFD
      }
      else 0xFFFD
    }
  }
  
  override def += (c: Int): this.type = {
    append(c)
    this
  }
  
  def ++= (cs: CharSequence): this.type = {
    append(cs)
    this
  }
}
