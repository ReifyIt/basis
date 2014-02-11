//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.text

import basis.collections._

abstract class StringBuilder extends Builder[Int] {
  override def expect(count: Int): this.type

  override def append(c: Int): Unit

  def append(cs: CharSequence): Unit = {
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
