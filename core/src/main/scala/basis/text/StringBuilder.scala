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

  def show(x: Any): Unit = x match {
    case x: String =>
      new UString(x).show(this)
    case x: Float =>
      append(java.lang.Float.toString(x))
      append('F')
    case x: Long =>
      append(java.lang.Long.toString(x))
      append('L')
    case _ =>
      append(x.toString)
  }

  def showHex(x: Any): Unit = {
    def hexToChar(x: Int): Int = if (x < 10) '0' + x else 'A' + (x - 10)
    x match {
      case x: Int =>
        append('0'); append('x')
        append(hexToChar(x >>> 28      )); append(hexToChar(x >>> 24 & 0xF))
        append(hexToChar(x >>> 20 & 0xF)); append(hexToChar(x >>> 16 & 0xF))
        append(hexToChar(x >>> 12 & 0xF)); append(hexToChar(x >>>  8 & 0xF))
        append(hexToChar(x >>>  4 & 0xF)); append(hexToChar(x        & 0xF))
      case x: Long =>
        val hi = (x >>> 32).toInt
        val lo = x.toInt
        append('0'); append('x')
        append(hexToChar(hi >>> 28      )); append(hexToChar(hi >>> 24 & 0xF))
        append(hexToChar(hi >>> 20 & 0xF)); append(hexToChar(hi >>> 16 & 0xF))
        append(hexToChar(hi >>> 12 & 0xF)); append(hexToChar(hi >>>  8 & 0xF))
        append(hexToChar(hi >>>  4 & 0xF)); append(hexToChar(hi        & 0xF))
        append(hexToChar(lo >>> 28      )); append(hexToChar(lo >>> 24 & 0xF))
        append(hexToChar(lo >>> 20 & 0xF)); append(hexToChar(lo >>> 16 & 0xF))
        append(hexToChar(lo >>> 12 & 0xF)); append(hexToChar(lo >>>  8 & 0xF))
        append(hexToChar(lo >>>  4 & 0xF)); append(hexToChar(lo        & 0xF))
        append('L')
      case _ =>
        append(x.toString)
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

  def +~= (x: Any): this.type = {
    show(x)
    this
  }
}
