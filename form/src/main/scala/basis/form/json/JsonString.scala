//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package json

import basis.text._

trait JsonString extends StringForm { variant: JsonVariant =>
  override type StringForm <: JsonString with AnyForm

  trait JsonString extends JsonValue with BaseString { this: StringForm =>
    override def writeJson(builder: StringBuilder): Unit = {
      def hexToChar(h: Int): Int = if (h < 10) '0' + h else 'A' + (h - 10)
      builder.append('"')
      val cs = iterator
      var c = 0
      while (!cs.isEmpty) {
        val b = c
        c = cs.head
        c match {
          case '"'  => builder.append('\\'); builder.append('"')
          case '\\' => builder.append('\\'); builder.append('\\')
          case '\b' => builder.append('\\'); builder.append('b')
          case '\f' => builder.append('\\'); builder.append('f')
          case '\n' => builder.append('\\'); builder.append('n')
          case '\r' => builder.append('\\'); builder.append('r')
          case '\t' => builder.append('\\'); builder.append('t')
          case '/'  if b == '<' => builder.append('\\'); builder.append('/')
          case c    if (c >= '\u0000' && c <= '\u001F') ||
                       (c >= '\u007F' && c <= '\u009F') =>
                       builder.append('\\'); builder.append('u')
                       builder.append(hexToChar(c >>> 12 & 0xF))
                       builder.append(hexToChar(c >>>  8 & 0xF))
                       builder.append(hexToChar(c >>>  4 & 0xF))
                       builder.append(hexToChar(c        & 0xF))
          case c    => builder.append(c)
        }
        cs.step()
      }
      builder.append('"')
    }

    override def toJson: String = {
      val builder = UString.Builder()
      writeJson(builder)
      builder.state.toString
    }
  }
}
