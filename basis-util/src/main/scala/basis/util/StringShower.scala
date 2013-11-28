//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import scala.annotation._

private[basis] final class StringShower(val builder: java.lang.StringBuilder) extends AnyVal {
  def show(value: Any): java.lang.StringBuilder = value match {
    case s: String => showString(s)
    case x: Float => showFloat(x)
    case a: Long => showLong(a)
    case _ => builder.append(value)
  }

  private def showString(s: String): java.lang.StringBuilder = {
    def hexToChar(x: Int): Char = (if (x < 10) '0' + x else 'A' + (x - 10)).toChar
    builder.append('\"')
    var i = 0
    val n = s.length
    while (i < n) {
      (s.codePointAt(i): @switch) match {
        case '\b' => builder.append('\\').append('b')
        case '\t' => builder.append('\\').append('t')
        case '\n' => builder.append('\\').append('n')
        case '\f' => builder.append('\\').append('f')
        case '\r' => builder.append('\\').append('r')
        case '\"' => builder.append('\\').append('\"')
        case '\\' => builder.append('\\').append('\\')
        case c    if (c >= '\u0000' && c <= '\u001F') ||
                     (c >= '\u007F' && c <= '\u009F') =>
                     builder.append('\\').append('u').
                             append(hexToChar(c >>> 12 & 0xF)).
                             append(hexToChar(c >>>  8 & 0xF)).
                             append(hexToChar(c >>>  4 & 0xF)).
                             append(hexToChar(c        & 0xF))
        case c    => builder.appendCodePoint(c)
      }
      i = s.offsetByCodePoints(i, 1)
    }
    builder.append('\"')
  }

  private def showFloat(x: Float): java.lang.StringBuilder =
    builder.append(x).append('F')

  private def showLong(a: Long): java.lang.StringBuilder =
    builder.append(a).append('L')
}

private[util] object StringShower {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  def StringBuilderToShower(c: Context)(builder: c.Expr[java.lang.StringBuilder]): c.Expr[StringShower] = {
    import c.{ Expr, mirror, WeakTypeTag }
    import c.universe._
    implicit val StringShowerTag =
      WeakTypeTag[StringShower](mirror.staticClass("basis.util.StringShower").toType)
    Expr[StringShower](
      Apply(
        Select(New(TypeTree(weakTypeOf[StringShower])), nme.CONSTRUCTOR),
        builder.tree :: Nil))
  }
}
