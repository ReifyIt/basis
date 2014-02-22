//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._
import scala.reflect.macros._

final class ReaderOps(val __ : Reader) extends AnyVal {
  def read[T]()(implicit T: Frame[T]): T                                                  = macro ReaderMacros.read[T]
  def readArray[T](count: Int)(implicit T: Frame[T]): Array[T]                            = macro ReaderMacros.readArray[T]
  def readToArray[T](array: Array[T], start: Int, count: Int)(implicit T: Frame[T]): Unit = macro ReaderMacros.readToArray[T]
}

private[data] object ReaderMacros {
  def ReaderToOps(c: Context)(data: c.Expr[Reader]): c.Expr[ReaderOps] = {
    import c.universe._
    c.Expr[ReaderOps](q"new basis.data.ReaderOps($data)")
  }

  def read[T: c.WeakTypeTag](c: ContextWithPre[ReaderOps])()(T: c.Expr[Frame[T]]): c.Expr[T] = {
    import c.universe._
    c.Expr[T](q"$T.read(${c.prefix}.__)")
  }

  def readArray[T]
      (c: ContextWithPre[ReaderOps])
      (count: c.Expr[T])
      (T: c.Expr[Frame[T]])
      (implicit TTag: c.WeakTypeTag[T])
    : c.Expr[Array[T]] = {
    import c.universe._
    c.Expr[Array[T]](q"""{
      val data = ${c.prefix}.__
      val T = $T
      val xs = new Array[$TTag]($count)
      var i = 0
      while (i < count) {
        xs(i) = T.read(data)
        i += 1
      }
      xs
    }""")
  }

  def readToArray[T]
      (c: ContextWithPre[ReaderOps])
      (array: c.Expr[Array[T]], start: c.Expr[Int], count: c.Expr[Int])
      (T: c.Expr[Frame[T]])
    : c.Expr[Unit] = {
    import c.universe._
    c.Expr[Unit](q"""{
      val data = ${c.prefix}.__
      val T = $T
      val xs = $array
      var i = $start
      val n = i + $count
      while (i < n) {
        xs(i) = T.read(data)
        i += 1
      }
    }""")
  }
}
