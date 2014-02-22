//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._
import scala.reflect.macros._

final class WriterOps(val __ : Writer) extends AnyVal {
  def write[T](value: T)(implicit T: Frame[T]): Unit                                     = macro WriterMacros.write[T]
  def writeArray[T](array: Array[T], start: Int, count: Int)(implicit T: Frame[T]): Unit = macro WriterMacros.writeArray[T]
}

private[data] object WriterMacros {
  def WriterToOps(c: Context)(data: c.Expr[Writer]): c.Expr[WriterOps] = {
    import c.universe._
    c.Expr[WriterOps](q"new basis.data.WriterOps($data)")
  }

  def write[T](c: ContextWithPre[WriterOps])(value: c.Expr[T])(T: c.Expr[Frame[T]]): c.Expr[Unit] = {
    import c.universe._
    c.Expr[Unit](q"$T.write(${c.prefix}.__, $value)")
  }

  def writeArray[T]
      (c: ContextWithPre[WriterOps])
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
        T.write(data, xs(i))
        i += 1
      }
    }""")
  }
}
