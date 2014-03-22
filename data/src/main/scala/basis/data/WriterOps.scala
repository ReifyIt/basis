//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import scala.reflect.macros._

final class WriterOps(val __ : Writer) extends AnyVal {
  def write[T](value: T)(implicit T: Frame[T]): Unit                                     = macro WriterMacros.write[T]
  def writeArray[T](array: Array[T], start: Int, count: Int)(implicit T: Frame[T]): Unit = macro WriterMacros.writeArray[T]
}

private[data] class WriterMacros(val c: blackbox.Context { type PrefixType <: WriterOps }) {
  import c.{ Expr, prefix }
  import c.universe._

  def write[T](value: Expr[T])(T: Expr[Frame[T]]): Expr[Unit] = Expr[Unit](q"$T.write($prefix.__, $value)")

  def writeArray[T](array: Expr[Array[T]], start: Expr[Int], count: Expr[Int])(T: Expr[Frame[T]]): Expr[Unit] = Expr[Unit](q"""{
    val data = $prefix.__
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
