//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import scala.reflect.macros._

final class StorerOps(val __ : Storer) extends AnyVal {
  def store[T](address: Long, value: T)(implicit T: Struct[T]): Unit                                     = macro StorerMacros.store[T]
  def storeArray[T](address: Long, array: Array[T], start: Int, count: Int)(implicit T: Struct[T]): Unit = macro StorerMacros.storeArray[T]
}

private[data] class StorerMacros(val c: blackbox.Context { type PrefixType <: StorerOps }) {
  import c.{ Expr, prefix }
  import c.universe._

  def store[T](address: Expr[Long], value: Expr[T])(T: Expr[Struct[T]]): Expr[Unit] =  Expr[Unit](q"$T.store($prefix.__, $address, $value)")

  def storeArray[T](address: Expr[Long], array: Expr[Array[T]], start: Expr[Int], count: Expr[Int])(T: Expr[Struct[T]]): Expr[Unit] = Expr[Unit](q"""{
    val data = $prefix.__
    val T = $T
    val xs = $array
    var p = $address
    var i = $start
    val n = i + $count
    while (i < n) {
      T.store(data, p, xs(i))
      p += T.size
      i += 1
    }
  }""")
}
