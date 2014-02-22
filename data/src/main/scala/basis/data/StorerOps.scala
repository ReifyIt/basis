//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._
import scala.reflect.macros._

final class StorerOps(val __ : Storer) extends AnyVal {
  def store[T](address: Long, value: T)(implicit T: Struct[T]): Unit                                     = macro StorerMacros.store[T]
  def storeArray[T](address: Long, array: Array[T], start: Int, count: Int)(implicit T: Struct[T]): Unit = macro StorerMacros.storeArray[T]
}

private[data] object StorerMacros {
  def StorerToOps(c: Context)(data: c.Expr[Storer]): c.Expr[StorerOps] = {
    import c.universe._
    c.Expr[StorerOps](q"new basis.data.StorerOps($data)")
  }

  def store[T](c: ContextWithPre[StorerOps])(address: c.Expr[Long], value: c.Expr[T])(T: c.Expr[Struct[T]]): c.Expr[Unit] = {
    import c.universe._
    c.Expr[Unit](q"$T.store(${c.prefix}.__, $address, $value)")
  }

  def storeArray[T]
      (c: ContextWithPre[StorerOps])
      (address: c.Expr[Long], array: c.Expr[Array[T]], start: c.Expr[Int], count: c.Expr[Int])
      (T: c.Expr[Struct[T]])
    : c.Expr[Unit] = {
    import c.universe._
    c.Expr[Unit](q"""{
      val data = ${c.prefix}.__
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
}
