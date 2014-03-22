//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import scala.reflect.macros._

final class ReaderOps(val __ : Reader) extends AnyVal {
  def read[T]()(implicit T: Frame[T]): T                                                  = macro ReaderMacros.read[T]
  def readArray[T](count: Int)(implicit T: Frame[T]): Array[T]                            = macro ReaderMacros.readArray[T]
  def readToArray[T](array: Array[T], start: Int, count: Int)(implicit T: Frame[T]): Unit = macro ReaderMacros.readToArray[T]
}

private[data] class ReaderMacros(val c: blackbox.Context { type PrefixType <: ReaderOps }) {
  import c.{ Expr, prefix, WeakTypeTag }
  import c.universe._

  def read[T]()(T: Expr[Frame[T]])(implicit TTag: WeakTypeTag[T]): Expr[T] = Expr[T](q"$T.read($prefix.__)")

  def readArray[T](count: Expr[T])(T: Expr[Frame[T]])(implicit TTag: WeakTypeTag[T]): Expr[Array[T]] = Expr[Array[T]](q"""{
    val data = $prefix.__
    val T = $T
    val xs = new Array[$TTag]($count)
    var i = 0
    while (i < count) {
      xs(i) = T.read(data)
      i += 1
    }
    xs
  }""")

  def readToArray[T](array: Expr[Array[T]], start: Expr[Int], count: Expr[Int])(T: Expr[Frame[T]]): Expr[Unit] = Expr[Unit](q"""{
    val data = $prefix.__
    val T = $T
    val xs = $array
    var i = $start
    val n = i + $count
    while (i < n) {
      xs(i) = T.read(data)
      i += 1
    }
  }""")

  implicit protected def ArrayTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[Array[A]] =
    WeakTypeTag(appliedType(definitions.ArrayClass.toTypeConstructor, A.tpe :: Nil))
}
