//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import scala.reflect.macros._

private[util] class UtilMacros(val c: blackbox.Context) {
  import c.{ Expr, mirror, WeakTypeTag }
  import c.universe._

  def IntToOps(a: Expr[Int]): Expr[IntOps]          = Expr[IntOps](q"new _root_.basis.util.IntOps($a)")
  def LongToOps(a: Expr[Long]): Expr[LongOps]       = Expr[LongOps](q"new _root_.basis.util.LongOps($a)")
  def FloatToOps(x: Expr[Float]): Expr[FloatOps]    = Expr[FloatOps](q"new _root_.basis.util.FloatOps($x)")
  def DoubleToOps(x: Expr[Double]): Expr[DoubleOps] = Expr[DoubleOps](q"new _root_.basis.util.DoubleOps($x)")

  def ArrowToOps[A : WeakTypeTag](left: Expr[A]): Expr[ArrowOps[A]] = Expr[ArrowOps[A]](q"new _root_.basis.util.ArrowOps($left)")

  implicit protected def IntOpsTag: WeakTypeTag[IntOps]       = WeakTypeTag(mirror.staticClass("basis.util.IntOps").toType)
  implicit protected def LongOpsTag: WeakTypeTag[LongOps]     = WeakTypeTag(mirror.staticClass("basis.util.LongOps").toType)
  implicit protected def FloatOpsTag: WeakTypeTag[FloatOps]   = WeakTypeTag(mirror.staticClass("basis.util.FloatOps").toType)
  implicit protected def DoubleOpsTag: WeakTypeTag[DoubleOps] = WeakTypeTag(mirror.staticClass("basis.util.DoubleOps").toType)

  implicit protected def ArrowOpsTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[ArrowOps[A]] =
    WeakTypeTag(appliedType(mirror.staticClass("basis.util.ArrowOps").toTypeConstructor, A.tpe :: Nil))
}
