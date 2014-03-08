//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import scala.reflect.macros._

private[util] class UtilMacros(val c: blackbox.Context) {
  import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
  import c.universe._

  def IntToOps(a: Expr[Int]): Expr[IntOps]          = Expr[IntOps](q"new _root_.basis.util.IntOps($a)")
  def LongToOps(a: Expr[Long]): Expr[LongOps]       = Expr[LongOps](q"new _root_.basis.util.LongOps($a)")
  def FloatToOps(x: Expr[Float]): Expr[FloatOps]    = Expr[FloatOps](q"new _root_.basis.util.FloatOps($x)")
  def DoubleToOps(x: Expr[Double]): Expr[DoubleOps] = Expr[DoubleOps](q"new _root_.basis.util.DoubleOps($x)")

  def ArrowToOps[A : WeakTypeTag](left: Expr[A]): Expr[ArrowOps[A]]                          = Expr[ArrowOps[A]](q"new _root_.basis.util.ArrowOps($left)")
  def ElseToOps[A : WeakTypeTag, B : WeakTypeTag](self: Expr[A Else B]): Expr[ElseOps[A, B]] = Expr[ElseOps[A, B]](q"new _root_.basis.util.ElseOps($self)")
  def TruthToOps(self: Expr[Truth]): Expr[TruthOps]                                          = Expr[TruthOps](q"new _root_.basis.util.TruthOps($self)")

  def StringBuilderToShower(builder: Expr[java.lang.StringBuilder]): Expr[StringShower] = Expr[StringShower](q"new _root_.basis.util.StringShower($builder)")

  def Try[A : WeakTypeTag](expr: Expr[A]): Expr[A Else Throwable] = Expr[A Else Throwable](q"""
    try _root_.basis.util.Bind($expr)
    catch { case e: Throwable => _root_.basis.util.Trap.NonFatal(e) }
  """)

  implicit protected def IntOpsTag: WeakTypeTag[IntOps]       = WeakTypeTag(mirror.staticClass("basis.util.IntOps").toType)
  implicit protected def LongOpsTag: WeakTypeTag[LongOps]     = WeakTypeTag(mirror.staticClass("basis.util.LongOps").toType)
  implicit protected def FloatOpsTag: WeakTypeTag[FloatOps]   = WeakTypeTag(mirror.staticClass("basis.util.FloatOps").toType)
  implicit protected def DoubleOpsTag: WeakTypeTag[DoubleOps] = WeakTypeTag(mirror.staticClass("basis.util.DoubleOps").toType)

  implicit protected def ArrowOpsTag[A : WeakTypeTag]: WeakTypeTag[ArrowOps[A]]                   = WeakTypeTag(appliedType(mirror.staticClass("basis.util.ArrowOps").toType, weakTypeOf[A] :: Nil))
  implicit protected def ElseOpsTag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[ElseOps[A, B]] = WeakTypeTag(appliedType(mirror.staticClass("basis.util.ElseOps").toType, weakTypeOf[A] :: weakTypeOf[B] :: Nil))
  implicit protected def ElseTag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[A Else B]         = WeakTypeTag(appliedType(mirror.staticClass("basis.util.Else").toType, weakTypeOf[A] :: weakTypeOf[B] :: Nil))

  implicit protected def TruthOpsTag: WeakTypeTag[TruthOps]         = WeakTypeTag(mirror.staticClass("basis.util.TruthOps").toType)
  implicit protected def StringShowerTag: WeakTypeTag[StringShower] = WeakTypeTag(mirror.staticClass("basis.util.StringShower").toType)
  implicit protected def ThrowableTag: WeakTypeTag[Throwable]       = WeakTypeTag(mirror.staticClass("java.lang.Throwable").toType)
}
