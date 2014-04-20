//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis

import scala.reflect.macros._

private[basis] class BasisMacros(val c: blackbox.Context) {
  import c.{ Expr, mirror, WeakTypeTag }
  import c.universe._

  def ElseToOps[A : WeakTypeTag, B : WeakTypeTag](self: Expr[A Else B]): Expr[ElseOps[A, B]] = Expr[ElseOps[A, B]](q"new _root_.basis.ElseOps($self)")
  def TruthToOps(self: Expr[Truth]): Expr[TruthOps]                                          = Expr[TruthOps](q"new _root_.basis.TruthOps($self)")

  def Try[A : WeakTypeTag](expr: Expr[A]): Expr[A Else Throwable] = Expr[A Else Throwable](q"""
    try _root_.basis.Bind($expr)
    catch { case e: Throwable => _root_.basis.Trap.NonFatal(e) }
  """)

  implicit protected def ElseTag[A, B](implicit A: WeakTypeTag[A], B: WeakTypeTag[B]): WeakTypeTag[A Else B] =
    WeakTypeTag(appliedType(mirror.staticClass("basis.Else").toType, A.tpe :: B.tpe :: Nil))

  implicit protected def ElseOpsTag[A, B](implicit A: WeakTypeTag[A], B: WeakTypeTag[B]): WeakTypeTag[ElseOps[A, B]] =
    WeakTypeTag(appliedType(mirror.staticClass("basis.ElseOps").toType, A.tpe :: B.tpe :: Nil))

  implicit protected def TruthOpsTag: WeakTypeTag[TruthOps]   = WeakTypeTag(mirror.staticClass("basis.TruthOps").toType)
  implicit protected def ThrowableTag: WeakTypeTag[Throwable] = WeakTypeTag(mirror.staticClass("java.lang.Throwable").toType)
}
