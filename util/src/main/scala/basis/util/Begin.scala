//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import scala.reflect.macros._

/** A breakable control flow context. */
class Begin {
  /** Returns this context's control-flow break exception. */
  val signal: Break = new Break

  /** Enters a dynamically-breakable scope. */
  def apply(op: Unit): Unit = macro BeginMacros.apply

  /** Exits the most recently entered scope. */
  def break(): Nothing = macro BeginMacros.break
}

private[util] class BeginMacros(val c: blackbox.Context { type PrefixType <: Begin }) {
  import c.{ Expr, mirror, prefix, weakTypeOf, WeakTypeTag }
  import c.universe._

  def apply(op: Expr[Unit]): Expr[Unit] = Expr[Unit](q"try $op catch { case signal: ${weakTypeOf[Break]} if signal eq $prefix.signal => }")
  def break(): Expr[Nothing]            = Expr[Nothing](q"throw $prefix.signal")

  implicit protected def BreakTag: WeakTypeTag[Break] = WeakTypeTag(mirror.staticClass("basis.util.Break").toType)
}
