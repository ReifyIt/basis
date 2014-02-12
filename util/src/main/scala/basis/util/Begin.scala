//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

/** A breakable control flow context.
  *
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  */
class Begin {
  /** Returns this context's control-flow break exception. */
  val signal: Break = new Break

  /** Enters a dynamically-breakable scope. */
  def apply(op: Unit): Unit = macro BeginMacros.apply

  /** Exits the most recently entered scope. */
  def break(): Nothing = macro BeginMacros.break
}

private[util] object BeginMacros {
  import scala.collection.immutable.{ ::, Nil }

  def apply(c: ContextWithPre[Begin])(op: c.Expr[Unit]): c.Expr[Unit] = {
    import c.{ Expr, fresh, prefix, WeakTypeTag }
    import c.universe._
    implicit val BreakType = typeOf[Break]
    c.Expr[Unit](q"try $op catch { case signal: $BreakType if signal eq $prefix => }")
  }

  def break(c: ContextWithPre[Begin])(): c.Expr[Nothing] = {
    import c.{ Expr, prefix, WeakTypeTag }
    import c.universe._
    Expr[Nothing](Throw(Select(prefix.tree, "signal": TermName)))(WeakTypeTag.Nothing)
  }
}
