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
  import scala.reflect.macros.Context

  def apply(c: Context { type PrefixType <: Begin })(op: c.Expr[Unit]): c.Expr[Unit] = {
    import c.{ Expr, fresh, prefix, WeakTypeTag }
    import c.universe._
    val signal = newTermName(fresh("signal$"))
    Expr[Unit](
      Try(
        op.tree,
        CaseDef(
          Bind(
            signal,
            Typed(
              Ident(nme.WILDCARD),
              Select(Select(Select(Ident(nme.ROOTPKG), "basis": TermName), "util": TermName), "Break": TypeName))),
          Apply(Select(Ident(signal), "eq": TermName), Select(prefix.tree, "signal": TermName) :: Nil),
          EmptyTree) :: Nil,
        EmptyTree))(WeakTypeTag.Unit)
  }

  def break(c: Context { type PrefixType <: Begin })(): c.Expr[Nothing] = {
    import c.{ Expr, prefix, WeakTypeTag }
    import c.universe._
    Expr[Nothing](Throw(Select(prefix.tree, "signal": TermName)))(WeakTypeTag.Nothing)
  }
}
