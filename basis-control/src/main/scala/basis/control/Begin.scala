/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.control

/** A breakable control flow context. */
class Begin {
  val signal: Break = new Break
  
  def apply(op: Unit): Unit = macro Begin.apply
  
  def break(): Nothing = macro Begin.break
}

private[control] object Begin {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def apply(c: Context { type PrefixType <: Begin })(op: c.Expr[Unit]): c.Expr[Unit] = {
    import c.{Expr, fresh, prefix, TypeTag}
    import c.universe._
    val signal = newTermName(fresh("signal$"))
    Expr(
      Try(
        op.tree,
        CaseDef(
          Bind(
            signal,
            Typed(
              Ident(nme.WILDCARD),
              Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), newTypeName("Break")))),
          Apply(Select(Ident(signal), "eq"), Select(prefix.tree, "signal") :: Nil),
          EmptyTree) :: Nil,
        EmptyTree))(TypeTag.Unit)
  }
  
  def break(c: Context { type PrefixType <: Begin })(): c.Expr[Nothing] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    Expr(Throw(Select(prefix.tree, "signal")))(TypeTag.Nothing)
  }
}
