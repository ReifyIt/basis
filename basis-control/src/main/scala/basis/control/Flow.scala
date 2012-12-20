/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.control

/** An exception signalling a break in control flow. */
class Break extends Throwable {
  override def fillInStackTrace(): Throwable = this
}

/** A breakable control flow context. */
class Flow {
  val signal: Break = new Break
  
  def apply(op: Unit): Unit = macro Flow.apply
  
  def break(): Nothing = macro Flow.break
}

private[control] object Flow {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def apply(c: Context { type PrefixType <: Flow })(op: c.Expr[Unit]): c.Expr[Unit] = {
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
  
  def break(c: Context { type PrefixType <: Flow })(): c.Expr[Nothing] = {
    import c.{Expr, prefix, TypeTag}
    import c.universe._
    Expr(Throw(Select(prefix.tree, "signal")))(TypeTag.Nothing)
  }
}
