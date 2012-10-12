/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.util

import basis._

private[basis] object RangeMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.api.Universe
  import scala.reflect.macros.Context
  
  private def BasisUtilRange(u: Universe): u.Tree = {
    import u._
    Select(Select(Select(Ident(nme.ROOTPKG), newTermName("basis")), newTermName("util")), newTypeName("Range"))
  }
  
  def by(c: Context { type PrefixType <: Range })(step: c.Expr[Int]): c.Expr[Range] = {
    import c.universe._
    c.Expr {
      Apply(Select(New(BasisUtilRange(c.universe)), nme.CONSTRUCTOR),
        c.prefix.tree match {
          case Apply(_, start :: end :: _ :: isInclusive :: Nil) =>
            start :: end :: step.tree :: isInclusive :: Nil
          case range =>
            Select(range, "start")       ::
            Select(range, "end")         ::
            step.tree                                 ::
            Select(range, "isInclusive") ::
            Nil
        })
    } (WeakTypeTag.Nothing)
  }
  
  def foreach[U : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[Int => U])
    : c.Expr[Unit] = {
    import c.universe._
    val Apply(_, range :: Nil) = c.prefix.tree
    c.Expr {
      range match {
        case Apply(_, start :: end :: step :: isInclusive :: Nil) => step match {
          case Literal(Constant(inc: Int)) =>
            if      (inc > 0) foreachForward(c)(f.tree)(start, end, Literal(Constant( inc)), isInclusive)
            else if (inc < 0) foreachReverse(c)(f.tree)(start, end, Literal(Constant(-inc)), isInclusive)
            else c.abort(step.pos, "Range step must be non-zero.")
          case _ => foreachVirtual(c)(f.tree)
        }
        case _ => foreachVirtual(c)(f.tree)
      }
    } (WeakTypeTag.Unit)
  }
  
  private def foreachForward
      (c: Context)
      (f: c.Tree)
      (start: c.Tree, end: c.Tree, step: c.Tree, isInclusive: c.Tree)
    : c.Tree = {
    import c.universe._
    isInclusive match {
      case Literal(Constant(inclusive: Boolean)) =>
        val i    = c.fresh(newTermName("i$"))
        val n    = c.fresh(newTermName("n$"))
        val loop = c.fresh(newTermName("loop$"))
        Block(
          ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), start) ::
          ValDef(Modifiers(),             n, TypeTree(), end)   ::
          Nil,
          LabelDef(loop, Nil, If(
            Apply(Select(Ident(i), if (inclusive) "$less$eq" else "$less"), Ident(n) :: Nil),
            Block(
              Apply(f, Ident(i) :: Nil)                                       ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), step :: Nil)) ::
              Nil,
              Apply(Ident(loop), Nil)
            ),
            EmptyTree)
          )
        )
      case _ => foreachVirtual(c)(f)
    }
  }
  
  private def foreachReverse
      (c: Context)
      (f: c.Tree)
      (start: c.Tree, end: c.Tree, step: c.Tree, isInclusive: c.Tree)
    : c.Tree = {
    import c.universe._
    isInclusive match {
      case Literal(Constant(inclusive: Boolean)) =>
        val i    = c.fresh(newTermName("i$"))
        val n    = c.fresh(newTermName("n$"))
        val loop = c.fresh(newTermName("loop$"))
        Block(
          ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), start) ::
          ValDef(Modifiers(),             n, TypeTree(), end)   ::
          Nil,
          LabelDef(loop, Nil, If(
            Apply(Select(Ident(i), if (inclusive) "$greater$eq" else "$greater"), Ident(n) :: Nil),
            Block(
              Apply(f, Ident(i) :: Nil)                                        ::
              Assign(Ident(i), Apply(Select(Ident(i), "$minus"), step :: Nil)) ::
              Nil,
              Apply(Ident(loop), Nil)
            ),
            EmptyTree)
          )
        )
      case _ => foreachVirtual(c)(f)
    }
  }
  
  private def foreachVirtual(c: Context)(f: c.Tree): c.Tree = {
    import c.universe._
    val Apply(_, range :: Nil) = c.prefix.tree
    Apply(Apply(Select(Ident("basis"), "traverse"), range :: Nil), f :: Nil)
  }
}
