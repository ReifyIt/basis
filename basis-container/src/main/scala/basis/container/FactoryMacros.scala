/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

private[basis] object FactoryMacros {
  import scala.collection.immutable.{List, ::, Nil}
  import scala.reflect.macros.Context
  
  def apply[A]
      (c: Context)
      (xs: c.Expr[A]*)
      (buffer: c.Expr[Buffer[_, A]])
    : c.Expr[buffer.value.State] = {
    import c.universe._
    c.Expr(Select(
      xs.foldLeft
        (Apply(Select(buffer.tree, "expect"), Literal(Constant(xs.length)) :: Nil))
        ((b: c.Tree, x: c.Expr[A]) => Apply(Select(b, "$plus$eq"), x.tree :: Nil)),
      "check")
    ) (WeakTypeTag.Nothing)
  }
  
  def fill[A]
      (c: Context)
      (n: c.Expr[Int])
      (element: c.Expr[A])
      (buffer: c.Expr[Buffer[_, A]])
    : c.Expr[buffer.value.State] = {
    import c.universe._
    val b    = c.fresh(newTermName("buffer$"))
    val i    = c.fresh(newTermName("i$"))
    val loop = c.fresh(newTermName("loop$"))
    c.Expr(Block(
      ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), n.tree) ::
      ValDef(Modifiers(), b, TypeTree(), Apply(Select(buffer.tree, "expect"), Ident(i) :: Nil)) ::
      LabelDef(loop, Nil, If(
        Apply(Select(Ident(i), "$greater"), Literal(Constant(0)) :: Nil),
        Block(
          Apply(Select(Ident(b), "$plus$eq"), element.tree :: Nil) ::
          Assign(Ident(i), Apply(Select(Ident(i), "$minus"), Literal(Constant(1)) :: Nil)) ::
          Nil,
          Apply(Ident(loop), Nil)
        ),
        EmptyTree)
      ) :: Nil,
      Select(Ident(b), "check")
    )) (WeakTypeTag.Nothing)
  }
  
  def tabulate[A]
      (c: Context)
      (n: c.Expr[Int])
      (f: c.Expr[Int => A])
      (buffer: c.Expr[Buffer[_, A]])
    : c.Expr[buffer.value.State] = {
    import c.universe._
    val b    = c.fresh(newTermName("buffer$"))
    val i    = c.fresh(newTermName("i$"))
    val k    = c.fresh(newTermName("n$"))
    val loop = c.fresh(newTermName("loop$"))
    c.Expr(Block(
      ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
      ValDef(Modifiers(), k, TypeTree(), n.tree) ::
      ValDef(Modifiers(), b, TypeTree(), Apply(Select(buffer.tree, "expect"), Ident(k) :: Nil)) ::
      LabelDef(loop, Nil, If(
        Apply(Select(Ident(i), "$less"), Ident(k) :: Nil),
        Block(
          Apply(Select(Ident(b), "$plus$eq"), Apply(f.tree, Ident(i) :: Nil) :: Nil) ::
          Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
          Nil,
          Apply(Ident(loop), Nil)
        ),
        EmptyTree)
      ) :: Nil,
      Select(Ident(b), "check")
    )) (WeakTypeTag.Nothing)
  }
}
