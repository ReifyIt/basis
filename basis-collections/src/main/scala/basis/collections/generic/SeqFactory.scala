/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package generic

import traversable._

trait SeqFactory[CC[_]] extends BuilderFactory[CC] {
  def fill[A](count: Int)(element: => A): CC[A] =
    macro SeqFactory.fill[CC, A]
  
  def tabulate[A](count: Int)(f: Int => A): CC[A] =
    macro SeqFactory.tabulate[CC, A]
  
  def iterate[A](start: A, count: Int)(f: A => A): CC[A] =
    macro SeqFactory.iterate[CC, A]
}

private[generic] object SeqFactory {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  def fill[CC[_], A]
      (c: Context { type PrefixType <: SeqFactory[CC] })
      (count: c.Expr[Int])
      (element: c.Expr[A])
      (implicit CCTag: c.WeakTypeTag[CC[_]], ATag: c.WeakTypeTag[A])
    : c.Expr[CC[A]] = {
    import c.{Expr, fresh, prefix, WeakTypeTag}
    import c.universe._
    val i    = newTermName(fresh("i$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    val builder = TypeApply(Select(prefix.tree, "Builder"), TypeTree(ATag.tpe) :: Nil)
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), count.tree) ::
        ValDef(NoMods, b, TypeTree(), Apply(Select(builder, "expect"), Ident(i) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$greater"), Literal(Constant(0)) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), element.tree :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$minus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (WeakTypeTag(appliedType(CCTag.tpe, ATag.tpe :: Nil)))
  }
  
  def tabulate[CC[_], A]
      (c: Context { type PrefixType <: SeqFactory[CC] })
      (count: c.Expr[Int])
      (f: c.Expr[Int => A])
      (implicit CCTag: c.WeakTypeTag[CC[_]], ATag: c.WeakTypeTag[A])
    : c.Expr[CC[A]] = {
    import c.{Expr, fresh, prefix, WeakTypeTag}
    import c.universe._
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    val builder = TypeApply(Select(prefix.tree, "Builder"), TypeTree(ATag.tpe) :: Nil)
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), count.tree) ::
        ValDef(NoMods, b, TypeTree(), Apply(Select(builder, "expect"), Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(f.tree, Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (WeakTypeTag(appliedType(CCTag.tpe, ATag.tpe :: Nil)))
  }
  
  def iterate[CC[_], A]
      (c: Context { type PrefixType <: SeqFactory[CC] })
      (start: c.Expr[A], count: c.Expr[Int])
      (f: c.Expr[A => A])
      (implicit CCTag: c.WeakTypeTag[CC[_]], ATag: c.WeakTypeTag[A])
    : c.Expr[CC[A]] = {
    import c.{Expr, fresh, prefix, WeakTypeTag}
    import c.universe._
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("builder$"))
    val a    = newTermName(fresh("a$"))
    val i    = newTermName(fresh("i$"))
    val loop = newTermName(fresh("loop$"))
    val builder = TypeApply(Select(prefix.tree, "Builder"), TypeTree(ATag.tpe) :: Nil)
    Expr {
      Block(
        ValDef(NoMods, n, TypeTree(), count.tree) ::
        ValDef(NoMods, b, TypeTree(), Apply(Select(builder, "expect"), Ident(n) :: Nil)) ::
        If(
          Apply(Select(Ident(n), "$greater"), Literal(Constant(0)) :: Nil),
          Block(
            ValDef(Modifiers(Flag.MUTABLE), a, TypeTree(), start.tree) ::
            Apply(Select(Ident(b), "$plus$eq"), Ident(a) :: Nil) ::
            ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(1))) :: Nil,
            LabelDef(loop, Nil,
              If(
                Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
                Block(
                  Assign(Ident(a), Apply(f.tree, Ident(a) :: Nil)) ::
                  Apply(Select(Ident(b), "$plus$eq"), Ident(a) :: Nil) ::
                  Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree))),
          EmptyTree) :: Nil,
        Select(Ident(b), "state"))
    } (WeakTypeTag(appliedType(CCTag.tpe, ATag.tpe :: Nil)))
  }
}
