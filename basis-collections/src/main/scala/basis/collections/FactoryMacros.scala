/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

import scala.collection.immutable.{::, Nil}
import scala.reflect.macros.Context

private[collections] object FactoryMacros {
  def apply[A]
      (c: Context)
      (xs: c.Expr[A]*)
      (buffer: c.Expr[Buffer[_, A]])
    : c.Expr[buffer.value.State] =
    new FactoryMacros[c.type](c).apply[A](xs: _*)(buffer)
  
  def fill[A]
      (c: Context)
      (count: c.Expr[Int])
      (element: c.Expr[A])
      (buffer: c.Expr[Buffer[_, A]])
    : c.Expr[buffer.value.State] =
    new FactoryMacros[c.type](c).fill[A](count)(element)(buffer)
  
  def tabulate[A]
      (c: Context)
      (count: c.Expr[Int])
      (f: c.Expr[Int => A])
      (buffer: c.Expr[Buffer[_, A]])
    : c.Expr[buffer.value.State] =
    new FactoryMacros[c.type](c).tabulate[A](count)(f)(buffer)
  
  def iterate[A]
      (c: Context)
      (start: c.Expr[A], count: c.Expr[Int])
      (f: c.Expr[A => A])
      (buffer: c.Expr[Buffer[_, A]])
    : c.Expr[buffer.value.State] =
    new FactoryMacros[c.type](c).iterate[A](start, count)(f)(buffer)
}

private[collections] final class FactoryMacros[C <: Context](val context: C) {
  import context.{Expr, fresh, WeakTypeTag}
  import universe._
  
  val universe: context.universe.type = context.universe
  
  def apply[A]
      (xs: Expr[A]*)
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    var b = Apply(Select(buffer.tree, "expect"), Literal(Constant(xs.length)) :: Nil)
    val iter = xs.iterator
    while (iter.hasNext) b = Apply(Select(b, "$plus$eq"), iter.next().tree :: Nil)
    Expr(Select(b, "state"))(BufferStateTag(buffer))
  }
  
  def fill[A]
      (count: Expr[Int])
      (element: Expr[A])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val i    = newTermName(fresh("i$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), count.tree) ::
        ValDef(NoMods, b, TypeTree(), Apply(Select(buffer.tree, "expect"), Ident(i) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$greater"), Literal(Constant(0)) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), element.tree :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$minus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (BufferStateTag(buffer))
  }
  
  def tabulate[A]
      (count: Expr[Int])
      (f: Expr[Int => A])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), count.tree) ::
        ValDef(NoMods, b, TypeTree(), Apply(Select(buffer.tree, "expect"), Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(f.tree, Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (BufferStateTag(buffer))
  }
  
  def iterate[A]
      (start: Expr[A], count: Expr[Int])
      (f: Expr[A => A])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("buffer$"))
    val a    = newTermName(fresh("a$"))
    val i    = newTermName(fresh("i$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, n, TypeTree(), count.tree) ::
        ValDef(NoMods, b, TypeTree(), Apply(Select(buffer.tree, "expect"), Ident(n) :: Nil)) ::
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
    } (BufferStateTag(buffer))
  }
  
  private def BufferType(buffer: Expr[Buffer[_, _]]): Type = {
    buffer.actualType.termSymbol match {
      case sym: TermSymbol if sym.isStable => singleType(NoPrefix, sym)
      case _ => buffer.actualType
    }
  }
  
  private def BufferStateTag(buffer: Expr[Buffer[_, _]]): WeakTypeTag[buffer.value.State] =
    WeakTypeTag(typeRef(BufferType(buffer), buffer.actualType.member(newTypeName("State")), Nil))
}
