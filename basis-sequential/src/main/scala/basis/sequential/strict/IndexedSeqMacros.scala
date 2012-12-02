/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential
package strict

import basis.collections._
import basis.collections.traversable._

import scala.collection.immutable.{::, Nil}
import scala.reflect.macros.Context

private[strict] final class IndexedSeqMacros[C <: Context](val context: C) {
  import context.{Expr, fresh, mirror, WeakTypeTag}
  import universe._
  
  val universe: context.universe.type = context.universe
  
  def collect[A, B, To : WeakTypeTag]
      (self: Expr[IndexedSeq[A]])
      (q: Expr[PartialFunction[A, B]])
      (builder: Expr[Builder[_, B, To]])
    : Expr[To] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) ::
              If(
                Apply(Select(q.tree, "isDefinedAt"), Ident(x) :: Nil),
                Apply(Select(Ident(b), "$plus$eq"), Apply(q.tree, Ident(x) :: Nil) :: Nil),
                EmptyTree) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def map[A, B, To : WeakTypeTag]
      (self: Expr[IndexedSeq[A]])
      (f: Expr[A => B])
      (builder: Expr[Builder[_, B, To]])
    : Expr[To] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(), Apply(Select(builder.tree, "expect"), Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(f.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def flatMap[A, B, To : WeakTypeTag]
      (self: Expr[IndexedSeq[A]])
      (f: Expr[A => Enumerator[B]])
      (builder: Expr[Builder[_, B, To]])
    : Expr[To] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$plus$eq"), Apply(f.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def filter[A, To : WeakTypeTag]
      (self: Expr[IndexedSeq[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A, To]])
    : Expr[To] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) ::
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Apply(Select(Ident(b), "$plus$eq"), Ident(x) :: Nil),
                EmptyTree) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def dropWhile[A, To : WeakTypeTag]
      (self: Expr[IndexedSeq[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A, To]])
    : Expr[To] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val b     = newTermName(fresh("builder$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("x$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(), builder.tree) ::
        LabelDef(loop1, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Apply(Ident(loop1), Nil),
                Apply(Select(Ident(b), "$plus$eq"), Ident(x) :: Nil))),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def takeWhile[A, To : WeakTypeTag]
      (self: Expr[IndexedSeq[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A, To]])
    : Expr[To] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Block(
                  Apply(Select(Ident(b), "$plus$eq"), Ident(x) :: Nil) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def span[A, To : WeakTypeTag]
      (self: Expr[IndexedSeq[A]])
      (p: Expr[A => Boolean])
      (builderA: Expr[Builder[_, A, To]], builderB: Expr[Builder[_, A, To]])
    : Expr[(To, To)] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val a     = newTermName(fresh("builder$"))
    val b     = newTermName(fresh("builder$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("x$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, a, TypeTree(), builderA.tree) ::
        ValDef(NoMods, b, TypeTree(), builderB.tree) ::
        LabelDef(loop1, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Block(
                  Apply(Select(Ident(a), "$plus$eq"), Ident(x) :: Nil) :: Nil,
                  Apply(Ident(loop1), Nil)),
                Apply(Select(Ident(b), "$plus$eq"), Ident(x) :: Nil))),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        ApplyConstructor(
          Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Tuple2")),
          Select(Ident(a), "state") :: Select(Ident(b), "state") :: Nil))
    } (Tuple2Tag(weakTypeTag[To], weakTypeTag[To]))
  }
  
  def drop[A, To : WeakTypeTag]
      (self: Expr[IndexedSeq[A]])
      (lower: Expr[Int])
      (builder: Expr[Builder[_, A, To]])
    : Expr[To] = {
    val xs   = newTermName(fresh("xs$"))
    val n    = newTermName(fresh("n$"))
    val i    = newTermName(fresh("i$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          min(max(Literal(Constant(0)), lower.tree), Ident(n))) ::
        ValDef(NoMods, b, TypeTree(),
          Apply(Select(builder.tree, "expect"), Apply(Select(Ident(n), "$minus"), Ident(i) :: Nil) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def take[A, To : WeakTypeTag]
      (self: Expr[IndexedSeq[A]])
      (upper: Expr[Int])
      (builder: Expr[Builder[_, A, To]])
    : Expr[To] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), min(upper.tree, Select(Ident(xs), "length"))) ::
        ValDef(NoMods, b, TypeTree(), Apply(Select(builder.tree, "expect"), Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def slice[A, To : WeakTypeTag]
      (self: Expr[IndexedSeq[A]])
      (lower: Expr[Int], upper: Expr[Int])
      (builder: Expr[Builder[_, A, To]])
    : Expr[To] = {
    val xs   = newTermName(fresh("xs$"))
    val n    = newTermName(fresh("n$"))
    val i    = newTermName(fresh("i$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(NoMods, n, TypeTree(), 
          min(max(Literal(Constant(0)), upper.tree), Select(Ident(xs), "length"))) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          min(max(Literal(Constant(0)), lower.tree), Ident(n))) ::
        ValDef(NoMods, b, TypeTree(),
          Apply(Select(builder.tree, "expect"), Apply(Select(Ident(n), "$minus"), Ident(i) :: Nil) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def reverse[A, To : WeakTypeTag]
      (self: Expr[IndexedSeq[A]])
      (builder: Expr[Builder[_, A, To]])
    : Expr[To] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), self.tree) ::
        ValDef(NoMods, i, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(), Apply(Select(builder.tree, "expect"), Ident(i) :: Nil)) ::
        Assign(Ident(i), Apply(Select(Ident(i), "$minus"), Literal(Constant(1)) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$greater$eq"), Literal(Constant(0)) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$minus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def zip[A, B, To : WeakTypeTag]
      (these: Expr[IndexedSeq[A]], those: Expr[IndexedSeq[B]])
      (builder: Expr[Builder[_, (A, B), To]])
    : Expr[To] = {
    val xs   = newTermName(fresh("xs$"))
    val ys   = newTermName(fresh("ys$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, ys, TypeTree(), those.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), min(Select(Ident(xs), "length"), Select(Ident(ys), "length"))) ::
        ValDef(NoMods, b, TypeTree(), Apply(Select(builder.tree, "expect"), Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(
                Select(Ident(b), "$plus$eq"),
                ApplyConstructor(
                  Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Tuple2")),
                  Apply(Ident(xs), Ident(i) :: Nil) :: Apply(Ident(ys), Ident(i) :: Nil) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def ++ [A, To : WeakTypeTag]
      (these: Expr[IndexedSeq[A]], those: Expr[IndexedSeq[A]])
      (builder: Expr[Builder[_, A, To]])
    : Expr[To] = {
    val xs = newTermName(fresh("xs$"))
    val ys = newTermName(fresh("ys$"))
    val b  = newTermName(fresh("builder$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, ys, TypeTree(), those.tree) ::
        ValDef(NoMods, b, TypeTree(),
          Apply(Select(builder.tree, "expect"),
            Apply(Select(Select(Ident(xs), "length"), "$plus"), Select(Ident(ys), "length") :: Nil) :: Nil)) ::
        Apply(Select(Ident(b), "$plus$plus$eq"), Ident(xs) :: Nil) ::
        Apply(Select(Ident(b), "$plus$plus$eq"), Ident(ys) :: Nil) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  private def max(x: Tree, y: Tree): Tree =
    Apply(Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "max"), x :: y :: Nil)
  
  private def min(x: Tree, y: Tree): Tree =
    Apply(Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "min"), x :: y :: Nil)
  
  implicit private def Tuple2Tag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[(A, B)] =
    WeakTypeTag(appliedType(mirror.staticClass("scala.Tuple2").toType, weakTypeOf[A] :: weakTypeOf[B] :: Nil))
}