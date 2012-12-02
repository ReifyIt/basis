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

private[strict] final class LinearSeqMacros[C <: Context](val context: C) {
  import context.{Expr, fresh, mirror, WeakTypeTag}
  import universe._
  
  val universe: context.universe.type = context.universe
  
  def collect[A : WeakTypeTag, B, To : WeakTypeTag]
      (self: Expr[LinearSeq[A]])
      (q: Expr[PartialFunction[A, B]])
      (builder: Expr[Builder[_, B, To]])
    : Expr[To] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), self.tree) ::
        ValDef(NoMods, b, TypeTree(), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) ::
              If(
                Apply(Select(q.tree, "isDefinedAt"), Ident(x) :: Nil),
                Apply(Select(Ident(b), "$plus$eq"), Apply(q.tree, Ident(x) :: Nil) :: Nil),
                EmptyTree) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def map[A : WeakTypeTag, B, To : WeakTypeTag]
      (self: Expr[LinearSeq[A]])
      (f: Expr[A => B])
      (builder: Expr[Builder[_, B, To]])
    : Expr[To] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), self.tree) ::
        ValDef(NoMods, b, TypeTree(), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(f.tree, Select(Ident(xs), "head") :: Nil) :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def flatMap[A : WeakTypeTag, B, To : WeakTypeTag]
      (self: Expr[LinearSeq[A]])
      (f: Expr[A => Enumerator[B]])
      (builder: Expr[Builder[_, B, To]])
    : Expr[To] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), self.tree) ::
        ValDef(NoMods, b, TypeTree(), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "$plus$plus$eq"), Apply(f.tree, Select(Ident(xs), "head") :: Nil) :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def filter[A : WeakTypeTag, To : WeakTypeTag]
      (self: Expr[LinearSeq[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A, To]])
    : Expr[To] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), self.tree) ::
        ValDef(NoMods, b, TypeTree(), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) ::
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Apply(Select(Ident(b), "$plus$eq"), Ident(x) :: Nil),
                EmptyTree) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def dropWhile[A : WeakTypeTag, To : WeakTypeTag]
      (self: Expr[LinearSeq[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A, To]])
    : Expr[To] = {
    val xs    = newTermName(fresh("xs$"))
    val b     = newTermName(fresh("builder$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("head$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), self.tree) ::
        ValDef(NoMods, b, TypeTree(), builder.tree) ::
        LabelDef(loop1, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Apply(Ident(loop1), Nil),
                Apply(Select(Ident(b), "$plus$eq"), Ident(x) :: Nil))),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Select(Ident(xs), "head") :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def takeWhile[A : WeakTypeTag, To : WeakTypeTag]
      (self: Expr[LinearSeq[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A, To]])
    : Expr[To] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), self.tree) ::
        ValDef(NoMods, b, TypeTree(), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
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
  
  def span[A : WeakTypeTag, To : WeakTypeTag]
      (self: Expr[LinearSeq[A]])
      (p: Expr[A => Boolean])
      (builderA: Expr[Builder[_, A, To]], builderB: Expr[Builder[_, A, To]])
    : Expr[(To, To)] = {
    val xs    = newTermName(fresh("xs$"))
    val a     = newTermName(fresh("builder$"))
    val b     = newTermName(fresh("builder$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("head$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), self.tree) ::
        ValDef(NoMods, a, TypeTree(), builderA.tree) ::
        ValDef(NoMods, b, TypeTree(), builderB.tree) ::
        LabelDef(loop1, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Block(
                  Apply(Select(Ident(a), "$plus$eq"), Ident(x) :: Nil) :: Nil,
                  Apply(Ident(loop1), Nil)),
                Apply(Select(Ident(b), "$plus$eq"), Ident(x) :: Nil))),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Select(Ident(xs), "head") :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        ApplyConstructor(
          Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Tuple2")),
          Select(Ident(a), "state") :: Select(Ident(b), "state") :: Nil))
    } (Tuple2Tag(weakTypeTag[To], weakTypeTag[To]))
  }
  
  def drop[A : WeakTypeTag, To : WeakTypeTag]
      (self: Expr[LinearSeq[A]])
      (lower: Expr[Int])
      (builder: Expr[Builder[_, A, To]])
    : Expr[To] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val b     = newTermName(fresh("builder$"))
    val loop1 = newTermName(fresh("loop$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), lower.tree) ::
        ValDef(NoMods, b, TypeTree(), builder.tree) ::
        LabelDef(loop1, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            If(
              Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
              Block(
                Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
                Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                Apply(Ident(loop1), Nil)),
              EmptyTree),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Select(Ident(xs), "head") :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def take[A : WeakTypeTag, To : WeakTypeTag]
      (self: Expr[LinearSeq[A]])
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
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), upper.tree) ::
        ValDef(NoMods, b, TypeTree(), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            If(
              Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
              Block(
                Apply(Select(Ident(b), "$plus$eq"), Select(Ident(xs), "head") :: Nil) ::
                Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
                Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                Apply(Ident(loop), Nil)),
              EmptyTree),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def slice[A : WeakTypeTag, To : WeakTypeTag]
      (self: Expr[LinearSeq[A]])
      (lower: Expr[Int], upper: Expr[Int])
      (builder: Expr[Builder[_, A, To]])
    : Expr[To] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val b     = newTermName(fresh("builder$"))
    val loop1 = newTermName(fresh("loop$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), self.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(Modifiers(Flag.MUTABLE), n, TypeTree(), lower.tree) ::
        ValDef(NoMods, b, TypeTree(), builder.tree) ::
        LabelDef(loop1, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            If(
              Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
              Block(
                Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
                Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                Apply(Ident(loop1), Nil)),
              EmptyTree),
            EmptyTree)) ::
        Assign(Ident(n), upper.tree) ::
        LabelDef(loop2, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            If(
              Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
              Block(
                Apply(Select(Ident(b), "$plus$eq"), Select(Ident(xs), "head") :: Nil) ::
                Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
                Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                Apply(Ident(loop2), Nil)),
              EmptyTree),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def zip[A : WeakTypeTag, B : WeakTypeTag, To : WeakTypeTag]
      (these: Expr[LinearSeq[A]], those: Expr[LinearSeq[B]])
      (builder: Expr[Builder[_, (A, B), To]])
    : Expr[To] = {
    val xs   = newTermName(fresh("xs$"))
    val ys   = newTermName(fresh("ys$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), ys, TypeTree(LinearSeqType[B]), those.tree) ::
        ValDef(NoMods, b, TypeTree(), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            If(
              Select(Select(Ident(ys), "isEmpty"), "unary_$bang"),
              Block(
                Apply(
                  Select(Ident(b), "$plus$eq"),
                  ApplyConstructor(
                    Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Tuple2")),
                    Select(Ident(xs), "head") :: Select(Ident(ys), "head") :: Nil) :: Nil) ::
                Assign(Ident(xs), Select(Ident(xs), "tail")) ::
                Assign(Ident(ys), Select(Ident(ys), "tail")) :: Nil,
                Apply(Ident(loop), Nil)),
              EmptyTree),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  def ++ [A, To : WeakTypeTag]
      (these: Expr[LinearSeq[A]], those: Expr[LinearSeq[A]])
      (builder: Expr[Builder[_, A, To]])
    : Expr[To] = {
    val b = newTermName(fresh("builder$"))
    Expr {
      Block(
        ValDef(NoMods, b, TypeTree(), builder.tree) ::
        Apply(Select(Ident(b), "$plus$plus$eq"), these.tree :: Nil) ::
        Apply(Select(Ident(b), "$plus$plus$eq"), those.tree :: Nil) :: Nil,
        Select(Ident(b), "state"))
    } (weakTypeTag[To])
  }
  
  private def LinearSeqType[A : WeakTypeTag]: Type =
    appliedType(mirror.staticClass("basis.collections.traversable.LinearSeq").toType, weakTypeOf[A] :: Nil)
  
  implicit private def Tuple2Tag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[(A, B)] =
    WeakTypeTag(appliedType(mirror.staticClass("scala.Tuple2").toType, weakTypeOf[A] :: weakTypeOf[B] :: Nil))
}
