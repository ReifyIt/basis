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
  
  def collect[A : WeakTypeTag, B]
      (these: Expr[LinearSeq[A]])
      (q: Expr[PartialFunction[A, B]])
      (builder: Expr[Builder[_, B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
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
    } (StateTag(builder))
  }
  
  def map[A : WeakTypeTag, B]
      (these: Expr[LinearSeq[A]])
      (f: Expr[A => B])
      (builder: Expr[Builder[_, B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(f.tree, Select(Ident(xs), "head") :: Nil) :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def flatMap[A : WeakTypeTag, B]
      (these: Expr[LinearSeq[A]])
      (f: Expr[A => Enumerator[B]])
      (builder: Expr[Builder[_, B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "$plus$plus$eq"), Apply(f.tree, Select(Ident(xs), "head") :: Nil) :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def filter[A : WeakTypeTag]
      (these: Expr[LinearSeq[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
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
    } (StateTag(builder))
  }
  
  def dropWhile[A : WeakTypeTag]
      (these: Expr[LinearSeq[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val b     = newTermName(fresh("builder$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("head$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
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
    } (StateTag(builder))
  }
  
  def takeWhile[A : WeakTypeTag]
      (these: Expr[LinearSeq[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
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
    } (StateTag(builder))
  }
  
  def span[A : WeakTypeTag]
      (these: Expr[LinearSeq[A]])
      (p: Expr[A => Boolean])
      (builder1: Expr[Builder[_, A]], builder2: Expr[Builder[_, A]])
    : Expr[(builder1.value.State, builder2.value.State)] = {
    val xs    = newTermName(fresh("xs$"))
    val a     = newTermName(fresh("builder$"))
    val b     = newTermName(fresh("builder$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("head$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), these.tree) ::
        ValDef(NoMods, a, TypeTree(BuilderType(builder1)), builder1.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder2)), builder2.tree) ::
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
    } (Tuple2Tag(StateTag(builder1), StateTag(builder2)))
  }
  
  def drop[A : WeakTypeTag]
      (these: Expr[LinearSeq[A]])
      (lower: Expr[Int])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val b     = newTermName(fresh("builder$"))
    val loop1 = newTermName(fresh("loop$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), lower.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
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
    } (StateTag(builder))
  }
  
  def take[A : WeakTypeTag]
      (these: Expr[LinearSeq[A]])
      (upper: Expr[Int])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), upper.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
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
    } (StateTag(builder))
  }
  
  def slice[A : WeakTypeTag]
      (these: Expr[LinearSeq[A]])
      (lower: Expr[Int], upper: Expr[Int])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val b     = newTermName(fresh("builder$"))
    val loop1 = newTermName(fresh("loop$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(Modifiers(Flag.MUTABLE), n, TypeTree(), lower.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
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
    } (StateTag(builder))
  }
  
  def zip[A : WeakTypeTag, B : WeakTypeTag]
      (these: Expr[LinearSeq[A]], those: Expr[LinearSeq[B]])
      (builder: Expr[Builder[_, (A, B)]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val ys   = newTermName(fresh("ys$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(LinearSeqType[A]), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), ys, TypeTree(LinearSeqType[B]), those.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
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
    } (StateTag(builder))
  }
  
  def ++ [A]
      (these: Expr[LinearSeq[A]], those: Expr[LinearSeq[A]])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val b = newTermName(fresh("builder$"))
    Expr {
      Block(
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        Apply(Select(Ident(b), "$plus$plus$eq"), these.tree :: Nil) ::
        Apply(Select(Ident(b), "$plus$plus$eq"), those.tree :: Nil) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  private def LinearSeqType[A : WeakTypeTag]: Type = {
    val LinearSeqTpc = mirror.staticClass("basis.collections.traversable.LinearSeq").toType
    appliedType(LinearSeqTpc, weakTypeOf[A] :: Nil)
  }
  
  private def BuilderType(builder: Expr[Builder[_, _]]): Type = builder.tree.symbol match {
    case symbol: TermSymbol if symbol.isStable => singleType(NoPrefix, symbol)
    case _ => builder.actualType
  }
  
  private def StateTag(builder: Expr[Builder[_, _]]): WeakTypeTag[builder.value.State] = {
    val StateSymbol = mirror.staticClass("basis.collections.Builder").toType.member(newTypeName("State"))
    WeakTypeTag(typeRef(BuilderType(builder), StateSymbol, Nil))
  }
  
  private def Tuple2Tag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[(A, B)] = {
    val Tuple2Tpc = mirror.staticClass("scala.Tuple2").toType
    WeakTypeTag(appliedType(Tuple2Tpc, weakTypeOf[A] :: weakTypeOf[B] :: Nil))
  }
}
