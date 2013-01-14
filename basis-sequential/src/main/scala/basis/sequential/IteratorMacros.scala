/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._
import basis.control._

import scala.collection.immutable.{::, Nil}
import scala.reflect.macros.Context

/** Iterator operations macro implementations.
  * 
  * @author Chris Sachs
  */
private[sequential] final class IteratorMacros[C <: Context](val context: C) {
  import context.{Expr, fresh, mirror, WeakTypeTag}
  import universe._
  
  val universe: context.universe.type = context.universe
  
  def foreach[A, U]
      (these: Expr[Iterator[A]])
      (f: Expr[A => U])
    : Expr[Unit] = {
    val xs   = newTermName(fresh("xs$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) :: Nil,
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(f.tree, Select(Ident(xs), "head") :: Nil) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)))
    } (TypeTag.Unit)
  }
  
  def foldLeft[A, B : WeakTypeTag]
      (these: Expr[Iterator[A]])
      (z: Expr[B])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), z.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head") :: Nil)) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r))
    } (weakTypeTag[B])
  }
  
  def reduceLeft[A, B >: A : WeakTypeTag]
      (these: Expr[Iterator[A]])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        If(
          Select(Ident(xs), "isEmpty"),
          Throw(
            ApplyConstructor(
              Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), newTypeName("UnsupportedOperationException")),
              Literal(Constant("Empty reduce.")) :: Nil)),
          EmptyTree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Select(Ident(xs), "head")) ::
        Apply(Select(Ident(xs), "step"), Nil) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head") :: Nil)) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r))
    } (weakTypeTag[B])
  }
  
  def reduceLeftOption[A, B >: A : WeakTypeTag]
      (these: Expr[Iterator[A]])
      (op: Expr[(B, A) => B])
    : Expr[Option[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    val OptionType = appliedType(mirror.staticClass("basis.control.Option").toType, weakTypeOf[B] :: Nil)
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) :: Nil,
        If(
          Select(Ident(xs), "isEmpty"),
          Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "None"),
          Block(
            ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Select(Ident(xs), "head")) ::
            Apply(Select(Ident(xs), "step"), Nil) ::
            LabelDef(loop, Nil,
              If(
                Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
                Block(
                  Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head") :: Nil)) ::
                  Apply(Select(Ident(xs), "step"), Nil) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree)) :: Nil,
            Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Some"), Ident(r) :: Nil))))
    } (WeakTypeTag(OptionType))
  }
  
  def find[A : WeakTypeTag]
      (these: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
    : Expr[Option[A]] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    val OptionType = appliedType(mirror.staticClass("basis.control.Option").toType, weakTypeOf[A] :: Nil)
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(OptionType),
          Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "None")) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Assign(
                  Ident(r),
                  Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Some"), Ident(x) :: Nil)),
                Block(
                  Apply(Select(Ident(xs), "step"), Nil) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (WeakTypeTag(OptionType))
  }
  
  def forall[A]
      (these: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(true))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            If(
              Apply(p.tree, Select(Ident(xs), "head") :: Nil),
              Block(
                Apply(Select(Ident(xs), "step"), Nil) :: Nil,
                Apply(Ident(loop), Nil)),
              Assign(Ident(r), Literal(Constant(false)))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (TypeTag.Boolean)
  }
  
  def exists[A]
      (these: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(false))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            If(
              Apply(p.tree, Select(Ident(xs), "head") :: Nil),
              Assign(Ident(r), Literal(Constant(true))),
              Block(
                Apply(Select(Ident(xs), "step"), Nil) :: Nil,
                Apply(Ident(loop), Nil))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (TypeTag.Boolean)
  }
  
  def count[A]
      (these: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
    : Expr[Int] = {
    val xs   = newTermName(fresh("xs$"))
    val t    = newTermName(fresh("t$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), t, TypeTree(), Literal(Constant(0))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              If(
                Apply(p.tree, Select(Ident(xs), "head") :: Nil),
                Assign(Ident(t), Apply(Select(Ident(t), "$plus"), Literal(Constant(1)) :: Nil)),
                EmptyTree) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(t))
    } (TypeTag.Int)
  }
  
  def choose[A, B : WeakTypeTag]
      (these: Expr[Iterator[A]])
      (q: Expr[PartialFunction[A, B]])
    : Expr[Option[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("r$"))
    val f    = newTermName(fresh("pf$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    val OptionType = appliedType(mirror.staticClass("basis.control.Option").toType, weakTypeOf[B] :: Nil)
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(OptionType),
          Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "None")) ::
        ValDef(NoMods, f, TypeTree(), q.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) :: Nil,
              If(
                Apply(Select(Ident(f), "isDefinedAt"), Ident(x) :: Nil),
                Assign(
                  Ident(r),
                  Apply(
                    Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Some"),
                    Apply(Select(Ident(f), "applyOrElse"), Ident(x) ::
                      Select(Select(Select(Ident(nme.ROOTPKG), "scala"), "PartialFunction"), "empty") :: Nil) :: Nil)),
                Block(
                  Apply(Select(Ident(xs), "step"), Nil) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (WeakTypeTag(OptionType))
  }
  
  def collect[A, B]
      (these: Expr[Iterator[A]])
      (q: Expr[PartialFunction[A, B]])
      (builder: Expr[Builder[_, B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("b$"))
    val f    = newTermName(fresh("pf$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        ValDef(NoMods, f, TypeTree(), q.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) ::
              If(
                Apply(Select(Ident(f), "isDefinedAt"), Ident(x) :: Nil),
                Apply(
                  Select(Ident(b), "append"),
                  Apply(Select(Ident(f), "applyOrElse"), Ident(x) ::
                    Select(Select(Select(Ident(nme.ROOTPKG), "scala"), "PartialFunction"), "empty") :: Nil) :: Nil),
                EmptyTree) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def map[A, B]
      (these: Expr[Iterator[A]])
      (f: Expr[A => B])
      (builder: Expr[Builder[_, B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "append"), Apply(f.tree, Select(Ident(xs), "head") :: Nil) :: Nil) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def flatMap[A, B]
      (these: Expr[Iterator[A]])
      (f: Expr[A => Enumerator[B]])
      (builder: Expr[Builder[_, B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "appendAll"), Apply(f.tree, Select(Ident(xs), "head") :: Nil) :: Nil) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def filter[A]
      (these: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) ::
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Apply(Select(Ident(b), "append"), Ident(x) :: Nil),
                EmptyTree) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def dropWhile[A]
      (these: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val b     = newTermName(fresh("b$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("x$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop1, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Apply(Ident(loop1), Nil),
                Apply(Select(Ident(b), "append"), Ident(x) :: Nil))),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "append"), Select(Ident(xs), "head") :: Nil) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def takeWhile[A]
      (these: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Block(
                  Apply(Select(Ident(b), "append"), Ident(x) :: Nil) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def span[A]
      (these: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
      (builder1: Expr[Builder[_, A]], builder2: Expr[Builder[_, A]])
    : Expr[(builder1.value.State, builder2.value.State)] = {
    val xs    = newTermName(fresh("xs$"))
    val a     = newTermName(fresh("b$"))
    val b     = newTermName(fresh("b$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("x$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, a, TypeTree(BuilderType(builder1)), builder1.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder2)), builder2.tree) ::
        LabelDef(loop1, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Block(
                  Apply(Select(Ident(a), "append"), Ident(x) :: Nil) :: Nil,
                  Apply(Ident(loop1), Nil)),
                Apply(Select(Ident(b), "append"), Ident(x) :: Nil))),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "append"), Select(Ident(xs), "head") :: Nil) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        ApplyConstructor(
          Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Tuple2")),
          Select(Ident(a), "state") :: Select(Ident(b), "state") :: Nil))
    } (Tuple2Tag(StateTag(builder1), StateTag(builder2)))
  }
  
  def drop[A]
      (these: Expr[Iterator[A]])
      (lower: Expr[Int])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val b     = newTermName(fresh("b$"))
    val loop1 = newTermName(fresh("loop$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
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
                Apply(Select(Ident(xs), "step"), Nil) :: Nil,
                Apply(Ident(loop1), Nil)),
              EmptyTree),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "append"), Select(Ident(xs), "head") :: Nil) ::
              Apply(Select(Ident(xs), "step"), Nil) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def take[A]
      (these: Expr[Iterator[A]])
      (upper: Expr[Int])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), upper.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            If(
              Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
              Block(
                Apply(Select(Ident(b), "append"), Select(Ident(xs), "head") :: Nil) ::
                Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
                Apply(Select(Ident(xs), "step"), Nil) :: Nil,
                Apply(Ident(loop), Nil)),
              EmptyTree),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def slice[A]
      (these: Expr[Iterator[A]])
      (lower: Expr[Int], upper: Expr[Int])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val b     = newTermName(fresh("b$"))
    val loop1 = newTermName(fresh("loop$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
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
                Apply(Select(Ident(xs), "step"), Nil) :: Nil,
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
                Apply(Select(Ident(b), "append"), Select(Ident(xs), "head") :: Nil) ::
                Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) ::
                Apply(Select(Ident(xs), "step"), Nil) :: Nil,
                Apply(Ident(loop2), Nil)),
              EmptyTree),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def zip[A, B]
      (these: Expr[Iterator[A]], those: Expr[Iterator[B]])
      (builder: Expr[Builder[_, (A, B)]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val ys   = newTermName(fresh("ys$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, ys, TypeTree(), those.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            If(
              Select(Select(Ident(ys), "isEmpty"), "unary_$bang"),
              Block(
                Apply(
                  Select(Ident(b), "append"),
                  ApplyConstructor(
                    Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Tuple2")),
                    Select(Ident(xs), "head") :: Select(Ident(ys), "head") :: Nil) :: Nil) ::
                Apply(Select(Ident(xs), "step"), Nil) ::
                Apply(Select(Ident(ys), "step"), Nil) :: Nil,
                Apply(Ident(loop), Nil)),
              EmptyTree),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  private def BuilderType(builder: Expr[Builder[_, _]]): Type = builder.tree.symbol match {
    case symbol: TermSymbol if symbol.isStable => singleType(NoPrefix, symbol)
    case _ => builder.actualType
  }
  
  private def StateTag(builder: Expr[Builder[_, _]]): WeakTypeTag[builder.value.State] = {
    val StateSymbol = mirror.staticClass("basis.collections.Builder").toType.member(newTypeName("State"))
    WeakTypeTag(typeRef(BuilderType(builder), StateSymbol, Nil))
  }
  
  private def Tuple2Tag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[(A, B)] =
    WeakTypeTag(appliedType(mirror.staticClass("scala.Tuple2").toType, weakTypeOf[A] :: weakTypeOf[B] :: Nil))
}
