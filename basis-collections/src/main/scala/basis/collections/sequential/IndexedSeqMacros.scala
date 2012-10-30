/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

import scala.collection.immutable.{::, Nil}
import scala.reflect.macros.Context

private[collections] final class IndexedSeqMacros[C <: Context](val context: C) {
  import context.{Expr, fresh, mirror, WeakTypeTag}
  import universe._
  
  val universe: context.universe.type = context.universe
  
  def foreach[A, U]
      (seq: Expr[IndexedSeq[A]])
      (f: Expr[A => U])
    : Expr[Unit] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) :: 
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) :: Nil,
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(f.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)))
    } (TypeTag.Unit)
  }
  
  def foldLeft[A, B : WeakTypeTag]
      (seq: Expr[IndexedSeq[A]])
      (z: Expr[B])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), z.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Apply(Ident(xs), Ident(i) :: Nil) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r))
    } (weakTypeTag[B])
  }
  
  def reduceLeft[A, B >: A : WeakTypeTag]
      (seq: Expr[IndexedSeq[A]])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("result$"))
    val i    = newTermName(fresh("i$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        If(
          Apply(Select(Ident(n), "$less$eq"), Literal(Constant(0)) :: Nil),
          Throw(
            ApplyConstructor(
              Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), newTypeName("UnsupportedOperationException")),
              Literal(Constant("Empty reduce.")) :: Nil)),
          EmptyTree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Apply(Ident(xs), Literal(Constant(0)) :: Nil)) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(1))) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Apply(Ident(xs), Ident(i) :: Nil) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r))
    } (weakTypeTag[B])
  }
  
  def reduceLeftOption[A, B >: A : WeakTypeTag]
      (seq: Expr[IndexedSeq[A]])
      (op: Expr[(B, A) => B])
    : Expr[Option[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("result$"))
    val i    = newTermName(fresh("i$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) :: Nil,
        If(
          Apply(Select(Ident(n), "$less$eq"), Literal(Constant(0)) :: Nil),
          Select(Select(Ident(nme.ROOTPKG), "scala"), "None"),
          Block(
            ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Apply(Ident(xs), Literal(Constant(0)) :: Nil)) ::
            ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(1))) ::
            LabelDef(loop, Nil,
              If(
                Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
                Block(
                  Assign(Ident(r), Apply(op.tree, Ident(r) :: Apply(Ident(xs), Ident(i) :: Nil) :: Nil)) ::
                  Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree)) :: Nil,
            ApplyConstructor(
              Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Some")),
              Ident(r) :: Nil))))
    } (OptionTag[B])
  }
  
  def foldRight[A, B : WeakTypeTag]
      (seq: Expr[IndexedSeq[A]])
      (z: Expr[B])
      (op: Expr[(A, B) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          Apply(Select(Select(Ident(xs), "length"), "$minus"), Literal(Constant(1)) :: Nil)) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), z.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$greater$eq"), Literal(Constant(0)) :: Nil),
            Block(
              Assign(Ident(r), Apply(op.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Ident(r) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$minus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r))
    } (weakTypeTag[B])
  }
  
  def reduceRight[A, B >: A : WeakTypeTag]
      (seq: Expr[IndexedSeq[A]])
      (op: Expr[(A, B) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          Apply(Select(Select(Ident(xs), "length"), "$minus"), Literal(Constant(1)) :: Nil)) ::
        If(
          Apply(Select(Ident(i), "$less"), Literal(Constant(0)) :: Nil),
          Throw(
            ApplyConstructor(
              Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), newTypeName("UnsupportedOperationException")),
              Literal(Constant("Empty reduce.")) :: Nil)),
          EmptyTree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Apply(Ident(xs), Ident(i) :: Nil)) ::
        Assign(Ident(i), Apply(Select(Ident(i), "$minus"), Literal(Constant(1)) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$greater$eq"), Literal(Constant(0)) :: Nil),
            Block(
              Assign(Ident(r), Apply(op.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Ident(r) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$minus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r))
    } (weakTypeTag[B])
  }
  
  def reduceRightOption[A, B >: A : WeakTypeTag]
      (seq: Expr[IndexedSeq[A]])
      (op: Expr[(A, B) => B])
    : Expr[Option[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          Apply(Select(Select(Ident(xs), "length"), "$minus"), Literal(Constant(1)) :: Nil)) :: Nil,
        If(
          Apply(Select(Ident(i), "$less"), Literal(Constant(0)) :: Nil),
          Select(Select(Ident(nme.ROOTPKG), "scala"), "None"),
          Block(
            ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Apply(Ident(xs), Ident(i) :: Nil)) ::
            Assign(Ident(i), Apply(Select(Ident(i), "$minus"), Literal(Constant(1)) :: Nil)) ::
            LabelDef(loop, Nil,
              If(
                Apply(Select(Ident(i), "$greater$eq"), Literal(Constant(0)) :: Nil),
                Block(
                  Assign(Ident(r), Apply(op.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Ident(r) :: Nil)) ::
                  Assign(Ident(i), Apply(Select(Ident(i), "$minus"), Literal(Constant(1)) :: Nil)) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree)) :: Nil,
            ApplyConstructor(
              Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Some")),
              Ident(r) :: Nil))))
    } (OptionTag[B])
  }
  
  def find[A : WeakTypeTag]
      (seq: Expr[IndexedSeq[A]])
      (p: Expr[A => Boolean])
    : Expr[Option[A]] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    Expr {
    Block(
      ValDef(NoMods, xs, TypeTree(), seq.tree) ::
      ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
      ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
      ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(OptionTag[A].tpe),
        Select(Select(Ident(nme.ROOTPKG), "scala"), "None")) ::
      LabelDef(loop, Nil,
        If(
          Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
          Block(
            ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) :: Nil,
            If(
              Apply(p.tree, Ident(x) :: Nil),
              Assign(
                Ident(r),
                ApplyConstructor(
                  Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Some")),
                  Ident(x) :: Nil)),
              Block(
                Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
                Apply(Ident(loop), Nil)))),
          EmptyTree)) :: Nil,
      Ident(r))
    } (OptionTag[A])
  }
  
  def forall[A]
      (seq: Expr[IndexedSeq[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(true))) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            If(
              Apply(p.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil),
              Block(
                Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
                Apply(Ident(loop), Nil)),
              Assign(Ident(r), Literal(Constant(false)))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (TypeTag.Boolean)
  }
  
  def exists[A]
      (seq: Expr[IndexedSeq[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(false))) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            If(
              Apply(p.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil),
              Assign(Ident(r), Literal(Constant(true))),
              Block(
                Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
                Apply(Ident(loop), Nil))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (TypeTag.Boolean)
  }
  
  def count[A]
      (seq: Expr[IndexedSeq[A]])
      (p: Expr[A => Boolean])
    : Expr[Int] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val t    = newTermName(fresh("total$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(Modifiers(Flag.MUTABLE), t, TypeTree(), Literal(Constant(0))) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              If(
                Apply(p.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil),
                Assign(Ident(t), Apply(Select(Ident(t), "$plus"), Literal(Constant(1)) :: Nil)),
                EmptyTree) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(t))
    } (TypeTag.Int)
  }
  
  def select[A, B : WeakTypeTag]
      (seq: Expr[IndexedSeq[A]])
      (q: Expr[PartialFunction[A, B]])
    : Expr[Option[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(OptionTag[B].tpe),
          Select(Select(Ident(nme.ROOTPKG), "scala"), "None")) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) :: Nil,
              If(
                Apply(Select(q.tree, "isDefinedAt"), Ident(x) :: Nil),
                Assign(
                  Ident(r),
                  ApplyConstructor(
                    Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Some")),
                    Apply(q.tree, Ident(x) :: Nil) :: Nil)),
                Block(
                  Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (OptionTag[B])
  }
  
  def collect[A, B]
      (seq: Expr[IndexedSeq[A]])
      (q: Expr[PartialFunction[A, B]])
      (buffer: Expr[Buffer[_, B]])
    : Expr[buffer.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(), buffer.tree) ::
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
    } (BufferStateTag(buffer))
  }
  
  def map[A, B]
      (seq: Expr[IndexedSeq[A]])
      (f: Expr[A => B])
      (buffer: Expr[Buffer[_, B]])
    : Expr[buffer.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(), Apply(Select(buffer.tree, "expect"), Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(f.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (BufferStateTag(buffer))
  }
  
  def flatMap[A, B]
      (seq: Expr[IndexedSeq[A]])
      (f: Expr[A => Enumerator[B]])
      (buffer: Expr[Buffer[_, B]])
    : Expr[buffer.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(), buffer.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$plus$eq"), Apply(f.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (BufferStateTag(buffer))
  }
  
  def filter[A]
      (seq: Expr[IndexedSeq[A]])
      (p: Expr[A => Boolean])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(), buffer.tree) ::
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
    } (BufferStateTag(buffer))
  }
  
  def dropWhile[A]
      (seq: Expr[IndexedSeq[A]])
      (p: Expr[A => Boolean])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val b     = newTermName(fresh("buffer$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("x$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(), buffer.tree) ::
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
    } (BufferStateTag(buffer))
  }
  
  def takeWhile[A]
      (seq: Expr[IndexedSeq[A]])
      (p: Expr[A => Boolean])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(), buffer.tree) ::
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
    } (BufferStateTag(buffer))
  }
  
  def span[A]
      (seq: Expr[IndexedSeq[A]])
      (p: Expr[A => Boolean])
      (bufferA: Expr[Buffer[_, A]], bufferB: Expr[Buffer[_, A]])
    : Expr[(bufferA.value.State, bufferB.value.State)] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val a     = newTermName(fresh("buffer$"))
    val b     = newTermName(fresh("buffer$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("x$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, a, TypeTree(), bufferA.tree) ::
        ValDef(NoMods, b, TypeTree(), bufferB.tree) ::
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
    } (Tuple2Tag(BufferStateTag(bufferA), BufferStateTag(bufferB)))
  }
  
  def drop[A]
      (seq: Expr[IndexedSeq[A]])
      (lower: Expr[Int])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val n    = newTermName(fresh("n$"))
    val i    = newTermName(fresh("i$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          min(max(Literal(Constant(0)), lower.tree), Ident(n))) ::
        ValDef(NoMods, b, TypeTree(),
          Apply(Select(buffer.tree, "expect"), Apply(Select(Ident(n), "$minus"), Ident(i) :: Nil) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (BufferStateTag(buffer))
  }
  
  def take[A]
      (seq: Expr[IndexedSeq[A]])
      (upper: Expr[Int])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), min(upper.tree, Select(Ident(xs), "length"))) ::
        ValDef(NoMods, b, TypeTree(), Apply(Select(buffer.tree, "expect"), Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (BufferStateTag(buffer))
  }
  
  def slice[A]
      (seq: Expr[IndexedSeq[A]])
      (lower: Expr[Int], upper: Expr[Int])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val n    = newTermName(fresh("n$"))
    val i    = newTermName(fresh("i$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(NoMods, n, TypeTree(), 
          min(max(Literal(Constant(0)), upper.tree), Select(Ident(xs), "length"))) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          min(max(Literal(Constant(0)), lower.tree), Ident(n))) ::
        ValDef(NoMods, b, TypeTree(),
          Apply(Select(buffer.tree, "expect"), Apply(Select(Ident(n), "$minus"), Ident(i) :: Nil) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (BufferStateTag(buffer))
  }
  
  def zip[A, B]
      (these: Expr[IndexedSeq[A]], those: Expr[IndexedSeq[B]])
      (buffer: Expr[Buffer[_, (A, B)]])
    : Expr[buffer.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val ys   = newTermName(fresh("ys$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, ys, TypeTree(), those.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), min(Select(Ident(xs), "length"), Select(Ident(ys), "length"))) ::
        ValDef(NoMods, b, TypeTree(), Apply(Select(buffer.tree, "expect"), Ident(n) :: Nil)) ::
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
        Select(Ident(b), "result"))
    } (BufferStateTag(buffer))
  }
  
  def reverse[A]
      (seq: Expr[IndexedSeq[A]])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), seq.tree) ::
        ValDef(NoMods, i, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(), Apply(Select(buffer.tree, "expect"), Ident(i) :: Nil)) ::
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
    } (BufferStateTag(buffer))
  }
  
  def ++ [A]
      (these: Expr[IndexedSeq[A]], those: Expr[IndexedSeq[A]])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val xs = newTermName(fresh("xs$"))
    val ys = newTermName(fresh("ys$"))
    val b  = newTermName(fresh("buffer$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, ys, TypeTree(), those.tree) ::
        ValDef(NoMods, b, TypeTree(),
          Apply(Select(buffer.tree, "expect"),
            Apply(Select(Select(Ident(xs), "length"), "$plus"), Select(Ident(ys), "length") :: Nil) :: Nil)) ::
        Apply(Select(Ident(b), "$plus$plus$eq"), Ident(xs) :: Nil) ::
        Apply(Select(Ident(b), "$plus$plus$eq"), Ident(ys) :: Nil) :: Nil,
        Select(Ident(b), "result"))
    } (BufferStateTag(buffer))
  }
  
  private def max(x: Tree, y: Tree): Tree = {
    Apply(
      Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "max"),
      x :: y :: Nil)
  }
  
  private def min(x: Tree, y: Tree): Tree = {
    Apply(
      Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "min"),
      x :: y :: Nil)
  }
  
  private def BufferType(buffer: Expr[Buffer[_, _]]): Type = {
    buffer.actualType.termSymbol match {
      case sym: TermSymbol if sym.isStable => singleType(NoPrefix, sym)
      case _ => buffer.actualType
    }
  }
  
  private def BufferStateTag(buffer: Expr[Buffer[_, _]]): WeakTypeTag[buffer.value.State] =
    WeakTypeTag(typeRef(BufferType(buffer), buffer.actualType.member(newTypeName("State")), Nil))
  
  implicit private def OptionTag[A : WeakTypeTag]: WeakTypeTag[Option[A]] =
    WeakTypeTag(appliedType(mirror.staticClass("scala.Option").toType, weakTypeOf[A] :: Nil))
  
  implicit private def Tuple2Tag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[(A, B)] =
    WeakTypeTag(appliedType(mirror.staticClass("scala.Tuple2").toType, weakTypeOf[A] :: weakTypeOf[B] :: Nil))
}
