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

private[collections] final class LinearSeqMacros[C <: Context](val context: C) {
  import context.{Expr, fresh, mirror, WeakTypeTag}
  import universe._
  
  val universe: context.universe.type = context.universe
  
  def foreach[A, U]
      (seq: Expr[LinearSeq[A]])
      (f: Expr[A => U])
    : Expr[Unit] = {
    val xs   = newTermName(fresh("xs$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) :: Nil,
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(f.tree, Select(Ident(xs), "head") :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)))
    } (TypeTag.Unit)
  }
  
  def foldLeft[A, B : WeakTypeTag]
      (seq: Expr[LinearSeq[A]])
      (z: Expr[B])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), z.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head") :: Nil)) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r))
    } (weakTypeTag[B])
  }
  
  def reduceLeft[A, B >: A : WeakTypeTag]
      (seq: Expr[LinearSeq[A]])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        If(
          Select(Ident(xs), "isEmpty"),
          Throw(
            ApplyConstructor(
              Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), newTypeName("UnsupportedOperationException")),
              Literal(Constant("Empty reduce.")) :: Nil)),
          EmptyTree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Select(Ident(xs), "head")) ::
        Assign(Ident(xs), Select(Ident(xs), "tail")) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head") :: Nil)) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r))
    } (weakTypeTag[B])
  }
  
  def reduceLeftOption[A, B >: A : WeakTypeTag]
      (seq: Expr[LinearSeq[A]])
      (op: Expr[(B, A) => B])
    : Expr[Option[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) :: Nil,
        If(
          Select(Ident(xs), "isEmpty"),
          Select(Select(Ident(nme.ROOTPKG), "scala"), "None"),
          Block(
            ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Select(Ident(xs), "head")) ::
            Assign(Ident(xs), Select(Ident(xs), "tail")) ::
            LabelDef(loop, Nil,
              If(
                Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
                Block(
                  Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head") :: Nil)) ::
                  Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree)) :: Nil,
            ApplyConstructor(
              Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Some")),
              Ident(r) :: Nil))))
    } (OptionTag[B])
  }
  
  def find[A : WeakTypeTag]
      (seq: Expr[LinearSeq[A]])
      (p: Expr[A => Boolean])
    : Expr[Option[A]] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(OptionTag[A].tpe),
          Select(Select(Ident(nme.ROOTPKG), "scala"), "None")) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Assign(
                  Ident(r),
                  ApplyConstructor(
                    Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Some")),
                    Ident(x) :: Nil)),
                Block(
                  Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (OptionTag[A])
  }
  
  def forall[A]
      (seq: Expr[LinearSeq[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(true))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            If(
              Apply(p.tree, Select(Ident(xs), "head") :: Nil),
              Block(
                Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                Apply(Ident(loop), Nil)),
              Assign(Ident(r), Literal(Constant(false)))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (TypeTag.Boolean)
  }
  
  def exists[A]
      (seq: Expr[LinearSeq[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(false))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            If(
              Apply(p.tree, Select(Ident(xs), "head") :: Nil),
              Assign(Ident(r), Literal(Constant(true))),
              Block(
                Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                Apply(Ident(loop), Nil))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (TypeTag.Boolean)
  }
  
  def count[A]
      (seq: Expr[LinearSeq[A]])
      (p: Expr[A => Boolean])
    : Expr[Int] = {
    val xs   = newTermName(fresh("xs$"))
    val t    = newTermName(fresh("total$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), t, TypeTree(), Literal(Constant(0))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              If(
                Apply(p.tree, Select(Ident(xs), "head") :: Nil),
                Assign(Ident(t), Apply(Select(Ident(t), "$plus"), Literal(Constant(1)) :: Nil)),
                EmptyTree) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(t))
    } (TypeTag.Int)
  }
  
  def select[A, B : WeakTypeTag]
      (seq: Expr[LinearSeq[A]])
      (q: Expr[PartialFunction[A, B]])
    : Expr[Option[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(OptionTag[B].tpe),
          Select(Select(Ident(nme.ROOTPKG), "scala"), "None")) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head")) :: Nil,
              If(
                Apply(Select(q.tree, "isDefinedAt"), Ident(x) :: Nil),
                Assign(
                  Ident(r),
                  ApplyConstructor(
                    Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Some")),
                    Apply(q.tree, Ident(x) :: Nil) :: Nil)),
                Block(
                  Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (OptionTag[B])
  }
  
  def collect[A, B]
      (seq: Expr[LinearSeq[A]])
      (q: Expr[PartialFunction[A, B]])
      (buffer: Expr[Buffer[_, B]])
    : Expr[buffer.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        ValDef(NoMods, b, TypeTree(), buffer.tree) ::
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
    } (BufferStateTag(buffer))
  }
  
  def map[A, B]
      (seq: Expr[LinearSeq[A]])
      (f: Expr[A => B])
      (buffer: Expr[Buffer[_, B]])
    : Expr[buffer.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        ValDef(NoMods, b, TypeTree(), buffer.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "$plus$eq"), Apply(f.tree, Select(Ident(xs), "head") :: Nil) :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (BufferStateTag(buffer))
  }
  
  def flatMap[A, B]
      (seq: Expr[LinearSeq[A]])
      (f: Expr[A => Enumerator[B]])
      (buffer: Expr[Buffer[_, B]])
    : Expr[buffer.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        ValDef(NoMods, b, TypeTree(), buffer.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty"), "unary_$bang"),
            Block(
              Apply(Select(Ident(b), "$plus$plus$eq"), Apply(f.tree, Select(Ident(xs), "head") :: Nil) :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail")) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (BufferStateTag(buffer))
  }
  
  def filter[A]
      (seq: Expr[LinearSeq[A]])
      (p: Expr[A => Boolean])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        ValDef(NoMods, b, TypeTree(), buffer.tree) ::
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
    } (BufferStateTag(buffer))
  }
  
  def dropWhile[A]
      (seq: Expr[LinearSeq[A]])
      (p: Expr[A => Boolean])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val b     = newTermName(fresh("buffer$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("head$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        ValDef(NoMods, b, TypeTree(), buffer.tree) ::
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
    } (BufferStateTag(buffer))
  }
  
  def takeWhile[A]
      (seq: Expr[LinearSeq[A]])
      (p: Expr[A => Boolean])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("head$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        ValDef(NoMods, b, TypeTree(), buffer.tree) ::
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
    } (BufferStateTag(buffer))
  }
  
  def span[A]
      (seq: Expr[LinearSeq[A]])
      (p: Expr[A => Boolean])
      (bufferA: Expr[Buffer[_, A]], bufferB: Expr[Buffer[_, A]])
    : Expr[(bufferA.value.State, bufferB.value.State)] = {
    val xs    = newTermName(fresh("xs$"))
    val a     = newTermName(fresh("buffer$"))
    val b     = newTermName(fresh("buffer$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("head$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        ValDef(NoMods, a, TypeTree(), bufferA.tree) ::
        ValDef(NoMods, b, TypeTree(), bufferB.tree) ::
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
    } (Tuple2Tag(BufferStateTag(bufferA), BufferStateTag(bufferB)))
  }
  
  def drop[A]
      (seq: Expr[LinearSeq[A]])
      (lower: Expr[Int])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val b     = newTermName(fresh("buffer$"))
    val loop1 = newTermName(fresh("loop$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), lower.tree) ::
        ValDef(NoMods, b, TypeTree(), buffer.tree) ::
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
    } (BufferStateTag(buffer))
  }
  
  def take[A]
      (seq: Expr[LinearSeq[A]])
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
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), upper.tree) ::
        ValDef(NoMods, b, TypeTree(), buffer.tree) ::
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
    } (BufferStateTag(buffer))
  }
  
  def slice[A]
      (seq: Expr[LinearSeq[A]])
      (lower: Expr[Int], upper: Expr[Int])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val b     = newTermName(fresh("buffer$"))
    val loop1 = newTermName(fresh("loop$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), seq.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(Modifiers(Flag.MUTABLE), n, TypeTree(), lower.tree) ::
        ValDef(NoMods, b, TypeTree(), buffer.tree) ::
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
    } (BufferStateTag(buffer))
  }
  
  def zip[A, B]
      (these: Expr[LinearSeq[A]], those: Expr[LinearSeq[B]])
      (buffer: Expr[Buffer[_, (A, B)]])
    : Expr[buffer.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val ys   = newTermName(fresh("ys$"))
    val b    = newTermName(fresh("buffer$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), ys, TypeTree(), those.tree) ::
        ValDef(NoMods, b, TypeTree(), buffer.tree) ::
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
        Select(Ident(b), "result"))
    } (BufferStateTag(buffer))
  }
  
  def ++ [A]
      (these: Expr[LinearSeq[A]], those: Expr[LinearSeq[A]])
      (buffer: Expr[Buffer[_, A]])
    : Expr[buffer.value.State] = {
    val b = newTermName(fresh("buffer$"))
    Expr {
      Block(
        ValDef(NoMods, b, TypeTree(), buffer.tree) ::
        Apply(Select(Ident(b), "$plus$plus$eq"), these.tree :: Nil) ::
        Apply(Select(Ident(b), "$plus$plus$eq"), those.tree :: Nil) :: Nil,
        Select(Ident(b), "result"))
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
  
  implicit private def OptionTag[A : WeakTypeTag]: WeakTypeTag[Option[A]] =
    WeakTypeTag(appliedType(mirror.staticClass("scala.Option").toType, weakTypeOf[A] :: Nil))
  
  implicit private def Tuple2Tag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[(A, B)] =
    WeakTypeTag(appliedType(mirror.staticClass("scala.Tuple2").toType, weakTypeOf[A] :: weakTypeOf[B] :: Nil))
}
