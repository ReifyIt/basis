/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

import scala.collection.immutable.{::, Nil}
import scala.reflect.macros.Context

/** Index operations macro implementations.
  * 
  * @author Chris Sachs
  */
private[sequential] final class IndexMacros[C <: Context](val context: C) {
  import context.{Expr, fresh, mirror, WeakTypeTag}
  import universe._
  
  val universe: context.universe.type = context.universe
  
  def foreach[A, U]
      (these: Expr[Index[A]])
      (f: Expr[A => U])
    : Expr[Unit] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) :: 
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
      (these: Expr[Index[A]])
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
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
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
      (these: Expr[Index[A]])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("result$"))
    val i    = newTermName(fresh("i$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
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
      (these: Expr[Index[A]])
      (op: Expr[(B, A) => B])
    : Expr[Option[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("result$"))
    val i    = newTermName(fresh("i$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
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
      (these: Expr[Index[A]])
      (z: Expr[B])
      (op: Expr[(A, B) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
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
      (these: Expr[Index[A]])
      (op: Expr[(A, B) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
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
      (these: Expr[Index[A]])
      (op: Expr[(A, B) => B])
    : Expr[Option[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
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
      (these: Expr[Index[A]])
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
      ValDef(NoMods, xs, TypeTree(), these.tree) ::
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
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
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
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("result$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
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
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
    : Expr[Int] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val t    = newTermName(fresh("total$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
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
  
  def choose[A, B : WeakTypeTag]
      (these: Expr[Index[A]])
      (q: Expr[PartialFunction[A, B]])
    : Expr[Option[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("result$"))
    val f    = newTermName(fresh("pf$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(OptionTag[B].tpe),
          Select(Select(Ident(nme.ROOTPKG), "scala"), "None")) ::
        ValDef(NoMods, f, TypeTree(), q.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) :: Nil,
              If(
                Apply(Select(Ident(f), "isDefinedAt"), Ident(x) :: Nil),
                Assign(
                  Ident(r),
                  ApplyConstructor(
                    Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Some")),
                    Apply(Select(Ident(f), "applyOrElse"), Ident(x) ::
                      Select(Select(Select(Ident(nme.ROOTPKG), "scala"), "PartialFunction"), "empty") :: Nil) :: Nil)),
                Block(
                  Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r))
    } (OptionTag[B])
  }
  
  def collect[A, B]
      (these: Expr[Index[A]])
      (q: Expr[PartialFunction[A, B]])
      (builder: Expr[Builder[_, B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("builder$"))
    val f    = newTermName(fresh("pf$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        ValDef(NoMods, f, TypeTree(), q.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) ::
              If(
                Apply(Select(Ident(f), "isDefinedAt"), Ident(x) :: Nil),
                Apply(
                  Select(Ident(b), "append"),
                  Apply(Select(Ident(f), "applyOrElse"), Ident(x) ::
                    Select(Select(Select(Ident(nme.ROOTPKG), "scala"), "PartialFunction"), "empty") :: Nil) :: Nil),
                EmptyTree) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def map[A, B]
      (these: Expr[Index[A]])
      (f: Expr[A => B])
      (builder: Expr[Builder[_, B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), Apply(Select(builder.tree, "expect"), Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "append"), Apply(f.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def flatMap[A, B]
      (these: Expr[Index[A]])
      (f: Expr[A => Enumerator[B]])
      (builder: Expr[Builder[_, B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "appendAll"), Apply(f.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def filter[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) ::
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Apply(Select(Ident(b), "append"), Ident(x) :: Nil),
                EmptyTree) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def dropWhile[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val b     = newTermName(fresh("builder$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("x$"))
    val loop2 = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop1, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Apply(Ident(loop1), Nil),
                Apply(Select(Ident(b), "append"), Ident(x) :: Nil))),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "append"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def takeWhile[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
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
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
      (builder1: Expr[Builder[_, A]], builder2: Expr[Builder[_, A]])
    : Expr[(builder1.value.State, builder2.value.State)] = {
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
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, a, TypeTree(BuilderType(builder1)), builder1.tree) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder2)), builder2.tree) ::
        LabelDef(loop1, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Block(
                  Apply(Select(Ident(a), "append"), Ident(x) :: Nil) :: Nil,
                  Apply(Ident(loop1), Nil)),
                Apply(Select(Ident(b), "append"), Ident(x) :: Nil))),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "append"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        ApplyConstructor(
          Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Tuple2")),
          Select(Ident(a), "state") :: Select(Ident(b), "state") :: Nil))
    } (Tuple2Tag(StateTag(builder1), StateTag(builder2)))
  }
  
  def drop[A]
      (these: Expr[Index[A]])
      (lower: Expr[Int])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val n    = newTermName(fresh("n$"))
    val i    = newTermName(fresh("i$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          min(max(Literal(Constant(0)), lower.tree), Ident(n))) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)),
          Apply(Select(builder.tree, "expect"), Apply(Select(Ident(n), "$minus"), Ident(i) :: Nil) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "append"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def take[A]
      (these: Expr[Index[A]])
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
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), min(upper.tree, Select(Ident(xs), "length"))) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), Apply(Select(builder.tree, "expect"), Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "append"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def slice[A]
      (these: Expr[Index[A]])
      (lower: Expr[Int], upper: Expr[Int])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val n    = newTermName(fresh("n$"))
    val i    = newTermName(fresh("i$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, n, TypeTree(), 
          min(max(Literal(Constant(0)), upper.tree), Select(Ident(xs), "length"))) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          min(max(Literal(Constant(0)), lower.tree), Ident(n))) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)),
          Apply(Select(builder.tree, "expect"), Apply(Select(Ident(n), "$minus"), Ident(i) :: Nil) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "append"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def reverse[A]
      (these: Expr[Index[A]])
      (builder: Expr[Builder[_, A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val b    = newTermName(fresh("builder$"))
    val loop = newTermName(fresh("loop$"))
    Expr {
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, i, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), Apply(Select(builder.tree, "expect"), Ident(i) :: Nil)) ::
        Assign(Ident(i), Apply(Select(Ident(i), "$minus"), Literal(Constant(1)) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$greater$eq"), Literal(Constant(0)) :: Nil),
            Block(
              Apply(Select(Ident(b), "append"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$minus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  def zip[A, B]
      (these: Expr[Index[A]], those: Expr[Index[B]])
      (builder: Expr[Builder[_, (A, B)]])
    : Expr[builder.value.State] = {
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
        ValDef(NoMods, b, TypeTree(BuilderType(builder)), Apply(Select(builder.tree, "expect"), Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(
                Select(Ident(b), "append"),
                ApplyConstructor(
                  Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Tuple2")),
                  Apply(Ident(xs), Ident(i) :: Nil) :: Apply(Ident(ys), Ident(i) :: Nil) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state"))
    } (StateTag(builder))
  }
  
  private def max(x: Tree, y: Tree): Tree =
    Apply(Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "max"), x :: y :: Nil)
  
  private def min(x: Tree, y: Tree): Tree =
    Apply(Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "min"), x :: y :: Nil)
  
  private def BuilderType(builder: Expr[Builder[_, _]]): Type = builder.tree.symbol match {
    case symbol: TermSymbol if symbol.isStable => singleType(NoPrefix, symbol)
    case _ => builder.actualType
  }
  
  private def StateTag(builder: Expr[Builder[_, _]]): WeakTypeTag[builder.value.State] = {
    val StateSymbol = mirror.staticClass("basis.collections.Builder").toType.member(newTypeName("State"))
    WeakTypeTag(typeRef(BuilderType(builder), StateSymbol, Nil))
  }
  
  private def OptionTag[A : WeakTypeTag]: WeakTypeTag[Option[A]] =
    WeakTypeTag(appliedType(mirror.staticClass("scala.Option").toType, weakTypeOf[A] :: Nil))
  
  private def Tuple2Tag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[(A, B)] =
    WeakTypeTag(appliedType(mirror.staticClass("scala.Tuple2").toType, weakTypeOf[A] :: weakTypeOf[B] :: Nil))
}
