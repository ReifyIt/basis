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

/** Index operations macro implementations.
  * 
  * @author Chris Sachs
  */
private[sequential] class IndexMacros[C <: Context](val context: C) {
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
    Expr[Unit](
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
            EmptyTree))))
  }
  
  def foldLeft[A, B : WeakTypeTag]
      (these: Expr[Index[A]])
      (z: Expr[B])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr[B](
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
        Ident(r)))
  }
  
  def reduceLeft[A, B >: A : WeakTypeTag]
      (these: Expr[Index[A]])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr[B](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(1))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        If(
          Apply(Select(Ident(n), "$less$eq"), Literal(Constant(0)) :: Nil),
          Throw(
            ApplyConstructor(
              Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), newTypeName("UnsupportedOperationException")),
              Literal(Constant("Empty reduce.")) :: Nil)),
          EmptyTree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Apply(Ident(xs), Literal(Constant(0)) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Apply(Ident(xs), Ident(i) :: Nil) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r)))
  }
  
  def mayReduceLeft[A, B >: A : WeakTypeTag]
      (these: Expr[Index[A]])
      (op: Expr[(B, A) => B])
    : Expr[Maybe[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr[Maybe[B]](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
            ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(1))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) :: Nil,
        If(
          Apply(Select(Ident(n), "$less$eq"), Literal(Constant(0)) :: Nil),
          Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Trap"),
          Block(
            ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Apply(Ident(xs), Literal(Constant(0)) :: Nil)) ::
            LabelDef(loop, Nil,
              If(
                Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
                Block(
                  Assign(Ident(r), Apply(op.tree, Ident(r) :: Apply(Ident(xs), Ident(i) :: Nil) :: Nil)) ::
                  Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree)) :: Nil,
            Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Free"), Ident(r) :: Nil)))))
  }
  
  def foldRight[A, B : WeakTypeTag]
      (these: Expr[Index[A]])
      (z: Expr[B])
      (op: Expr[(A, B) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr[B](
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
        Ident(r)))
  }
  
  def reduceRight[A, B >: A : WeakTypeTag]
      (these: Expr[Index[A]])
      (op: Expr[(A, B) => B])
    : Expr[B] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr[B](
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
        Ident(r)))
  }
  
  def mayReduceRight[A, B >: A : WeakTypeTag]
      (these: Expr[Index[A]])
      (op: Expr[(A, B) => B])
    : Expr[Maybe[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr[Maybe[B]](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          Apply(Select(Select(Ident(xs), "length"), "$minus"), Literal(Constant(1)) :: Nil)) :: Nil,
        If(
          Apply(Select(Ident(i), "$less"), Literal(Constant(0)) :: Nil),
          Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Trap"),
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
            Apply(Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Free"), Ident(r) :: Nil)))))
  }
  
  def find[A : WeakTypeTag]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
    : Expr[Maybe[A]] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    implicit val MaybeATag = MaybeTag[A]
    Expr[Maybe[A]](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[Maybe[A]]),
          Select(Select(Ident(nme.ROOTPKG), "scala"), "Trap")) ::
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
                    Select(Select(Ident(nme.ROOTPKG), "scala"), newTypeName("Free")),
                    Ident(x) :: Nil)),
                Block(
                  Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }
  
  def forall[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr[Boolean](
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
        Ident(r)))
  }
  
  def exists[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("r$"))
    val loop = newTermName(fresh("loop$"))
    Expr[Boolean](
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
        Ident(r)))
  }
  
  def count[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
    : Expr[Int] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val t    = newTermName(fresh("t$"))
    val loop = newTermName(fresh("loop$"))
    Expr[Int](
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
        Ident(t)))
  }
  
  def choose[A, B : WeakTypeTag]
      (these: Expr[Index[A]])
      (q: Expr[PartialFunction[A, B]])
    : Expr[Maybe[B]] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val r    = newTermName(fresh("r$"))
    val f    = newTermName(fresh("q$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    implicit val MaybeBTag = MaybeTag[B]
    Expr[Maybe[B]](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[Maybe[B]]),
          Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Trap")) ::
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
                  Apply(
                    Select(Select(Select(Ident(nme.ROOTPKG), "basis"), "control"), "Free"),
                    Apply(Select(Ident(f), "applyOrElse"), Ident(x) ::
                      Select(Select(Select(Ident(nme.ROOTPKG), "scala"), "PartialFunction"), "empty") :: Nil) :: Nil)),
                Block(
                  Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }
  
  def collect[A, B]
      (these: Expr[Index[A]])
      (q: Expr[PartialFunction[A, B]])
      (builder: Expr[Builder[B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("b$"))
    val f    = newTermName(fresh("q$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
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
        Select(Ident(b), "state")))
  }
  
  def map[A, B]
      (these: Expr[Index[A]])
      (f: Expr[A => B])
      (builder: Expr[Builder[B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), Apply(Select(builder.tree, "expect"), Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "append"), Apply(f.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state")))
  }
  
  def flatMap[A, B]
      (these: Expr[Index[A]])
      (f: Expr[A => Enumerator[B]])
      (builder: Expr[Builder[B]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "appendAll"), Apply(f.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state")))
  }
  
  def filter[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
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
        Select(Ident(b), "state")))
  }
  
  def dropWhile[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val b     = newTermName(fresh("b$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("x$"))
    val loop2 = newTermName(fresh("loop$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
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
        Select(Ident(b), "state")))
  }
  
  def takeWhile[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    val x    = newTermName(fresh("x$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
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
        Select(Ident(b), "state")))
  }
  
  def span[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
      (builder1: Expr[Builder[A]], builder2: Expr[Builder[A]])
    : Expr[(builder1.value.State, builder2.value.State)] = {
    val xs    = newTermName(fresh("xs$"))
    val i     = newTermName(fresh("i$"))
    val n     = newTermName(fresh("n$"))
    val a     = newTermName(fresh("b$"))
    val b     = newTermName(fresh("b$"))
    val loop1 = newTermName(fresh("loop$"))
    val x     = newTermName(fresh("x$"))
    val loop2 = newTermName(fresh("loop$"))
    implicit val builder1TypeTag = BuilderTypeTag(builder1)
    implicit val builder1StateTag = BuilderStateTag(builder1)
    implicit val builder2TypeTag = BuilderTypeTag(builder2)
    implicit val builder2StateTag = BuilderStateTag(builder2)
    Expr[(builder1.value.State, builder2.value.State)](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, a, TypeTree(builder1TypeTag.tpe), builder1.tree) ::
        ValDef(NoMods, b, TypeTree(builder2TypeTag.tpe), builder2.tree) ::
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
          Select(Ident(a), "state") :: Select(Ident(b), "state") :: Nil)))
  }
  
  def drop[A]
      (these: Expr[Index[A]])
      (lower: Expr[Int])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val n    = newTermName(fresh("n$"))
    val i    = newTermName(fresh("i$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          min(max(Literal(Constant(0)), lower.tree), Ident(n))) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe),
          Apply(Select(builder.tree, "expect"), Apply(Select(Ident(n), "$minus"), Ident(i) :: Nil) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "append"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state")))
  }
  
  def take[A]
      (these: Expr[Index[A]])
      (upper: Expr[Int])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), min(upper.tree, Select(Ident(xs), "length"))) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), Apply(Select(builder.tree, "expect"), Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "append"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state")))
  }
  
  def slice[A]
      (these: Expr[Index[A]])
      (lower: Expr[Int], upper: Expr[Int])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val n    = newTermName(fresh("n$"))
    val i    = newTermName(fresh("i$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, n, TypeTree(), 
          min(max(Literal(Constant(0)), upper.tree), Select(Ident(xs), "length"))) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          min(max(Literal(Constant(0)), lower.tree), Ident(n))) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe),
          Apply(Select(builder.tree, "expect"), Apply(Select(Ident(n), "$minus"), Ident(i) :: Nil) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$less"), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "append"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$plus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state")))
  }
  
  def reverse[A]
      (these: Expr[Index[A]])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val i    = newTermName(fresh("i$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, i, TypeTree(), Select(Ident(xs), "length")) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), Apply(Select(builder.tree, "expect"), Ident(i) :: Nil)) ::
        Assign(Ident(i), Apply(Select(Ident(i), "$minus"), Literal(Constant(1)) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), "$greater$eq"), Literal(Constant(0)) :: Nil),
            Block(
              Apply(Select(Ident(b), "append"), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), "$minus"), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state")))
  }
  
  def zip[A, B]
      (these: Expr[Index[A]], those: Expr[Index[B]])
      (builder: Expr[Builder[(A, B)]])
    : Expr[builder.value.State] = {
    val xs   = newTermName(fresh("xs$"))
    val ys   = newTermName(fresh("ys$"))
    val i    = newTermName(fresh("i$"))
    val n    = newTermName(fresh("n$"))
    val b    = newTermName(fresh("b$"))
    val loop = newTermName(fresh("loop$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, ys, TypeTree(), those.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), min(Select(Ident(xs), "length"), Select(Ident(ys), "length"))) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), Apply(Select(builder.tree, "expect"), Ident(n) :: Nil)) ::
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
        Select(Ident(b), "state")))
  }
  
  def :+ [A]
      (these: Expr[Index[A]], elem: Expr[A])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs = newTermName(fresh("xs$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) :: Nil,
        Select(
          Apply(
            Select(
              Apply(
                Select(
                  Apply(
                    Select(builder.tree, "expect"),
                    Apply(
                      Select(Select(Ident(xs), "length"), "$plus"),
                      Literal(Constant(1)) :: Nil) :: Nil),
                  "$plus$plus$eq"),
                Ident(xs) :: Nil),
              "$plus$eq"),
            elem.tree :: Nil),
          "state")))
  }
  
  def +: [A]
      (elem: Expr[A], these: Expr[Index[A]])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs = newTermName(fresh("xs$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) :: Nil,
        Select(
          Apply(
            Select(
              Apply(
                Select(
                  Apply(
                    Select(builder.tree, "expect"),
                    Apply(
                      Select(Literal(Constant(1)), "$plus"),
                      Select(Ident(xs), "length") :: Nil) :: Nil),
                  "$plus$eq"),
                elem.tree :: Nil),
              "$plus$plus$eq"),
            Ident(xs) :: Nil),
          "state")))
  }
  
  def ++ [A]
      (these: Expr[Index[A]], those: Expr[Index[A]])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs = newTermName(fresh("xs$"))
    val ys = newTermName(fresh("ys$"))
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, ys, TypeTree(), those.tree) :: Nil,
        Select(
          Apply(
            Select(
              Apply(
                Select(
                  Apply(
                    Select(builder.tree, "expect"),
                    Apply(
                      Select(Select(Ident(xs), "length"), "$plus"),
                      Select(Ident(ys), "length") :: Nil) :: Nil),
                  "$plus$plus$eq"),
                Ident(xs) :: Nil),
              "$plus$plus$eq"),
            Ident(ys) :: Nil),
          "state")))
  }
  
  private[this] def max(x: Tree, y: Tree): Tree =
    Apply(Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "max"), x :: y :: Nil)
  
  private[this] def min(x: Tree, y: Tree): Tree =
    Apply(Select(Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "Math"), "min"), x :: y :: Nil)
  
  protected[this] def BuilderTypeTag(builder: Expr[Builder[_]]): WeakTypeTag[builder.value.type] =
    WeakTypeTag[builder.value.type](builder.tree.symbol match {
      case sym: TermSymbol if sym.isStable => singleType(NoPrefix, sym)
      case _ => builder.actualType
    })
  
  protected[this] def BuilderStateTag
      (builder: Expr[Builder[_]])
      (implicit BuilderTypeTag: WeakTypeTag[builder.value.type])
    : WeakTypeTag[builder.value.State] = {
    val BuilderTpc = mirror.staticClass("basis.collections.Builder").toType
    val BuilderStateSym = BuilderTpc member newTypeName("State")
    val BuilderStateTpe = typeRef(BuilderTypeTag.tpe, BuilderStateSym, Nil).normalize
    WeakTypeTag[builder.value.State](BuilderStateTpe)
  }
  
  implicit protected[this] def MaybeTag[A : WeakTypeTag]: WeakTypeTag[Maybe[A]] = {
    val BasisControl = mirror.staticPackage("basis.control").moduleClass
    val MaybeTpc = BasisControl.typeSignature.member(newTypeName("Maybe")).asType.toType
    val MaybeATpe = appliedType(MaybeTpc, weakTypeOf[A] :: Nil)
    WeakTypeTag[Maybe[A]](MaybeATpe)
  }
  
  implicit private[this] def Tuple2Tag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[(A, B)] = {
    val Tuple2Tpc = mirror.staticClass("scala.Tuple2").toType
    val Tuple2ABTpe = appliedType(Tuple2Tpc, weakTypeOf[A] :: weakTypeOf[B] :: Nil)
    WeakTypeTag[(A, B)](Tuple2ABTpe)
  }
}
