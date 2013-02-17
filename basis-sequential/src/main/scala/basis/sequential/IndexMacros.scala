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
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[Unit](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) :: 
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) :: Nil,
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              Apply(f.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree))))
  }
  
  def foldLeft[A, B : WeakTypeTag]
      (these: Expr[Index[A]])
      (z: Expr[B])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[B](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), z.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Apply(Ident(xs), Ident(i) :: Nil) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r)))
  }
  
  def reduceLeft[A, B >: A : WeakTypeTag]
      (these: Expr[Index[A]])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[B](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(1))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) ::
        If(
          Apply(Select(Ident(n), ("<=": TermName).encodedName), Literal(Constant(0)) :: Nil),
          Throw(
            Apply(
              Select(New(TypeTree(weakTypeOf[UnsupportedOperationException])), nme.CONSTRUCTOR),
              Literal(Constant("Empty reduce.")) :: Nil)),
          EmptyTree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Apply(Ident(xs), Literal(Constant(0)) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Apply(Ident(xs), Ident(i) :: Nil) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r)))
  }
  
  def mayReduceLeft[A, B >: A : WeakTypeTag]
      (these: Expr[Index[A]])
      (op: Expr[(B, A) => B])
    : Expr[Maybe[B]] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[Maybe[B]](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
            ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(1))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) :: Nil,
        If(
          Apply(Select(Ident(n), ("<=": TermName).encodedName), Literal(Constant(0)) :: Nil),
          Select(BasisControl, "Trap": TermName),
          Block(
            ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Apply(Ident(xs), Literal(Constant(0)) :: Nil)) ::
            LabelDef(loop, Nil,
              If(
                Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
                Block(
                  Assign(Ident(r), Apply(op.tree, Ident(r) :: Apply(Ident(xs), Ident(i) :: Nil) :: Nil)) ::
                  Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree)) :: Nil,
            Apply(Select(BasisControl, "Bind": TermName), Ident(r) :: Nil)))))
  }
  
  def foldRight[A, B : WeakTypeTag]
      (these: Expr[Index[A]])
      (z: Expr[B])
      (op: Expr[(A, B) => B])
    : Expr[B] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[B](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          Apply(Select(Select(Ident(xs), "length": TermName), ("-": TermName).encodedName), Literal(Constant(1)) :: Nil)) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), z.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), (">=": TermName).encodedName), Literal(Constant(0)) :: Nil),
            Block(
              Assign(Ident(r), Apply(op.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Ident(r) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("-": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r)))
  }
  
  def reduceRight[A, B >: A : WeakTypeTag]
      (these: Expr[Index[A]])
      (op: Expr[(A, B) => B])
    : Expr[B] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[B](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          Apply(Select(Select(Ident(xs), "length": TermName), ("-": TermName).encodedName), Literal(Constant(1)) :: Nil)) ::
        If(
          Apply(Select(Ident(i), ("<": TermName).encodedName), Literal(Constant(0)) :: Nil),
          Throw(
            Apply(
              Select(New(TypeTree(weakTypeOf[UnsupportedOperationException])), nme.CONSTRUCTOR),
              Literal(Constant("Empty reduce.")) :: Nil)),
          EmptyTree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Apply(Ident(xs), Ident(i) :: Nil)) ::
        Assign(Ident(i), Apply(Select(Ident(i), ("-": TermName).encodedName), Literal(Constant(1)) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), (">=": TermName).encodedName), Literal(Constant(0)) :: Nil),
            Block(
              Assign(Ident(r), Apply(op.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Ident(r) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("-": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r)))
  }
  
  def mayReduceRight[A, B >: A : WeakTypeTag]
      (these: Expr[Index[A]])
      (op: Expr[(A, B) => B])
    : Expr[Maybe[B]] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[Maybe[B]](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          Apply(Select(Select(Ident(xs), "length": TermName), ("-": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
        If(
          Apply(Select(Ident(i), ("<": TermName).encodedName), Literal(Constant(0)) :: Nil),
          Select(BasisControl, "Trap": TermName),
          Block(
            ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Apply(Ident(xs), Ident(i) :: Nil)) ::
            Assign(Ident(i), Apply(Select(Ident(i), ("-": TermName).encodedName), Literal(Constant(1)) :: Nil)) ::
            LabelDef(loop, Nil,
              If(
                Apply(Select(Ident(i), (">=": TermName).encodedName), Literal(Constant(0)) :: Nil),
                Block(
                  Assign(Ident(r), Apply(op.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Ident(r) :: Nil)) ::
                  Assign(Ident(i), Apply(Select(Ident(i), ("-": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree)) :: Nil,
            Apply(Select(BasisControl, "Bind": TermName), Ident(r) :: Nil)))))
  }
  
  def find[A : WeakTypeTag]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
    : Expr[Maybe[A]] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    val x    = fresh("x$"): TermName
    implicit val MaybeATag = MaybeTag[A]
    Expr[Maybe[A]](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[Maybe[A]]), Select(BasisControl, "Trap": TermName)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Assign(Ident(r), Apply(Select(BasisControl, "Bind": TermName), Ident(x) :: Nil)),
                Block(
                  Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }
  
  def forall[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[Boolean](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(true))) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            If(
              Apply(p.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil),
              Block(
                Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
                Apply(Ident(loop), Nil)),
              Assign(Ident(r), Literal(Constant(false)))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }
  
  def exists[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[Boolean](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(false))) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            If(
              Apply(p.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil),
              Assign(Ident(r), Literal(Constant(true))),
              Block(
                Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
                Apply(Ident(loop), Nil))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }
  
  def count[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
    : Expr[Int] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val t    = fresh("t$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[Int](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) ::
        ValDef(Modifiers(Flag.MUTABLE), t, TypeTree(), Literal(Constant(0))) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              If(
                Apply(p.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil),
                Assign(Ident(t), Apply(Select(Ident(t), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)),
                EmptyTree) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(t)))
  }
  
  def choose[A, B : WeakTypeTag]
      (these: Expr[Index[A]])
      (q: Expr[PartialFunction[A, B]])
    : Expr[Maybe[B]] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val r    = fresh("r$"): TermName
    val f    = fresh("q$"): TermName
    val loop = fresh("loop$"): TermName
    val x    = fresh("x$"): TermName
    implicit val MaybeBTag = MaybeTag[B]
    Expr[Maybe[B]](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[Maybe[B]]), Select(BasisControl, "Trap": TermName)) ::
        ValDef(NoMods, f, TypeTree(), q.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) :: Nil,
              If(
                Apply(Select(Ident(f), "isDefinedAt": TermName), Ident(x) :: Nil),
                Assign(
                  Ident(r),
                  Apply(
                    Select(BasisControl, "Bind": TermName),
                    Apply(
                      Select(Ident(f), "applyOrElse": TermName),
                      Ident(x) :: Select(ScalaPartialFunction, "empty": TermName) :: Nil) :: Nil)),
                Block(
                  Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }
  
  def collect[A, B]
      (these: Expr[Index[A]])
      (q: Expr[PartialFunction[A, B]])
      (builder: Expr[Builder[B]])
    : Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val b    = fresh("b$"): TermName
    val f    = fresh("q$"): TermName
    val loop = fresh("loop$"): TermName
    val x    = fresh("x$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        ValDef(NoMods, f, TypeTree(), q.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) ::
              If(
                Apply(Select(Ident(f), "isDefinedAt": TermName), Ident(x) :: Nil),
                Apply(
                  Select(Ident(b), "append": TermName),
                  Apply(
                    Select(Ident(f), "applyOrElse": TermName), Ident(x) ::
                    Select(ScalaPartialFunction, "empty": TermName) :: Nil) :: Nil),
                EmptyTree) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }
  
  def map[A, B]
      (these: Expr[Index[A]])
      (f: Expr[A => B])
      (builder: Expr[Builder[B]])
    : Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe),
          Apply(Select(builder.tree, "expect": TermName), Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              Apply(
                Select(Ident(b), "append": TermName),
                Apply(f.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }
  
  def flatMap[A, B]
      (these: Expr[Index[A]])
      (f: Expr[A => Enumerator[B]])
      (builder: Expr[Builder[B]])
    : Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              Apply(
                Select(Ident(b), "appendAll": TermName),
                Apply(f.tree, Apply(Ident(xs), Ident(i) :: Nil) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }
  
  def filter[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    val x    = fresh("x$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) ::
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Apply(Select(Ident(b), "append": TermName), Ident(x) :: Nil),
                EmptyTree) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }
  
  def dropWhile[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs    = fresh("xs$"): TermName
    val i     = fresh("i$"): TermName
    val n     = fresh("n$"): TermName
    val b     = fresh("b$"): TermName
    val loop1 = fresh("loop$"): TermName
    val x     = fresh("x$"): TermName
    val loop2 = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop1, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Apply(Ident(loop1), Nil),
                Apply(Select(Ident(b), "append": TermName), Ident(x) :: Nil))),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "append": TermName), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }
  
  def takeWhile[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    val x    = fresh("x$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Block(
                  Apply(Select(Ident(b), "append": TermName), Ident(x) :: Nil) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }
  
  def span[A]
      (these: Expr[Index[A]])
      (p: Expr[A => Boolean])
      (builder1: Expr[Builder[A]], builder2: Expr[Builder[A]])
    : Expr[(builder1.value.State, builder2.value.State)] = {
    val xs    = fresh("xs$"): TermName
    val i     = fresh("i$"): TermName
    val n     = fresh("n$"): TermName
    val a     = fresh("b$"): TermName
    val b     = fresh("b$"): TermName
    val loop1 = fresh("loop$"): TermName
    val x     = fresh("x$"): TermName
    val loop2 = fresh("loop$"): TermName
    implicit val builder1TypeTag = BuilderTypeTag(builder1)
    implicit val builder1StateTag = BuilderStateTag(builder1)
    implicit val builder2TypeTag = BuilderTypeTag(builder2)
    implicit val builder2StateTag = BuilderStateTag(builder2)
    Expr[(builder1.value.State, builder2.value.State)](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) ::
        ValDef(NoMods, a, TypeTree(builder1TypeTag.tpe), builder1.tree) ::
        ValDef(NoMods, b, TypeTree(builder2TypeTag.tpe), builder2.tree) ::
        LabelDef(loop1, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              ValDef(NoMods, x, TypeTree(), Apply(Ident(xs), Ident(i) :: Nil)) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Block(
                  Apply(Select(Ident(a), "append": TermName), Ident(x) :: Nil) :: Nil,
                  Apply(Ident(loop1), Nil)),
                Apply(Select(Ident(b), "append": TermName), Ident(x) :: Nil))),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "append": TermName), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        Apply(
          Select(New(TypeTree(weakTypeOf[(builder1.value.State, builder2.value.State)])), nme.CONSTRUCTOR),
          Select(Ident(a), "state": TermName) :: Select(Ident(b), "state": TermName) :: Nil)))
  }
  
  def drop[A]
      (these: Expr[Index[A]])
      (lower: Expr[Int])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val n    = fresh("n$"): TermName
    val i    = fresh("i$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, n, TypeTree(), Select(Ident(xs), "length": TermName)) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          min(max(Literal(Constant(0)), lower.tree), Ident(n))) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe),
          Apply(
            Select(builder.tree, "expect": TermName),
            Apply(Select(Ident(n), ("-": TermName).encodedName), Ident(i) :: Nil) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "append": TermName), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }
  
  def take[A]
      (these: Expr[Index[A]])
      (upper: Expr[Int])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), min(upper.tree, Select(Ident(xs), "length": TermName))) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe),
          Apply(Select(builder.tree, "expect": TermName), Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "append": TermName), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }
  
  def slice[A]
      (these: Expr[Index[A]])
      (lower: Expr[Int], upper: Expr[Int])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val n    = fresh("n$"): TermName
    val i    = fresh("i$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, n, TypeTree(), 
          min(max(Literal(Constant(0)), upper.tree), Select(Ident(xs), "length": TermName))) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(),
          min(max(Literal(Constant(0)), lower.tree), Ident(n))) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe),
          Apply(
            Select(builder.tree, "expect": TermName),
            Apply(Select(Ident(n), ("-": TermName).encodedName), Ident(i) :: Nil) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), "append": TermName), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }
  
  def reverse[A]
      (these: Expr[Index[A]])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, i, TypeTree(), Select(Ident(xs), "length": TermName)) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe),
          Apply(Select(builder.tree, "expect": TermName), Ident(i) :: Nil)) ::
        Assign(Ident(i), Apply(Select(Ident(i), ("-": TermName).encodedName), Literal(Constant(1)) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), (">=": TermName).encodedName), Literal(Constant(0)) :: Nil),
            Block(
              Apply(Select(Ident(b), "append": TermName), Apply(Ident(xs), Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("-": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }
  
  def zip[A : WeakTypeTag, B : WeakTypeTag]
      (these: Expr[Index[A]], those: Expr[Index[B]])
      (builder: Expr[Builder[(A, B)]])
    : Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val ys   = fresh("ys$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, ys, TypeTree(), those.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), min(Select(Ident(xs), "length": TermName), Select(Ident(ys), "length": TermName))) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), Apply(Select(builder.tree, "expect": TermName), Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              Apply(
                Select(Ident(b), "append": TermName),
                Apply(
                  Select(New(TypeTree(weakTypeOf[(A, B)])), nme.CONSTRUCTOR),
                  Apply(Ident(xs), Ident(i) :: Nil) ::
                  Apply(Ident(ys), Ident(i) :: Nil) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }
  
  def :+ [A]
      (these: Expr[Index[A]], elem: Expr[A])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs = fresh("xs$"): TermName
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
                    Select(builder.tree, "expect": TermName),
                    Apply(
                      Select(Select(Ident(xs), "length": TermName), ("+": TermName).encodedName),
                      Literal(Constant(1)) :: Nil) :: Nil),
                  ("++=": TermName).encodedName),
                Ident(xs) :: Nil),
              ("+=": TermName).encodedName),
            elem.tree :: Nil),
          "state": TermName)))
  }
  
  def +: [A]
      (elem: Expr[A], these: Expr[Index[A]])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs = fresh("xs$"): TermName
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
                    Select(builder.tree, "expect": TermName),
                    Apply(
                      Select(Literal(Constant(1)), ("+": TermName).encodedName),
                      Select(Ident(xs), "length": TermName) :: Nil) :: Nil),
                  ("+=": TermName).encodedName),
                elem.tree :: Nil),
              ("++=": TermName).encodedName),
            Ident(xs) :: Nil),
          "state": TermName)))
  }
  
  def ++ [A]
      (these: Expr[Index[A]], those: Expr[Index[A]])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs = fresh("xs$"): TermName
    val ys = fresh("ys$"): TermName
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
                    Select(builder.tree, "expect": TermName),
                    Apply(
                      Select(Select(Ident(xs), "length": TermName), ("+": TermName).encodedName),
                      Select(Ident(ys), "length": TermName) :: Nil) :: Nil),
                  ("++=": TermName).encodedName),
                Ident(xs) :: Nil),
              ("++=": TermName).encodedName),
            Ident(ys) :: Nil),
          "state": TermName)))
  }
  
  protected def BuilderTypeTag(builder: Expr[Builder[_]]): WeakTypeTag[builder.value.type] =
    WeakTypeTag[builder.value.type](builder.tree.symbol match {
      case sym: TermSymbol if sym.isStable => singleType(NoPrefix, sym)
      case _ => builder.actualType
    })
  
  protected def BuilderStateTag
      (builder: Expr[Builder[_]])
      (implicit BuilderTypeTag: WeakTypeTag[builder.value.type])
    : WeakTypeTag[builder.value.State] = {
    val BuilderTpc = mirror.staticClass("basis.collections.Builder").toType
    val BuilderStateSym = BuilderTpc member newTypeName("State")
    val BuilderStateTpe = typeRef(BuilderTypeTag.tpe, BuilderStateSym, Nil).normalize
    WeakTypeTag[builder.value.State](BuilderStateTpe)
  }
  
  implicit protected def MaybeTag[A : WeakTypeTag]: WeakTypeTag[Maybe[A]] = {
    val BasisControl = mirror.staticPackage("basis.control").moduleClass
    val MaybeTpc = BasisControl.typeSignature.member(newTypeName("Maybe")).asType.toType
    val MaybeATpe = appliedType(MaybeTpc, weakTypeOf[A] :: Nil)
    WeakTypeTag[Maybe[A]](MaybeATpe)
  }
  
  implicit private def Tuple2Tag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[(A, B)] = {
    val Tuple2Tpc = mirror.staticClass("scala.Tuple2").toType
    val Tuple2ABTpe = appliedType(Tuple2Tpc, weakTypeOf[A] :: weakTypeOf[B] :: Nil)
    WeakTypeTag[(A, B)](Tuple2ABTpe)
  }
  
  implicit private def UnsupportedOperationExceptionTag: WeakTypeTag[UnsupportedOperationException] =
    WeakTypeTag(mirror.staticClass("java.lang.UnsupportedOperationException").toType)
  
  private def BasisControl: Tree =
    Select(Select(Ident(nme.ROOTPKG), "basis": TermName), "control": TermName)
  
  private def ScalaPartialFunction: Tree =
    Select(Select(Ident(nme.ROOTPKG), "scala": TermName), "PartialFunction": TermName)
  
  private def JavaLang: Tree =
    Select(Select(Ident(nme.ROOTPKG), "java": TermName), "lang": TermName)
  
  private def max(x: Tree, y: Tree): Tree =
    Apply(Select(Select(JavaLang, "Math": TermName), "max": TermName), x :: y :: Nil)
  
  private def min(x: Tree, y: Tree): Tree =
    Apply(Select(Select(JavaLang, "Math": TermName), "min": TermName), x :: y :: Nil)
}
