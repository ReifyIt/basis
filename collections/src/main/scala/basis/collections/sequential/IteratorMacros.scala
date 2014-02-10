//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._
import scala.collection.immutable.{ ::, Nil }
import scala.reflect.macros.Context

private[sequential] class IteratorMacros[C <: Context](val context: C) {
  import context.{ Expr, fresh, mirror, WeakTypeTag }
  import universe.{ Traverser => _, _ }

  val universe: context.universe.type = context.universe

  def foreach[A, U]
      (these: Expr[Iterator[A]])
      (f: Expr[A => U])
    : Expr[Unit] = {
    val xs   = fresh("xs$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[Unit](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) :: Nil,
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              Apply(f.tree, Select(Ident(xs), "head": TermName) :: Nil) ::
              Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree))))
  }

  def foldLeft[A, B : WeakTypeTag]
      (these: Expr[Iterator[A]])
      (z: Expr[B])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = fresh("xs$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[B](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), z.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head": TermName) :: Nil)) ::
              Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r)))
  }

  def reduceLeft[A, B >: A : WeakTypeTag]
      (these: Expr[Iterator[A]])
      (op: Expr[(B, A) => B])
    : Expr[B] = {
    val xs   = fresh("xs$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[B](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        If(
          Select(Ident(xs), "isEmpty": TermName),
          Throw(
            Apply(
              Select(New(TypeTree(weakTypeOf[UnsupportedOperationException])), nme.CONSTRUCTOR),
              Literal(Constant("empty reduce")) :: Nil)),
          EmptyTree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Select(Ident(xs), "head": TermName)) ::
        Apply(Select(Ident(xs), "step": TermName), Nil) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head": TermName) :: Nil)) ::
              Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r)))
  }

  def mayReduceLeft[A, B >: A : WeakTypeTag]
      (these: Expr[Iterator[A]])
      (op: Expr[(B, A) => B])
    : Expr[Maybe[B]] = {
    val xs   = fresh("xs$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[Maybe[B]](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) :: Nil,
        If(
          Select(Ident(xs), "isEmpty": TermName),
          Select(BasisUtil, "Trap": TermName),
          Block(
            ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Select(Ident(xs), "head": TermName)) ::
            Apply(Select(Ident(xs), "step": TermName), Nil) ::
            LabelDef(loop, Nil,
              If(
                Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
                Block(
                  Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head": TermName) :: Nil)) ::
                  Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree)) :: Nil,
            Apply(Select(BasisUtil, "Bind": TermName), Ident(r) :: Nil)))))
  }

  def find[A : WeakTypeTag]
      (these: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
    : Expr[Maybe[A]] = {
    val xs   = fresh("xs$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    val x    = fresh("x$"): TermName
    implicit val MaybeATag = MaybeTag[A]
    Expr[Maybe[A]](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[Maybe[A]]), Select(BasisUtil, "Trap": TermName)) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head": TermName)) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Assign(Ident(r), Apply(Select(BasisUtil, "Bind": TermName), Ident(x) :: Nil)),
                Block(
                  Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }

  def forall[A]
      (these: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = fresh("xs$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[Boolean](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(true))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            If(
              Apply(p.tree, Select(Ident(xs), "head": TermName) :: Nil),
              Block(
                Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
                Apply(Ident(loop), Nil)),
              Assign(Ident(r), Literal(Constant(false)))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }

  def exists[A]
      (these: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
    : Expr[Boolean] = {
    val xs   = fresh("xs$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[Boolean](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(false))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            If(
              Apply(p.tree, Select(Ident(xs), "head": TermName) :: Nil),
              Assign(Ident(r), Literal(Constant(true))),
              Block(
                Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
                Apply(Ident(loop), Nil))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }

  def count[A]
      (these: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
    : Expr[Int] = {
    val xs   = fresh("xs$"): TermName
    val t    = fresh("t$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[Int](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), t, TypeTree(), Literal(Constant(0))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              If(
                Apply(p.tree, Select(Ident(xs), "head": TermName) :: Nil),
                Assign(Ident(t), Apply(Select(Ident(t), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)),
                EmptyTree) ::
              Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(t)))
  }

  def choose[A, B : WeakTypeTag]
      (these: Expr[Iterator[A]])
      (q: Expr[PartialFunction[A, B]])
    : Expr[Maybe[B]] = {
    val xs   = fresh("xs$"): TermName
    val r    = fresh("r$"): TermName
    val f    = fresh("q$"): TermName
    val loop = fresh("loop$"): TermName
    val x    = fresh("x$"): TermName
    implicit val MaybeBTag = MaybeTag[B]
    Expr[Maybe[B]](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[Maybe[B]]), Select(BasisUtil, "Trap": TermName)) ::
        ValDef(NoMods, f, TypeTree(), q.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head": TermName)) :: Nil,
              If(
                Apply(Select(Ident(f), "isDefinedAt": TermName), Ident(x) :: Nil),
                Assign(
                  Ident(r),
                  Apply(
                    Select(BasisUtil, "Bind": TermName),
                    Apply(
                      Select(Ident(f), "applyOrElse": TermName),
                      Ident(x) :: Select(ScalaPartialFunction, "empty": TermName) :: Nil) :: Nil)),
                Block(
                  Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }

  def collect[A, B]
      (these: Expr[Iterator[A]])
      (q: Expr[PartialFunction[A, B]])
      (builder: Expr[Builder[B]])
    : Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val b    = fresh("b$"): TermName
    val f    = fresh("q$"): TermName
    val loop = fresh("loop$"): TermName
    val x    = fresh("x$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        ValDef(NoMods, f, TypeTree(), q.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head": TermName)) ::
              If(
                Apply(Select(Ident(f), "isDefinedAt": TermName), Ident(x) :: Nil),
                Apply(
                  Select(Ident(b), "append": TermName),
                  Apply(
                    Select(Ident(f), "applyOrElse": TermName),
                    Ident(x) :: Select(ScalaPartialFunction, "empty": TermName) :: Nil) :: Nil),
                EmptyTree) ::
              Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def map[A, B]
      (these: Expr[Iterator[A]])
      (f: Expr[A => B])
      (builder: Expr[Builder[B]])
    : Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              Apply(
                Select(Ident(b), "append": TermName),
                Apply(f.tree, Select(Ident(xs), "head": TermName) :: Nil) :: Nil) ::
              Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def flatMap[A, B]
      (these: Expr[Iterator[A]])
      (f: Expr[A => Traverser[B]])
      (builder: Expr[Builder[B]])
    : Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              Apply(
                Select(Ident(b), "appendAll": TermName),
                Apply(f.tree, Select(Ident(xs), "head": TermName) :: Nil) :: Nil) ::
              Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def filter[A]
      (these: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    val x    = fresh("x$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head": TermName)) ::
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Apply(Select(Ident(b), "append": TermName), Ident(x) :: Nil),
                EmptyTree) ::
              Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def dropWhile[A]
      (these: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs    = fresh("xs$"): TermName
    val b     = fresh("b$"): TermName
    val loop1 = fresh("loop$"): TermName
    val x     = fresh("x$"): TermName
    val loop2 = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop1, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head": TermName)) ::
              Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Apply(Ident(loop1), Nil),
                Apply(Select(Ident(b), "append": TermName), Ident(x) :: Nil))),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              Apply(Select(Ident(b), "append": TermName), Select(Ident(xs), "head": TermName) :: Nil) ::
              Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def takeWhile[A]
      (these: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    val x    = fresh("x$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head": TermName)) ::
              Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
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
      (these: Expr[Iterator[A]])
      (p: Expr[A => Boolean])
      (builder1: Expr[Builder[A]], builder2: Expr[Builder[A]])
    : Expr[(builder1.value.State, builder2.value.State)] = {
    val xs    = fresh("xs$"): TermName
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
        ValDef(NoMods, a, TypeTree(builder1TypeTag.tpe), builder1.tree) ::
        ValDef(NoMods, b, TypeTree(builder2TypeTag.tpe), builder2.tree) ::
        LabelDef(loop1, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head": TermName)) ::
              Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Block(
                  Apply(Select(Ident(a), "append": TermName), Ident(x) :: Nil) :: Nil,
                  Apply(Ident(loop1), Nil)),
                Apply(Select(Ident(b), "append": TermName), Ident(x) :: Nil))),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              Apply(Select(Ident(b), "append": TermName), Select(Ident(xs), "head": TermName) :: Nil) ::
              Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        Apply(
          Select(New(TypeTree(weakTypeOf[(builder1.value.State, builder2.value.State)])), nme.CONSTRUCTOR),
          Select(Ident(a), "state": TermName) :: Select(Ident(b), "state": TermName) :: Nil)))
  }

  def drop[A]
      (these: Expr[Iterator[A]])
      (lower: Expr[Int])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs    = fresh("xs$"): TermName
    val i     = fresh("i$"): TermName
    val n     = fresh("n$"): TermName
    val b     = fresh("b$"): TermName
    val loop1 = fresh("loop$"): TermName
    val loop2 = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), lower.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop1, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            If(
              Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
              Block(
                Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) ::
                Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
                Apply(Ident(loop1), Nil)),
              EmptyTree),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              Apply(Select(Ident(b), "append": TermName), Select(Ident(xs), "head": TermName) :: Nil) ::
              Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def take[A]
      (these: Expr[Iterator[A]])
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
        ValDef(NoMods, n, TypeTree(), upper.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            If(
              Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
              Block(
                Apply(Select(Ident(b), "append": TermName), Select(Ident(xs), "head": TermName) :: Nil) ::
                Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) ::
                Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
                Apply(Ident(loop), Nil)),
              EmptyTree),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def slice[A]
      (these: Expr[Iterator[A]])
      (lower: Expr[Int], upper: Expr[Int])
      (builder: Expr[Builder[A]])
    : Expr[builder.value.State] = {
    val xs    = fresh("xs$"): TermName
    val i     = fresh("i$"): TermName
    val n     = fresh("n$"): TermName
    val b     = fresh("b$"): TermName
    val loop1 = fresh("loop$"): TermName
    val loop2 = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(Modifiers(Flag.MUTABLE), n, TypeTree(), lower.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop1, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            If(
              Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
              Block(
                Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) ::
                Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
                Apply(Ident(loop1), Nil)),
              EmptyTree),
            EmptyTree)) ::
        Assign(Ident(n), upper.tree) ::
        LabelDef(loop2, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            If(
              Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
              Block(
                Apply(Select(Ident(b), "append": TermName), Select(Ident(xs), "head": TermName) :: Nil) ::
                Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) ::
                Apply(Select(Ident(xs), "step": TermName), Nil) :: Nil,
                Apply(Ident(loop2), Nil)),
              EmptyTree),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def zip[A : WeakTypeTag, B : WeakTypeTag]
      (these: Expr[Iterator[A]], those: Expr[Iterator[B]])
      (builder: Expr[Builder[(A, B)]])
    : Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val ys   = fresh("ys$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(NoMods, xs, TypeTree(), these.tree) ::
        ValDef(NoMods, ys, TypeTree(), those.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            If(
              Select(Select(Ident(ys), "isEmpty": TermName), ("unary_!": TermName).encodedName),
              Block(
                Apply(
                  Select(Ident(b), "append": TermName),
                  Apply(
                    Select(New(TypeTree(weakTypeOf[(A, B)])), nme.CONSTRUCTOR),
                    Select(Ident(xs), "head": TermName) ::
                    Select(Ident(ys), "head": TermName) :: Nil) :: Nil) ::
                Apply(Select(Ident(xs), "step": TermName), Nil) ::
                Apply(Select(Ident(ys), "step": TermName), Nil) :: Nil,
                Apply(Ident(loop), Nil)),
              EmptyTree),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
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
    val BuilderStateSym = BuilderTpc.member("State": TypeName)
    val BuilderStateTpe = typeRef(BuilderTypeTag.tpe, BuilderStateSym, Nil).normalize
    WeakTypeTag[builder.value.State](BuilderStateTpe)
  }

  implicit protected def MaybeTag[A : WeakTypeTag]: WeakTypeTag[Maybe[A]] = {
    val BasisUtil = mirror.staticPackage("basis.util").moduleClass
    val MaybeTpc = BasisUtil.typeSignature.member("Maybe": TypeName).asType.toType
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

  private def BasisUtil: Tree =
    Select(Select(Ident(nme.ROOTPKG), "basis": TermName), "util": TermName)

  private def ScalaPartialFunction: Tree =
    Select(Select(Ident(nme.ROOTPKG), "scala": TermName), "PartialFunction": TermName)
}
