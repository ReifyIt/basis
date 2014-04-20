//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._
import scala.reflect.macros._

private[sequential] abstract class LinearSeqMacros(override val c: blackbox.Context) extends TraverserMacros(c) {
  import c.{ Expr, fresh, mirror, WeakTypeTag }
  import c.universe.{ Traverser => _, _ }

  override def these: Expr[LinearSeq[_]]

  def foreach[A : WeakTypeTag, U](f: Expr[A => U]): Expr[Unit] = {
    val xs   = fresh("xs$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[Unit](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) :: Nil,
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              Apply(f.tree, Select(Ident(xs), "head": TermName) :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree))))
  }

  def foldLeft[A : WeakTypeTag, B : WeakTypeTag](z: Expr[B])(op: Expr[(B, A) => B]): Expr[B] = {
    val xs   = fresh("xs$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[B](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), z.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head": TermName) :: Nil)) ::
              Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r)))
  }

  def reduceLeft[A : WeakTypeTag, B >: A : WeakTypeTag](op: Expr[(B, A) => B]): Expr[B] = {
    val xs   = fresh("xs$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[B](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
        If(
          Select(Ident(xs), "isEmpty": TermName),
          Throw(
            Apply(
              Select(New(TypeTree(weakTypeOf[UnsupportedOperationException])), nme.CONSTRUCTOR),
              Literal(Constant("empty reduce")) :: Nil)),
          EmptyTree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Select(Ident(xs), "head": TermName)) ::
        Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head": TermName) :: Nil)) ::
              Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(r)))
  }

  def mayReduceLeft[A : WeakTypeTag, B >: A : WeakTypeTag](op: Expr[(B, A) => B]): Expr[Maybe[B]] = {
    val xs   = fresh("xs$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[Maybe[B]](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) :: Nil,
        If(
          Select(Ident(xs), "isEmpty": TermName),
          Select(BasisUtil, "Trap": TermName),
          Block(
            ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[B]), Select(Ident(xs), "head": TermName)) ::
            Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) ::
            LabelDef(loop, Nil,
              If(
                Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
                Block(
                  Assign(Ident(r), Apply(op.tree, Ident(r) :: Select(Ident(xs), "head": TermName) :: Nil)) ::
                  Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree)) :: Nil,
            Apply(Select(BasisUtil, "Bind": TermName), Ident(r) :: Nil)))))
  }

  def find[A : WeakTypeTag](p: Expr[A => Boolean]): Expr[Maybe[A]] = {
    val xs   = fresh("xs$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    val x    = fresh("x$"): TermName
    implicit val MaybeATag = MaybeTag[A]
    Expr[Maybe[A]](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
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
                  Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }

  def forall[A : WeakTypeTag](p: Expr[A => Boolean]): Expr[Boolean] = {
    val xs   = fresh("xs$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[Boolean](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(true))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            If(
              Apply(p.tree, Select(Ident(xs), "head": TermName) :: Nil),
              Block(
                Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
                Apply(Ident(loop), Nil)),
              Assign(Ident(r), Literal(Constant(false)))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }

  def exists[A : WeakTypeTag](p: Expr[A => Boolean]): Expr[Boolean] = {
    val xs   = fresh("xs$"): TermName
    val r    = fresh("r$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[Boolean](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(), Literal(Constant(false))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            If(
              Apply(p.tree, Select(Ident(xs), "head": TermName) :: Nil),
              Assign(Ident(r), Literal(Constant(true))),
              Block(
                Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
                Apply(Ident(loop), Nil))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }

  def count[A : WeakTypeTag](p: Expr[A => Boolean]): Expr[Int] = {
    val xs   = fresh("xs$"): TermName
    val t    = fresh("t$"): TermName
    val loop = fresh("loop$"): TermName
    Expr[Int](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), t, TypeTree(), Literal(Constant(0))) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              If(
                Apply(p.tree, Select(Ident(xs), "head": TermName) :: Nil),
                Assign(Ident(t), Apply(Select(Ident(t), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)),
                EmptyTree) ::
              Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Ident(t)))
  }

  def choose[A : WeakTypeTag, B : WeakTypeTag](q: Expr[PartialFunction[A, B]]): Expr[Maybe[B]] = {
    val xs   = fresh("xs$"): TermName
    val r    = fresh("r$"): TermName
    val f    = fresh("q$"): TermName
    val loop = fresh("loop$"): TermName
    val x    = fresh("x$"): TermName
    implicit val MaybeBTag = MaybeTag[B]
    Expr[Maybe[B]](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
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
                  Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }

  def collect[A : WeakTypeTag, B](q: Expr[PartialFunction[A, B]])(builder: Expr[Builder[B]]): Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val b    = fresh("b$"): TermName
    val f    = fresh("q$"): TermName
    val loop = fresh("loop$"): TermName
    val x    = fresh("x$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
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
              Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def map[A : WeakTypeTag, B](f: Expr[A => B])(builder: Expr[Builder[B]]): Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              Apply(
                Select(Ident(b), "append": TermName),
                Apply(f.tree, Select(Ident(xs), "head": TermName) :: Nil) :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def flatMap[A : WeakTypeTag, B](f: Expr[A => Traverser[B]])(builder: Expr[Builder[B]]): Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              Apply(
                Select(Ident(b), "appendAll": TermName),
                Apply(f.tree, Select(Ident(xs), "head": TermName) :: Nil) :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def filter[A : WeakTypeTag](p: Expr[A => Boolean])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    val x    = fresh("x$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
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
              Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def dropWhile[A : WeakTypeTag](p: Expr[A => Boolean])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    val xs    = fresh("xs$"): TermName
    val b     = fresh("b$"): TermName
    val loop1 = fresh("loop$"): TermName
    val x     = fresh("x$"): TermName
    val loop2 = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop1, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head": TermName)) ::
              Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
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
              Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def takeWhile[A : WeakTypeTag](p: Expr[A => Boolean])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    val x    = fresh("x$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
        ValDef(NoMods, b, TypeTree(builderTypeTag.tpe), builder.tree) ::
        LabelDef(loop, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head": TermName)) ::
              Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
              If(
                Apply(p.tree, Ident(x) :: Nil),
                Block(
                  Apply(Select(Ident(b), "append": TermName), Ident(x) :: Nil) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def span[A : WeakTypeTag](p: Expr[A => Boolean])(builder1: Expr[Builder[A]], builder2: Expr[Builder[A]]): Expr[(builder1.value.State, builder2.value.State)] = {
    val xs    = fresh("xs$"): TermName
    val a     = fresh("b$"): TermName
    val b     = fresh("b$"): TermName
    val loop1 = fresh("loop$"): TermName
    val x     = fresh("x$"): TermName
    val loop2 = fresh("loop$"): TermName
    implicit val builder1TypeTag = BuilderTypeTag(builder1)
    implicit val builder2TypeTag = BuilderTypeTag(builder2)
    Expr[(builder1.value.State, builder2.value.State)](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
        ValDef(NoMods, a, TypeTree(builder1TypeTag.tpe), builder1.tree) ::
        ValDef(NoMods, b, TypeTree(builder2TypeTag.tpe), builder2.tree) ::
        LabelDef(loop1, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              ValDef(NoMods, x, TypeTree(), Select(Ident(xs), "head": TermName)) ::
              Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
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
              Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        Apply(
          Select(New(TypeTree(weakTypeOf[(builder1.value.State, builder2.value.State)])), nme.CONSTRUCTOR),
          Select(Ident(a), "state": TermName) :: Select(Ident(b), "state": TermName) :: Nil)))
  }

  def drop[A : WeakTypeTag](lower: Expr[Int])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
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
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
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
                Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
                Apply(Ident(loop1), Nil)),
              EmptyTree),
            EmptyTree)) ::
        LabelDef(loop2, Nil,
          If(
            Select(Select(Ident(xs), "isEmpty": TermName), ("unary_!": TermName).encodedName),
            Block(
              Apply(Select(Ident(b), "append": TermName), Select(Ident(xs), "head": TermName) :: Nil) ::
              Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
              Apply(Ident(loop2), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def take[A : WeakTypeTag](upper: Expr[Int])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
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
                Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
                Apply(Ident(loop), Nil)),
              EmptyTree),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def slice[A : WeakTypeTag](lower: Expr[Int], upper: Expr[Int])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
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
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
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
                Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
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
                Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) :: Nil,
                Apply(Ident(loop2), Nil)),
              EmptyTree),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def zip[A : WeakTypeTag, B : WeakTypeTag](those: Expr[LinearSeq[B]])(builder: Expr[Builder[(A, B)]]): Expr[builder.value.State] = {
    val xs   = fresh("xs$"): TermName
    val ys   = fresh("ys$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val builderTypeTag = BuilderTypeTag(builder)
    implicit val builderStateTag = BuilderStateTag(builder)
    Expr[builder.value.State](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), xs, TypeTree(weakTypeOf[LinearSeq[A]]), these.tree) ::
        ValDef(Modifiers(Flag.MUTABLE), ys, TypeTree(weakTypeOf[LinearSeq[B]]), those.tree) ::
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
                Assign(Ident(xs), Select(Ident(xs), "tail": TermName)) ::
                Assign(Ident(ys), Select(Ident(ys), "tail": TermName)) :: Nil,
                Apply(Ident(loop), Nil)),
              EmptyTree),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  implicit private def UnsupportedOperationExceptionTag: WeakTypeTag[UnsupportedOperationException] =
    WeakTypeTag(mirror.staticClass("java.lang.UnsupportedOperationException").toType)

  private def BasisUtil: Tree =
    Select(Select(Ident(nme.ROOTPKG), "basis": TermName), "util": TermName)

  private def ScalaPartialFunction: Tree =
    Select(Select(Ident(nme.ROOTPKG), "scala": TermName), "PartialFunction": TermName)
}
