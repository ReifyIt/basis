//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._
import scala.reflect.macros._

private[sequential] abstract class ArrayMacros(val c: blackbox.Context) {
  import c.{ Expr, fresh, mirror, WeakTypeTag, literalUnit }
  import c.universe.{ Traverser => _, _ }
  import c.universe.internal._

  def these: Expr[Array[_]]

  // This logic is to make it easy to use common code across methods
  // which return Option/Maybe and methods which return the underlying
  // type, without there being any penalty for the unwrapped case
  // (i.e. we don't want to generate an identity closure and apply it.)
  private val IdExprVal: Expr[Any] => Expr[Any] = x => x
  private def IdExpr[A]: Expr[A] => Expr[A]  = IdExprVal.asInstanceOf[Expr[A] => Expr[A]]
  private def isIdExpr(x: AnyRef) = x eq IdExprVal

  def isEmpty[A] = Expr[Boolean](q"$these.length == 0")

  private def foldCommon[A, B](xs: Expr[Array[_]])(start: Int, zero: Expr[B])(op: Expr[(B, A) => B]) = Expr[B](q"""{
    val len = $xs.length
    var i = $start
    var acc = $zero
    while (i < len) {
      acc = $op(acc, $xs(i))
      i += 1
    }
    acc
  }""")

  // Expr[B] => Expr[C], now we're metaprogramming!
  private def reduceCommon[A, B, C](xs: Expr[Array[_]])(none: Expr[C], some: Expr[B] => Expr[C])(op: Expr[(B, A) => B]) = {
    def fold: Expr[B] = foldCommon[A, B](xs)(1, Expr(q"$xs(0)"))(op)
    def result: Expr[C] = if (isIdExpr(some)) fold.asInstanceOf[Expr[C]] else some(fold)

    Expr[C](q"if ($xs.length == 0) $none else $result")
  }

  def foreach[A: WeakTypeTag, U](f: Expr[A => U]) = Expr[Unit](q"""{
    val len = $these.length
    var i = 0
    while (i < len) {
      $f($these(i))
      i += 1
    }
  }""")

  def foldLeft[A: WeakTypeTag, B : WeakTypeTag](z: Expr[B])(op: Expr[(B, A) => B]) =
    foldCommon[A, B](these)(0, z)(op)

  def reduceLeft[A, B >: A : WeakTypeTag](op: Expr[(B, A) => B]) =
    reduceCommon[A, B, B](these)(Expr[B](q"""throw new UnsupportedOperationException("empty reduce")"""), IdExpr[B])(op)

  def mayReduceLeft[A, B >: A : WeakTypeTag](op: Expr[(B, A) => B]) =
    reduceCommon[A, B, Maybe[B]](these)(Expr[Maybe[B]](q"basis.util.Trap"), (res: Expr[B]) => Expr[Maybe[B]](q"basis.util.Bind($res)"))(op)

  def foldRight[A, B : WeakTypeTag](z: Expr[B])(op: Expr[(A, B) => B]) = Expr[B](q"""{
    var i = $these.length - 1
    var acc = $z
    while (i >= 0) {
      acc = $op($these(i), acc)
      i -= 1
    }
    acc
  }""")

  def reduceRight[A, B >: A : WeakTypeTag](op: Expr[(A, B) => B]): Expr[B] = {
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
              Literal(Constant("empty reduce")) :: Nil)),
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

  def mayReduceRight[A, B >: A : WeakTypeTag](op: Expr[(A, B) => B]): Expr[Maybe[B]] = {
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
          Select(BasisUtil, "Trap": TermName),
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
            Apply(Select(BasisUtil, "Bind": TermName), Ident(r) :: Nil)))))
  }

  def find[A: WeakTypeTag](p: Expr[A => Boolean]) = Expr[Maybe[A]](q"""{
    val xs = $these
    var i = 0
    val n = xs.length
    def loop(i: Int): basis.util.Maybe[${weakTypeOf[A]}] = (
      if (i >= n) basis.util.Trap
      else if ($p(xs(i))) basis.util.Bind(xs(i))
      else loop(i + 1)
    )
    loop(0)
  }""")

  def forall[A: WeakTypeTag](p: Expr[A => Boolean]) = Expr[Boolean](q"""{
    val xs = $these
    val n = xs.length
    def loop(i: Int): Boolean = (i >= n) || ($p(xs(i)) && loop(i + 1))
    loop(0)
  }""")

  def exists[A](p: Expr[A => Boolean]) = Expr[Boolean](q"""{
    val xs = $these
    val n = xs.length
    def loop(i: Int): Boolean = (i < n) && ($p(xs(i)) || loop(i + 1))
    loop(0)
  }""")

  def count[A](p: Expr[A => Boolean]) = Expr[Int](q"""{
    val xs = $these
    val n = xs.length
    def loop(i: Int, acc: Int): Boolean = if (i >= n) acc else loop(i + 1, acc + ( if ($p(xs(i))) 1 else 0 ))
    loop(0, 0)
  }""")

  def choose[A, B : WeakTypeTag](q: Expr[PartialFunction[A, B]]): Expr[Maybe[B]] = {
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
        ValDef(Modifiers(Flag.MUTABLE), r, TypeTree(weakTypeOf[Maybe[B]]), Select(BasisUtil, "Trap": TermName)) ::
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
                    Select(BasisUtil, "Bind": TermName),
                    Apply(
                      Select(Ident(f), "applyOrElse": TermName),
                      Ident(x) :: Select(ScalaPartialFunction, "empty": TermName) :: Nil) :: Nil)),
                Block(
                  Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
                  Apply(Ident(loop), Nil)))),
            EmptyTree)) :: Nil,
        Ident(r)))
  }

  def collect[A, B](q: Expr[PartialFunction[A, B]])(builder: Expr[Builder[B]]): Expr[builder.value.State] = {
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
                    Select(Ident(f), "applyOrElse": TermName),
                    Ident(x) :: Select(ScalaPartialFunction, "empty": TermName) :: Nil) :: Nil),
                EmptyTree) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def map[A, B](f: Expr[A => B])(builder: Expr[Builder[B]]): Expr[builder.value.State] = {
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

  def flatMap[A, B](f: Expr[A => Traverser[B]])(builder: Expr[Builder[B]]): Expr[builder.value.State] = {
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

  def filter[A](p: Expr[A => Boolean])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
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

  def dropWhile[A](p: Expr[A => Boolean])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
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

  def takeWhile[A](p: Expr[A => Boolean])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
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

  def span[A](p: Expr[A => Boolean])(builder1: Expr[Builder[A]], builder2: Expr[Builder[A]]): Expr[(builder1.value.State, builder2.value.State)] = {
    val xs    = fresh("xs$"): TermName
    val i     = fresh("i$"): TermName
    val n     = fresh("n$"): TermName
    val a     = fresh("b$"): TermName
    val b     = fresh("b$"): TermName
    val loop1 = fresh("loop$"): TermName
    val x     = fresh("x$"): TermName
    val loop2 = fresh("loop$"): TermName
    implicit val builder1TypeTag = BuilderTypeTag(builder1)
    implicit val builder2TypeTag = BuilderTypeTag(builder2)
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

  def drop[A](lower: Expr[Int])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
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

  def take[A](upper: Expr[Int])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
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

  def slice[A](lower: Expr[Int], upper: Expr[Int])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
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

  def reverse[A](builder: Expr[Builder[A]]): Expr[builder.value.State] = {
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

  def zip[A : WeakTypeTag, B : WeakTypeTag](those: Expr[Array[B]])(builder: Expr[Builder[(A, B)]]): Expr[builder.value.State] = {
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

  def :+ [A](elem: Expr[A])(builder: Expr[ArrayBuilder[A]]): Expr[builder.value.State] = {
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

  def +: [A](elem: Expr[A])(builder: Expr[ArrayBuilder[A]]): Expr[builder.value.State] = {
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

  def ++ [A](those: Expr[Array[A]])(builder: Expr[ArrayBuilder[A]]): Expr[builder.value.State] = {
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
    val BuilderStateSym = BuilderTpc.member("State": TypeName)
    val BuilderStateTpe = typeRef(BuilderTypeTag.tpe, BuilderStateSym, Nil).normalize
    WeakTypeTag[builder.value.State](BuilderStateTpe)
  }

  implicit protected def ArrayTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[Array[A]] =
    WeakTypeTag(appliedType(definitions.ArrayClass.toTypeConstructor, A.tpe :: Nil))

  implicit protected def IteratorTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[Iterator[A]] =
    WeakTypeTag(appliedType(mirror.staticClass(s"basis.collections.Iterator").toTypeConstructor, A.tpe :: Nil))

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

  private def mathBinaryIntExpr(name: TermName)(x: Tree, y: Tree): Tree = {
    val p1 = Expr[Int](x)
    val p2 = Expr[Int](y)
    q"java.lang.Math.$name($p1, $p2)"
  }

  private def max(x: Tree, y: Tree): Tree = mathBinaryIntExpr("max")(x, y)
  private def min(x: Tree, y: Tree): Tree = mathBinaryIntExpr("min")(x, y)
}
