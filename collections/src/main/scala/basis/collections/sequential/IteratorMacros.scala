//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._
import scala.reflect.macros._

private[sequential] abstract class IteratorMacros(override val c: blackbox.Context) extends TraverserMacros {
  import c.{ Expr, fresh, mirror, WeakTypeTag }
  import c.universe.{ Traverser => _, _ }
  import c.universe.internal._

  override def these: Expr[Iterator[_]]

  def foreach[A, U](f: Expr[A => U]): Expr[Unit] = Expr[Unit](q"""{
    val xs = $these
    while (!xs.isEmpty) {
      $f(xs.head)
      xs.step()
    }
  }""")

  def foldLeft[A, B : WeakTypeTag](z: Expr[B])(op: Expr[(B, A) => B]): Expr[B] = Expr[B](q"""{
    val xs = $these
    var r = $z
    while (!xs.isEmpty) {
      r = $op(r, xs.head)
      xs.step()
    }
    r
  }""")

  def reduceLeft[A, B >: A](op: Expr[(B, A) => B])(implicit B: WeakTypeTag[B]): Expr[B] = Expr[B](q"""{
    val xs = $these
    if (xs.isEmpty) throw new _root_.java.lang.UnsupportedOperationException("empty reduce")
    else {
      var r = xs.head: $B
      xs.step()
      while (!xs.isEmpty) {
        r = $op(r, xs.head)
        xs.step()
      }
      r
    }
  }""")

  def mayReduceLeft[A, B >: A](op: Expr[(B, A) => B])(implicit B: WeakTypeTag[B]): Expr[Maybe[B]] = Expr[Maybe[B]](q"""{
    val xs = $these
    if (xs.isEmpty) _root_.basis.util.Trap
    else {
      var r = xs.head: $B
      xs.step()
      while (!xs.isEmpty) {
        r = $op(r, xs.head)
        xs.step()
      }
      _root_.basis.util.Bind(r)
    }
  }""")

  def find[A : WeakTypeTag](p: Expr[A => Boolean]): Expr[Maybe[A]] = {
    implicit val MaybeA = MaybeTag[A]
    Expr[Maybe[A]](q"""{
      val xs = $these
      var r = _root_.basis.util.Trap: $MaybeA
      while (!xs.isEmpty && {
        val x = xs.head
        !$p(x) && { xs.step(); true } || { r = _root_.basis.util.Bind(x); false }
      }) ()
      r
    }""")
  }

  def forall[A](p: Expr[A => Boolean]): Expr[Boolean] = Expr[Boolean](q"""{
    val xs = $these
    var r = true
    while (!xs.isEmpty && ($p(xs.head) && { xs.step(); true } || { r = false; false })) ()
    r
  }""")

  def exists[A](p: Expr[A => Boolean]): Expr[Boolean] = Expr[Boolean](q"""{
    val xs = $these
    var r = false
    while (!xs.isEmpty && ($p(xs.head) && { r = true; false } || { xs.step(); true })) ()
    r
  }""")

  def count[A](p: Expr[A => Boolean]): Expr[Int] = Expr[Int](q"""{
    val xs = $these
    var t = 0
    while (!xs.isEmpty) {
      if ($p(xs.head)) t += 1
      xs.step()
    }
    t
  }""")

  def choose[A, B : WeakTypeTag](q: Expr[PartialFunction[A, B]]): Expr[Maybe[B]] = {
    implicit val MaybeB = MaybeTag[B]
    Expr[Maybe[B]](q"""{
      val xs = $these
      var r = _root_.basis.util.Trap: $MaybeB
      val f = $q
      while (!xs.isEmpty && {
        val x = xs.head
        f.isDefinedAt(x) && { r = _root_.basis.util.Bind(f.applyOrElse(x, _root_.scala.PartialFunction.empty)); false } || { xs.step(); true }
      }) ()
      r
    }""")
  }

  def collect[A, B](q: Expr[PartialFunction[A, B]])(builder: Expr[Builder[B]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      val b = $builder: $builderType
      val f = $q
      while (!xs.isEmpty) {
        val x = xs.head
        if (f.isDefinedAt(x)) b.append(f.applyOrElse(x, _root_.scala.PartialFunction.empty))
        xs.step()
      }
      b.state
    }""")
  }

  def map[A, B](f: Expr[A => B])(builder: Expr[Builder[B]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      val b = $builder: $builderType
      while (!xs.isEmpty) {
        b.append($f(xs.head))
        xs.step()
      }
      b.state
    }""")
  }

  def flatMap[A, B](f: Expr[A => Traverser[B]])(builder: Expr[Builder[B]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      val b = $builder: $builderType
      while (!xs.isEmpty) {
        b.appendAll($f(xs.head))
        xs.step()
      }
      b.state
    }""")
  }

  def filter[A](p: Expr[A => Boolean])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
    implicit val builderType = BuilderTypeTag(builder)
    implicit val builderState = BuilderStateTag(builder)
    Expr[builder.value.State](q"""{
      val xs = $these
      val b = $builder: $builderType
      while (!xs.isEmpty) {
        val x = xs.head
        if ($p(x)) b.append(x)
        xs.step()
      }
      b.state
    }""")
  }

  def dropWhile[A](p: Expr[A => Boolean])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
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

  def takeWhile[A](p: Expr[A => Boolean])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
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

  def span[A](p: Expr[A => Boolean])(builder1: Expr[Builder[A]], builder2: Expr[Builder[A]]): Expr[(builder1.value.State, builder2.value.State)] = {
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

  def drop[A](lower: Expr[Int])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
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

  def slice[A](lower: Expr[Int], upper: Expr[Int])(builder: Expr[Builder[A]]): Expr[builder.value.State] = {
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

  def zip[A : WeakTypeTag, B : WeakTypeTag](those: Expr[Iterator[B]])(builder: Expr[Builder[(A, B)]]): Expr[builder.value.State] = {
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

  def zipContainer[A : WeakTypeTag, B : WeakTypeTag](those: Expr[Container[B]])(builder: Expr[Builder[(A, B)]]): Expr[builder.value.State] =
    zip[A, B](Expr[Iterator[B]](q"$those.iterator"))(builder)

  protected override def BuilderTypeTag(builder: Expr[Builder[_]]): WeakTypeTag[builder.value.type] =
    WeakTypeTag[builder.value.type](builder.tree.symbol match {
      case sym: TermSymbol if sym.isStable => singleType(NoPrefix, sym)
      case _ => builder.actualType
    })

  protected override def BuilderStateTag
      (builder: Expr[Builder[_]])
      (implicit BuilderTypeTag: WeakTypeTag[builder.value.type])
    : WeakTypeTag[builder.value.State] = {
    val BuilderTpc = mirror.staticClass("basis.collections.Builder").toType
    val BuilderStateSym = BuilderTpc.member("State": TypeName)
    val BuilderStateTpe = typeRef(BuilderTypeTag.tpe, BuilderStateSym, Nil).normalize
    WeakTypeTag[builder.value.State](BuilderStateTpe)
  }

  implicit protected def IteratorTag[A](implicit A: WeakTypeTag[A]): WeakTypeTag[Iterator[A]] =
    WeakTypeTag(appliedType(mirror.staticClass(s"basis.collections.Iterator").toTypeConstructor, A.tpe :: Nil))

  implicit protected def MaybeTag[A : WeakTypeTag]: WeakTypeTag[Maybe[A]] = {
    val BasisUtil = mirror.staticPackage("basis.util").moduleClass
    val MaybeTpc = BasisUtil.typeSignature.member("Maybe": TypeName).asType.toTypeConstructor
    val MaybeATpe = appliedType(MaybeTpc, weakTypeOf[A] :: Nil)
    WeakTypeTag[Maybe[A]](MaybeATpe)
  }

  implicit private def Tuple2Tag[A : WeakTypeTag, B : WeakTypeTag]: WeakTypeTag[(A, B)] = {
    val Tuple2Tpc = mirror.staticClass("scala.Tuple2").toTypeConstructor
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
