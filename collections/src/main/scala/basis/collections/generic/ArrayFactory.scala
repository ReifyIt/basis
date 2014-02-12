//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package generic

import scala.reflect._
import basis.util._

trait ArrayFactory[+CC[_]] {
  def empty[A](implicit A: ClassTag[A]): CC[A] = Builder[A]().state

  def apply[A](elems: A*): CC[A] =
    macro ArrayFactory.apply[CC, A]

  def fill[A](count: Int)(elem: => A): CC[A] =
    macro ArrayFactory.fill[CC, A]

  def tabulate[A](count: Int)(f: Int => A): CC[A] =
    macro ArrayFactory.tabulate[CC, A]

  def iterate[A](start: A, count: Int)(f: A => A): CC[A] =
    macro ArrayFactory.iterate[CC, A]

  def coerce[A](elems: Traverser[A]): CC[A] = {
    val builder = Builder[A]()(ClassTag.AnyRef.asInstanceOf[ClassTag[A]])
    elems.traverse(new Buffer.Append(builder))
    builder.state
  }

  def from[A](elems: Traverser[A])(implicit A: ClassTag[A]): CC[A] = {
    val builder = Builder[A]()
    elems.traverse(new Buffer.Append(builder))
    builder.state
  }

  def from[A](elems: scala.collection.TraversableOnce[A])(implicit A: ClassTag[A]): CC[A] = {
    val builder = Builder[A]()
    elems.foreach(new Buffer.Append(builder))
    builder.state
  }

  implicit def Builder[A]()(implicit A: ClassTag[A]): Builder[A] with State[CC[A]]
}

private[generic] object ArrayFactory {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  def apply[CC[_], A]
      (c: ContextWithPre[ArrayFactory[CC]])
      (elems: c.Expr[A]*)
      (implicit CCTag: c.WeakTypeTag[CC[_]], ATag: c.WeakTypeTag[A])
    : c.Expr[CC[A]] = {

    import c.{ Expr, prefix, weakTypeOf, WeakTypeTag }
    import c.universe._

    var b = Apply(TypeApply(Select(prefix.tree, "Builder": TermName), TypeTree(weakTypeOf[A]) :: Nil), Nil)
    b = Apply(Select(b, "expect": TermName), Literal(Constant(elems.length)) :: Nil)

    val xs = elems.iterator
    while (xs.hasNext) b = Apply(Select(b, ("+=": TermName).encodedName), xs.next().tree :: Nil)

    implicit val CCATag = applied[CC, A](c)
    Expr[CC[A]](Select(b, "state": TermName))
  }

  def fill[CC[_], A]
      (c: ContextWithPre[ArrayFactory[CC]])
      (count: c.Expr[Int])
      (elem: c.Expr[A])
      (implicit CCTag : c.WeakTypeTag[CC[_]], ATag : c.WeakTypeTag[A])
    : c.Expr[CC[A]] = {
    import c.{ Expr, fresh, prefix, weakTypeOf, WeakTypeTag }
    import c.universe._
    val i    = fresh("i$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val CCATag = applied[CC, A](c)
    Expr[CC[A]](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), count.tree) ::
        ValDef(NoMods, b, TypeTree(),
          Apply(
            Select(Apply(TypeApply(Select(prefix.tree, "Builder": TermName), TypeTree(weakTypeOf[A]) :: Nil), Nil), "expect": TermName),
            Ident(i) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), (">": TermName).encodedName), Literal(Constant(0)) :: Nil),
            Block(
              Apply(Select(Ident(b), ("+=": TermName).encodedName), elem.tree :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("-": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def tabulate[CC[_], A]
      (c: ContextWithPre[ArrayFactory[CC]])
      (count: c.Expr[Int])
      (f: c.Expr[Int => A])
      (implicit CCTag : c.WeakTypeTag[CC[_]], ATag : c.WeakTypeTag[A])
    : c.Expr[CC[A]] = {
    import c.{ Expr, fresh, prefix, weakTypeOf, WeakTypeTag }
    import c.universe._
    val i    = fresh("i$"): TermName
    val n    = fresh("n$"): TermName
    val b    = fresh("b$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val CCATag = applied[CC, A](c)
    Expr[CC[A]](
      Block(
        ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(0))) ::
        ValDef(NoMods, n, TypeTree(), count.tree) ::
        ValDef(NoMods, b, TypeTree(),
          Apply(
            Select(Apply(TypeApply(Select(prefix.tree, "Builder": TermName), TypeTree(weakTypeOf[A]) :: Nil), Nil), "expect": TermName),
            Ident(n) :: Nil)) ::
        LabelDef(loop, Nil,
          If(
            Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
            Block(
              Apply(Select(Ident(b), ("+=": TermName).encodedName), Apply(f.tree, Ident(i) :: Nil) :: Nil) ::
              Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
              Apply(Ident(loop), Nil)),
            EmptyTree)) :: Nil,
        Select(Ident(b), "state": TermName)))
  }

  def iterate[CC[_], A]
      (c: ContextWithPre[ArrayFactory[CC]])
      (start: c.Expr[A], count: c.Expr[Int])
      (f: c.Expr[A => A])
      (implicit CCTag : c.WeakTypeTag[CC[_]], ATag : c.WeakTypeTag[A])
    : c.Expr[CC[A]] = {
    import c.{ Expr, fresh, prefix, weakTypeOf, WeakTypeTag }
    import c.universe._
    val n    = fresh("n$"): TermName
    val b    = fresh("b$"): TermName
    val a    = fresh("a$"): TermName
    val i    = fresh("i$"): TermName
    val loop = fresh("loop$"): TermName
    implicit val CCATag = applied[CC, A](c)
    Expr[CC[A]](
      Block(
        ValDef(NoMods, n, TypeTree(), count.tree) ::
        ValDef(NoMods, b, TypeTree(),
          Apply(
            Select(Apply(TypeApply(Select(prefix.tree, "Builder": TermName), TypeTree(weakTypeOf[A]) :: Nil), Nil), "expect": TermName),
            Ident(n) :: Nil)) ::
        If(
          Apply(Select(Ident(n), (">": TermName).encodedName), Literal(Constant(0)) :: Nil),
          Block(
            ValDef(Modifiers(Flag.MUTABLE), a, TypeTree(), start.tree) ::
            Apply(Select(Ident(b), ("+=": TermName).encodedName), Ident(a) :: Nil) ::
            ValDef(Modifiers(Flag.MUTABLE), i, TypeTree(), Literal(Constant(1))) :: Nil,
            LabelDef(loop, Nil,
              If(
                Apply(Select(Ident(i), ("<": TermName).encodedName), Ident(n) :: Nil),
                Block(
                  Assign(Ident(a), Apply(f.tree, Ident(a) :: Nil)) ::
                  Apply(Select(Ident(b), ("+=": TermName).encodedName), Ident(a) :: Nil) ::
                  Assign(Ident(i), Apply(Select(Ident(i), ("+": TermName).encodedName), Literal(Constant(1)) :: Nil)) :: Nil,
                  Apply(Ident(loop), Nil)),
                EmptyTree))),
          EmptyTree) :: Nil,
        Select(Ident(b), "state": TermName)))
  }
}
