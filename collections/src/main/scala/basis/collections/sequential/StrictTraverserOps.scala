//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

import basis.util._

final class StrictTraverserOps[+A, -Family](val __ : Traverser[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[B] with From[Family]): builder.State = {
    __.traverse(new StrictTraverserOps.CollectInto(q)(builder))
    builder.state
  }

  def map[B](f: A => B)(implicit builder: Builder[B] with From[Family]): builder.State = {
    __.traverse(new StrictTraverserOps.MapInto(f)(builder))
    builder.state
  }

  def flatMap[B](f: A => Traverser[B])(implicit builder: Builder[B] with From[Family]): builder.State = {
    __.traverse(new StrictTraverserOps.FlatMapInto(f)(builder))
    builder.state
  }

  def filter(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State = {
    __.traverse(new StrictTraverserOps.FilterInto(p)(builder))
    builder.state
  }

  def withFilter(p: A => Boolean): Traverser[A] =
    new NonStrictTraverserOps.Filter(__, p)

  def dropWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State = {
    __.traverse(new StrictTraverserOps.DropWhileInto(p)(builder))
    builder.state
  }

  def takeWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State = {
    begin { __.traverse(new StrictTraverserOps.TakeWhileInto(p)(builder)) }
    builder.state
  }

  def span(p: A => Boolean)
      (implicit builder1: Builder[A] with From[Family], builder2: Builder[A] with From[Family])
    : (builder1.State, builder2.State) = {
    __.traverse(new StrictTraverserOps.SpanInto(p)(builder1, builder2))
    (builder1.state, builder2.state)
  }

  def drop(lower: Int)(implicit builder: Builder[A] with From[Family]): builder.State = {
    __.traverse(new StrictTraverserOps.DropInto(lower)(builder))
    builder.state
  }

  def take(upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State = {
    begin { __.traverse(new StrictTraverserOps.TakeInto(upper)(builder)) }
    builder.state
  }

  def slice(lower: Int, upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State = {
    begin { __.traverse(new StrictTraverserOps.SliceInto(lower, upper)(builder)) }
    builder.state
  }

  def ++ [B >: A](those: Traverser[B])(implicit builder: Builder[B] with From[Family]): builder.State =
    (builder ++= __ ++= those).state
}

private[sequential] object StrictTraverserOps {
  import scala.runtime._

  final class CollectInto[-A, B](q: PartialFunction[A, B])(builder: Builder[B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) builder.append(q(x))
  }

  final class MapInto[-A, +B](f: A => B)(builder: Builder[B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = builder.append(f(x))
  }

  final class FlatMapInto[-A, +B](f: A => Traverser[B])(builder: Builder[B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = builder.appendAll(f(x))
  }

  final class FilterInto[-A](p: A => Boolean)(builder: Builder[A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) builder.append(x)
  }

  final class DropWhileInto[-A](p: A => Boolean)(builder: Builder[A]) extends AbstractFunction1[A, Unit] {
    private[this] var taking: Boolean = false
    override def apply(x: A): Unit = if (taking || (!p(x) && { taking = true; true })) builder.append(x)
  }

  final class TakeWhileInto[-A](p: A => Boolean)(builder: Builder[A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) builder.append(x) else begin.break()
  }

  final class SpanInto[-A](p: A => Boolean)(builder1: Builder[A], builder2: Builder[A]) extends AbstractFunction1[A, Unit] {
    private[this] var taking: Boolean = false
    override def apply(x: A): Unit = if (!taking && (p(x) || { taking = true; false })) builder1.append(x) else builder2.append(x)
  }

  final class DropInto[-A](lower: Int)(builder: Builder[A]) extends AbstractFunction1[A, Unit] {
    private[this] var i: Int = 0
    override def apply(x: A): Unit = if (i >= lower) builder.append(x) else i += 1
  }

  final class TakeInto[-A](upper: Int)(builder: Builder[A]) extends AbstractFunction1[A, Unit] {
    private[this] var i: Int = 0
    override def apply(x: A): Unit = if (i < upper) { builder.append(x); i += 1 } else begin.break()
  }

  final class SliceInto[-A](lower: Int, upper: Int)(builder: Builder[A]) extends AbstractFunction1[A, Unit] {
    private[this] var l: Int = 0 max lower
    private[this] var u: Int = l max upper
    private[this] var i: Int = 0
    override def apply(x: A): Unit = if (i < u) { if (i >= l) builder.append(x); i += 1 } else begin.break()
  }
}

private[sequential] object StrictTraverserMacros {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Traverser[A]] = {
    import c.{ Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag }
    import c.universe.{ Traverser => _, _ }
    val Apply(_, these :: Nil) = prefix.tree
    implicit val TraverserATag =
      WeakTypeTag[Traverser[A]](
        appliedType(
          mirror.staticClass("basis.collections.Traverser").toType,
          weakTypeOf[A] :: Nil))
    Expr[Traverser[A]](typeCheck(these, weakTypeOf[Traverser[A]]))
  }

  def :+ [A : c.WeakTypeTag]
      (c: Context)
      (elem: c.Expr[A])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new TraverserMacros[c.type](c).:+[A](unApply[A](c), elem)(builder)

  def +: [A : c.WeakTypeTag]
      (c: Context)
      (elem: c.Expr[A])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new TraverserMacros[c.type](c).+:[A](elem, unApply[A](c))(builder)

  def ++ [A : c.WeakTypeTag]
      (c: Context)
      (those: c.Expr[Traverser[A]])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new TraverserMacros[c.type](c).++[A](unApply[A](c), those)(builder)
}
