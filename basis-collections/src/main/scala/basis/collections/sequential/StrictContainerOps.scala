//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

final class StrictContainerOps[+A, -Family](val __ : Container[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[B] with From[Family]): builder.State =
    macro StrictContainerOps.collect[A, B]

  def map[B](f: A => B)(implicit builder: Builder[B] with From[Family]): builder.State =
    macro StrictContainerOps.map[A, B]

  def flatMap[B](f: A => Traverser[B])(implicit builder: Builder[B] with From[Family]): builder.State =
    macro StrictContainerOps.flatMap[A, B]

  def filter(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictContainerOps.filter[A]

  def withFilter(p: A => Boolean): Container[A] =
    new NonStrictContainerOps.Filter(__, p)

  def dropWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictContainerOps.dropWhile[A]

  def takeWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictContainerOps.takeWhile[A]

  //FIXME: SI-6447
  //def span(p: A => Boolean)
  //    (implicit builder1: Builder[A] with From[Family], builder2: Builder[A] with From[Family])
  //  : (builder1.State, builder2.State) =
  //  macro StrictContainerOps.span[A]

  def drop(lower: Int)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictContainerOps.drop[A]

  def take(upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictContainerOps.take[A]

  def slice(lower: Int, upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictContainerOps.slice[A]

  def zip[B](those: Container[B])(implicit builder: Builder[(A, B)] with From[Family]): builder.State =
    macro StrictContainerOps.zip[A, B]

  def ++ [B >: A](those: Container[B])(implicit builder: Builder[B] with From[Family]): builder.State =
    macro StrictTraverserOps.++[B]
}

private[sequential] object StrictContainerOps {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Container[A]] = {
    import c.{ Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag }
    import c.universe._
    val Apply(_, these :: Nil) = prefix.tree
    implicit val ContainerATag =
      WeakTypeTag[Container[A]](
        appliedType(
          mirror.staticClass("basis.collections.Container").toType,
          weakTypeOf[A] :: Nil))
    Expr[Container[A]](typeCheck(these, weakTypeOf[Container[A]]))
  }

  private def iterator[A : c.WeakTypeTag](c: Context)(these: c.Expr[Container[A]]): c.Expr[Iterator[A]] = {
    import c.{ Expr, mirror, weakTypeOf, WeakTypeTag }
    import c.universe._
    implicit val IteratorATag =
      WeakTypeTag[Iterator[A]](
        appliedType(
          mirror.staticClass("basis.collections.Iterator").toType,
          weakTypeOf[A] :: Nil))
    Expr[Iterator[A]](Select(these.tree, "iterator": TermName))
  }

  def collect[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
      (builder: c.Expr[Builder[B]])
    : c.Expr[builder.value.State] =
    new IteratorMacros[c.type](c).collect[A, B](iterator(c)(unApply[A](c)))(q)(builder)

  def map[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => B])
      (builder: c.Expr[Builder[B]])
    : c.Expr[builder.value.State] =
    new IteratorMacros[c.type](c).map[A, B](iterator(c)(unApply[A](c)))(f)(builder)

  def flatMap[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => Traverser[B]])
      (builder: c.Expr[Builder[B]])
    : c.Expr[builder.value.State] =
    new IteratorMacros[c.type](c).flatMap[A, B](iterator(c)(unApply[A](c)))(f)(builder)

  def filter[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new IteratorMacros[c.type](c).filter[A](iterator(c)(unApply[A](c)))(p)(builder)

  def dropWhile[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new IteratorMacros[c.type](c).dropWhile[A](iterator(c)(unApply[A](c)))(p)(builder)

  def takeWhile[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new IteratorMacros[c.type](c).takeWhile[A](iterator(c)(unApply[A](c)))(p)(builder)

  def span[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder1: c.Expr[Builder[A]], builder2: c.Expr[Builder[A]])
    : c.Expr[(builder1.value.State, builder2.value.State)] =
    new IteratorMacros[c.type](c).span[A](iterator(c)(unApply[A](c)))(p)(builder1, builder2)

  def drop[A : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new IteratorMacros[c.type](c).drop[A](iterator(c)(unApply[A](c)))(lower)(builder)

  def take[A : c.WeakTypeTag]
      (c: Context)
      (upper: c.Expr[Int])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new IteratorMacros[c.type](c).take[A](iterator(c)(unApply[A](c)))(upper)(builder)

  def slice[A : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int], upper: c.Expr[Int])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new IteratorMacros[c.type](c).slice[A](iterator(c)(unApply[A](c)))(lower, upper)(builder)

  def zip[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (those: c.Expr[Container[B]])
      (builder: c.Expr[Builder[(A, B)]])
    : c.Expr[builder.value.State] =
    new IteratorMacros[c.type](c).zip[A, B](iterator(c)(unApply[A](c)), iterator(c)(those))(builder)
}
