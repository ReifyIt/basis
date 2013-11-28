//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

final class StrictLinearSeqOps[+A, -Family](val __ : LinearSeq[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[B] with From[Family]): builder.State =
    macro StrictLinearSeqOps.collect[A, B]

  def map[B](f: A => B)(implicit builder: Builder[B] with From[Family]): builder.State =
    macro StrictLinearSeqOps.map[A, B]

  def flatMap[B](f: A => Traverser[B])(implicit builder: Builder[B] with From[Family]): builder.State =
    macro StrictLinearSeqOps.flatMap[A, B]

  def filter(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictLinearSeqOps.filter[A]

  def withFilter(p: A => Boolean): LinearSeq[A] =
    new NonStrictLinearSeqOps.Filter(__, p)

  def dropWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictLinearSeqOps.dropWhile[A]

  def takeWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictLinearSeqOps.takeWhile[A]

  //FIXME: SI-6447
  //def span(p: A => Boolean)
  //    (implicit builder1: Builder[A] with From[Family], builder2: Builder[A] with From[Family])
  //  : (builder1.State, builder2.State) =
  //  macro StrictLinearSeqOps.span[A]

  def drop(lower: Int)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictLinearSeqOps.drop[A]

  def take(upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictLinearSeqOps.take[A]

  def slice(lower: Int, upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictLinearSeqOps.slice[A]

  def zip[B](those: LinearSeq[B])(implicit builder: Builder[(A, B)] with From[Family]): builder.State =
    macro StrictLinearSeqOps.zip[A, B]

  def :+ (elem: A)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictTraverserOps.:+[A]

  def +: (elem: A)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictTraverserOps.+:[A]

  def ++ [B >: A](those: LinearSeq[B])(implicit builder: Builder[B] with From[Family]): builder.State =
    macro StrictTraverserOps.++[B]
}

private[sequential] object StrictLinearSeqOps {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[LinearSeq[A]] = {
    import c.{ Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag }
    import c.universe._
    val Apply(_, these :: Nil) = prefix.tree
    implicit val LinkATag =
      WeakTypeTag[LinearSeq[A]](
        appliedType(
          mirror.staticClass("basis.collections.LinearSeq").toType,
          weakTypeOf[A] :: Nil))
    Expr[LinearSeq[A]](typeCheck(these, weakTypeOf[LinearSeq[A]]))
  }

  def collect[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
      (builder: c.Expr[Builder[B]])
    : c.Expr[builder.value.State] =
    new LinearSeqMacros[c.type](c).collect[A, B](unApply[A](c))(q)(builder)

  def map[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => B])
      (builder: c.Expr[Builder[B]])
    : c.Expr[builder.value.State] =
    new LinearSeqMacros[c.type](c).map[A, B](unApply[A](c))(f)(builder)

  def flatMap[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => Traverser[B]])
      (builder: c.Expr[Builder[B]])
    : c.Expr[builder.value.State] =
    new LinearSeqMacros[c.type](c).flatMap[A, B](unApply[A](c))(f)(builder)

  def filter[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new LinearSeqMacros[c.type](c).filter[A](unApply[A](c))(p)(builder)

  def dropWhile[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new LinearSeqMacros[c.type](c).dropWhile[A](unApply[A](c))(p)(builder)

  def takeWhile[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new LinearSeqMacros[c.type](c).takeWhile[A](unApply[A](c))(p)(builder)

  def span[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder1: c.Expr[Builder[A]], builder2: c.Expr[Builder[A]])
    : c.Expr[(builder1.value.State, builder2.value.State)] =
    new LinearSeqMacros[c.type](c).span[A](unApply[A](c))(p)(builder1, builder2)

  def drop[A : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new LinearSeqMacros[c.type](c).drop[A](unApply[A](c))(lower)(builder)

  def take[A : c.WeakTypeTag]
      (c: Context)
      (upper: c.Expr[Int])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new LinearSeqMacros[c.type](c).take[A](unApply[A](c))(upper)(builder)

  def slice[A : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int], upper: c.Expr[Int])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new LinearSeqMacros[c.type](c).slice[A](unApply[A](c))(lower, upper)(builder)

  def zip[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (those: c.Expr[LinearSeq[B]])
      (builder: c.Expr[Builder[(A, B)]])
    : c.Expr[builder.value.State] =
    new LinearSeqMacros[c.type](c).zip[A, B](unApply[A](c), those)(builder)
}
