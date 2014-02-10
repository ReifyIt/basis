//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

final class StrictArrayOps[A, -Family](val __ : Array[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[B] with From[Family]): builder.State =
    macro StrictArrayOps.collect[A, B]

  def map[B](f: A => B)(implicit builder: Builder[B] with From[Family]): builder.State =
    macro StrictArrayOps.map[A, B]

  def flatMap[B](f: A => Traverser[B])(implicit builder: Builder[B] with From[Family]): builder.State =
    macro StrictArrayOps.flatMap[A, B]

  def filter(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictArrayOps.filter[A]

  def withFilter(p: A => Boolean): IndexedSeq[A] =
    new NonStrictArrayOps.Filter(__, p)

  def dropWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictArrayOps.dropWhile[A]

  def takeWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictArrayOps.takeWhile[A]

  //FIXME: SI-6447
  //def span(p: A => Boolean)
  //    (implicit builder1: Builder[A] with From[Family], builder2: Builder[A] with From[Family])
  //  : (builder1.State, builder2.State) =
  //  macro StrictArrayOps.span[A]

  def drop(lower: Int)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictArrayOps.drop[A]

  def take(upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictArrayOps.take[A]

  def slice(lower: Int, upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictArrayOps.slice[A]

  def reverse(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictArrayOps.reverse[A]

  def zip[B](those: Array[B])(implicit builder: Builder[(A, B)] with From[Family]): builder.State =
    macro StrictArrayOps.zip[A, B]

  def :+ (elem: A)(implicit builder: ArrayBuilder[A] with From[Family]): builder.State =
    macro StrictArrayOps.:+[A]

  def +: (elem: A)(implicit builder: ArrayBuilder[A] with From[Family]): builder.State =
    macro StrictArrayOps.+:[A]

  def ++ (those: Array[A])(implicit builder: ArrayBuilder[A] with From[Family]): builder.State =
    macro StrictArrayOps.++[A]
}

private[sequential] object StrictArrayOps {
  import scala.collection.immutable.{ ::, Nil }
  import scala.reflect.macros.Context

  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Array[A]] = {
    import c.{ Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag }
    import c.universe._
    val Apply(_, these :: Nil) = prefix.tree
    implicit val ArrayATag =
      WeakTypeTag[Array[A]](
        appliedType(
          mirror.staticClass("scala.Array").toType,
          weakTypeOf[A] :: Nil))
    Expr[Array[A]](typeCheck(these, weakTypeOf[Array[A]]))
  }

  def collect[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
      (builder: c.Expr[Builder[B]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).collect[A, B](unApply[A](c))(q)(builder)

  def map[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => B])
      (builder: c.Expr[Builder[B]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).map[A, B](unApply[A](c))(f)(builder)

  def flatMap[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => Traverser[B]])
      (builder: c.Expr[Builder[B]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).flatMap[A, B](unApply[A](c))(f)(builder)

  def filter[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).filter[A](unApply[A](c))(p)(builder)

  def dropWhile[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).dropWhile[A](unApply[A](c))(p)(builder)

  def takeWhile[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).takeWhile[A](unApply[A](c))(p)(builder)

  def span[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder1: c.Expr[Builder[A]], builder2: c.Expr[Builder[A]])
    : c.Expr[(builder1.value.State, builder2.value.State)] =
    new ArrayMacros[c.type](c).span[A](unApply[A](c))(p)(builder1, builder2)

  def drop[A : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).drop[A](unApply[A](c))(lower)(builder)

  def take[A : c.WeakTypeTag]
      (c: Context)
      (upper: c.Expr[Int])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).take[A](unApply[A](c))(upper)(builder)

  def slice[A : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int], upper: c.Expr[Int])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).slice[A](unApply[A](c))(lower, upper)(builder)

  def reverse[A : c.WeakTypeTag]
      (c: Context)
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).reverse[A](unApply[A](c))(builder)

  def zip[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (those: c.Expr[Array[B]])
      (builder: c.Expr[Builder[(A, B)]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).zip[A, B](unApply[A](c), those)(builder)

  def :+ [A : c.WeakTypeTag]
      (c: Context)
      (elem: c.Expr[A])
      (builder: c.Expr[ArrayBuilder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).:+[A](unApply[A](c), elem)(builder)

  def +: [A : c.WeakTypeTag]
      (c: Context)
      (elem: c.Expr[A])
      (builder: c.Expr[ArrayBuilder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).+:[A](elem, unApply[A](c))(builder)

  def ++ [A : c.WeakTypeTag]
      (c: Context)
      (those: c.Expr[Array[A]])
      (builder: c.Expr[ArrayBuilder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).++[A](unApply[A](c), those)(builder)
}
