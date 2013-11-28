//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

final class StrictSetOps[+A, -Family](val __ : Set[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[B] with From[Family]): builder.State =
    macro StrictContainerOps.collect[A, B]

  def map[B](f: A => B)(implicit builder: Builder[B] with From[Family]): builder.State =
    macro StrictContainerOps.map[A, B]

  def flatMap[B](f: A => Traverser[B])(implicit builder: Builder[B] with From[Family]): builder.State =
    macro StrictContainerOps.flatMap[A, B]

  def filter(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State =
    macro StrictContainerOps.filter[A]

  def withFilter(p: A => Boolean): Set[A] =
    new NonStrictSetOps.Filter(__, p)

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

  def ++ [B >: A](those: Set[B])(implicit builder: Builder[B] with From[Family]): builder.State =
    macro StrictTraverserOps.++[B]
}
