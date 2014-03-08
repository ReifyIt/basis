//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package sequential

final class StrictSeqOps[+A, -Family](val __ : Seq[A]) extends AnyVal {
  def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[B] with From[Family]): builder.State = macro StrictContainerMacros.collect[A, B]
  def map[B](f: A => B)(implicit builder: Builder[B] with From[Family]): builder.State                    = macro StrictContainerMacros.map[A, B]
  def flatMap[B](f: A => Traverser[B])(implicit builder: Builder[B] with From[Family]): builder.State     = macro StrictContainerMacros.flatMap[A, B]
  def filter(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State              = macro StrictContainerMacros.filter[A]
  def withFilter(p: A => Boolean): Seq[A]                                                                 = new NonStrictSeqOps.Filter(__, p)
  def dropWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State           = macro StrictContainerMacros.dropWhile[A]
  def takeWhile(p: A => Boolean)(implicit builder: Builder[A] with From[Family]): builder.State           = macro StrictContainerMacros.takeWhile[A]
  def drop(lower: Int)(implicit builder: Builder[A] with From[Family]): builder.State                     = macro StrictContainerMacros.drop[A]
  def take(upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State                     = macro StrictContainerMacros.take[A]
  def slice(lower: Int, upper: Int)(implicit builder: Builder[A] with From[Family]): builder.State        = macro StrictContainerMacros.slice[A]
  def zip[B](those: Container[B])(implicit builder: Builder[(A, B)] with From[Family]): builder.State     = macro StrictContainerMacros.zip[A, B]
  def :+ (elem: A)(implicit builder: Builder[A] with From[Family]): builder.State                         = macro StrictTraverserMacros.:+[A]
  def +: (elem: A)(implicit builder: Builder[A] with From[Family]): builder.State                         = macro StrictTraverserMacros.+:[A]
  def ++ [B >: A](those: Seq[B])(implicit builder: Builder[B] with From[Family]): builder.State           = macro StrictTraverserMacros.++[B]

  def span(p: A => Boolean)(implicit builder1: Builder[A] with From[Family], builder2: Builder[A] with From[Family]): (builder1.State, builder2.State) = macro StrictContainerMacros.span[A]
}
