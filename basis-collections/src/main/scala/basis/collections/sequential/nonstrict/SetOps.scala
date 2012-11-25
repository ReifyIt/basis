/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential
package nonstrict

import traversable._

/** Non-strictly evaluated set operations.
  * 
  * @groupprio  Traversing  -6
  * @groupprio  Reducing    -5
  * @groupprio  Querying    -4
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  * 
  * @define collection  set
  */
trait SetOps[+A, +From]
  extends Any
    with ContainerOps[A, From]
    with general.SetOps[A, From] {
  
  protected[this] override def self: Set[A]
  
  override def collect[B](q: PartialFunction[A, B]): Set[B] =
    new SetView.Collect(self, q)
  
  override def map[B](f: A => B): Set[B] =
    new SetView.Map(self, f)
  
  override def filter(p: A => Boolean): Set[A] =
    new SetView.Filter(self, p)
  
  override def dropWhile(p: A => Boolean): Set[A] =
    new SetView.DropWhile(self, p)
  
  override def takeWhile(p: A => Boolean): Set[A] =
    new SetView.TakeWhile(self, p)
  
  override def span(p: A => Boolean): (Set[A], Set[A]) =
    (takeWhile(p), dropWhile(p))
  
  override def drop(lower: Int): Set[A] =
    new SetView.Drop(self, lower)
  
  override def take(upper: Int): Set[A] =
    new SetView.Take(self, upper)
  
  override def slice(lower: Int, upper: Int): Set[A] =
    new SetView.Slice(self, lower, upper)
}

class SetView[+A, +From](override val self: Set[A])
  extends AnyVal with SetOps[A, From] {
  
  override def collect[B](q: PartialFunction[A, B]): Set[B] =
    new SetView.Collect(self, q)
  
  override def map[B](f: A => B): Set[B] =
    new SetView.Map(self, f)
  
  override def filter(p: A => Boolean): Set[A] =
    new SetView.Filter(self, p)
  
  override def dropWhile(p: A => Boolean): Set[A] =
    new SetView.DropWhile(self, p)
  
  override def takeWhile(p: A => Boolean): Set[A] =
    new SetView.TakeWhile(self, p)
  
  override def span(p: A => Boolean): (Set[A], Set[A]) =
    (takeWhile(p), dropWhile(p))
  
  override def drop(lower: Int): Set[A] =
    new SetView.Drop(self, lower)
  
  override def take(upper: Int): Set[A] =
    new SetView.Take(self, upper)
  
  override def slice(lower: Int, upper: Int): Set[A] =
    new SetView.Slice(self, lower, upper)
}

private[nonstrict] object SetView {
  import scala.annotation.unchecked.uncheckedVariance
  
  class Collect[-A, +B](
      protected[this] override val base: Set[A],
      protected[this] override val q: PartialFunction[A, B])
    extends ContainerView.Collect[A, B](base, q) with Set[B]
  
  class Map[-A, +B](
      protected[this] override val base: Set[A],
      protected[this] override val f: A => B)
    extends ContainerView.Map[A, B](base, f) with Set[B]
  
  class Filter[+A](
      protected[this] override val base: Set[A],
      protected[this] override val p: A => Boolean)
    extends ContainerView.Filter[A](base, p) with Set[A] {
    
    override def contains(element: A @uncheckedVariance): Boolean =
      (base contains element) && p(element)
  }
  
  class DropWhile[+A](
      protected[this] override val base: Set[A],
      protected[this] override val p: A => Boolean)
    extends ContainerView.DropWhile[A](base, p) with Set[A]
  
  class TakeWhile[+A](
      protected[this] override val base: Set[A],
      protected[this] override val p: A => Boolean)
    extends ContainerView.TakeWhile[A](base, p) with Set[A]
  
  class Drop[+A](
      protected[this] override val base: Set[A],
      protected[this] override val lower: Int)
    extends ContainerView.Drop[A](base, lower) with Set[A]
  
  class Take[+A](
      protected[this] override val base: Set[A],
      protected[this] override val upper: Int)
    extends ContainerView.Take[A](base, upper) with Set[A]
  
  class Slice[+A](
      protected[this] override val base: Set[A],
      protected[this] override val lower: Int,
      protected[this] override val upper: Int)
    extends ContainerView.Slice[A](base, lower, upper) with Set[A]
}
