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

/** Non-strictly evaluated sequence operations.
  * 
  * @groupprio  Traversing  -6
  * @groupprio  Reducing    -5
  * @groupprio  Querying    -4
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  * 
  * @define collection  sequence
  */
trait SeqOps[+A, +From]
  extends Any
    with ContainerOps[A, From]
    with general.SeqOps[A, From] {
  
  protected[this] override def self: Seq[A]
  
  override def collect[B](q: PartialFunction[A, B]): Seq[B] =
    new SeqView.Collect(self, q)
  
  override def map[B](f: A => B): Seq[B] =
    new SeqView.Map(self, f)
  
  override def filter(p: A => Boolean): Seq[A] =
    new SeqView.Filter(self, p)
  
  override def dropWhile(p: A => Boolean): Seq[A] =
    new SeqView.DropWhile(self, p)
  
  override def takeWhile(p: A => Boolean): Seq[A] =
    new SeqView.TakeWhile(self, p)
  
  override def span(p: A => Boolean): (Seq[A], Seq[A]) =
    (takeWhile(p), dropWhile(p))
  
  override def drop(lower: Int): Seq[A] =
    new SeqView.Drop(self, lower)
  
  override def take(upper: Int): Seq[A] =
    new SeqView.Take(self, upper)
  
  override def slice(lower: Int, upper: Int): Seq[A] =
    new SeqView.Slice(self, lower, upper)
}

class SeqView[+A, +From](override val self: Seq[A])
  extends AnyVal with SeqOps[A, From] {
  
  override def collect[B](q: PartialFunction[A, B]): Seq[B] =
    new SeqView.Collect(self, q)
  
  override def map[B](f: A => B): Seq[B] =
    new SeqView.Map(self, f)
  
  /** Returns a view concatenating all elements returned by a function
    * applied to each element in this $collection.
    * 
    * @param  f   the $collection-yielding function to apply to each element.
    * @return a non-strict view concatenating all elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => Seq[B]): Seq[B] =
    new SeqView.FlatMap(self, f)
  
  override def filter(p: A => Boolean): Seq[A] =
    new SeqView.Filter(self, p)
  
  override def dropWhile(p: A => Boolean): Seq[A] =
    new SeqView.DropWhile(self, p)
  
  override def takeWhile(p: A => Boolean): Seq[A] =
    new SeqView.TakeWhile(self, p)
  
  override def span(p: A => Boolean): (Seq[A], Seq[A]) =
    (takeWhile(p), dropWhile(p))
  
  override def drop(lower: Int): Seq[A] =
    new SeqView.Drop(self, lower)
  
  override def take(upper: Int): Seq[A] =
    new SeqView.Take(self, upper)
  
  override def slice(lower: Int, upper: Int): Seq[A] =
    new SeqView.Slice(self, lower, upper)
  
  /** Returns a view of pairs of elemnts from this and another $collection.
    * 
    * @param  those   the $collection whose elements to lazily pair with these elements.
    * @return a non-strict view of the pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B](those: Seq[B]): Seq[(A, B)] =
    new SeqView.Zip(self, those)
  
  /** Returns a view concatenating this and another $collection.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: Seq[B]): Seq[B] =
    new SeqView.++(self, those)
}

private[nonstrict] object SeqView {
  class Collect[-A, +B](
      protected[this] override val base: Seq[A],
      protected[this] override val q: PartialFunction[A, B])
    extends ContainerView.Collect[A, B](base, q) with Seq[B]
  
  class Map[-A, +B](
      protected[this] override val base: Seq[A],
      protected[this] override val f: A => B)
    extends ContainerView.Map[A, B](base, f) with Seq[B]
  
  class FlatMap[-A, +B](
      protected[this] override val base: Seq[A],
      protected[this] override val f: A => Seq[B])
    extends ContainerView.FlatMap[A, B](base, f) with Seq[B]
  
  class Filter[+A](
      protected[this] override val base: Seq[A],
      protected[this] override val p: A => Boolean)
    extends ContainerView.Filter[A](base, p) with Seq[A]
  
  class DropWhile[+A](
      protected[this] override val base: Seq[A],
      protected[this] override val p: A => Boolean)
    extends ContainerView.DropWhile[A](base, p) with Seq[A]
  
  class TakeWhile[+A](
      protected[this] override val base: Seq[A],
      protected[this] override val p: A => Boolean)
    extends ContainerView.TakeWhile[A](base, p) with Seq[A]
  
  class Drop[+A](
      protected[this] override val base: Seq[A],
      protected[this] override val lower: Int)
    extends ContainerView.Drop[A](base, lower) with Seq[A]
  
  class Take[+A](
      protected[this] override val base: Seq[A],
      protected[this] override val upper: Int)
    extends ContainerView.Take[A](base, upper) with Seq[A]
  
  class Slice[+A](
      protected[this] override val base: Seq[A],
      protected[this] override val lower: Int,
      protected[this] override val upper: Int)
    extends ContainerView.Slice[A](base, lower, upper) with Seq[A]
  
  class Zip[+A, +B](
      protected[this] override val xs: Seq[A],
      protected[this] override val ys: Seq[B])
    extends ContainerView.Zip[A, B](xs, ys) with Seq[(A, B)]
  
  class ++[+A](
      protected[this] override val xs: Seq[A],
      protected[this] override val ys: Seq[A])
    extends ContainerView.++[A](xs, ys) with Seq[A]
}
