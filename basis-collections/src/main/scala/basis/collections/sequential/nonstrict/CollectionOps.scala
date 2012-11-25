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

/** Non-strictly evaluated collection operations.
  * 
  * @groupprio  Traversing  -6
  * @groupprio  Reducing    -5
  * @groupprio  Querying    -4
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  * 
  * @define collection  collection
  */
trait CollectionOps[+A, +From]
  extends Any
    with EnumeratorOps[A, From]
    with general.CollectionOps[A, From] {
  
  protected[this] override def self: Collection[A]
  
  override def collect[B](q: PartialFunction[A, B]): Collection[B] =
    new CollectionView.Collect(self, q)
  
  override def map[B](f: A => B): Collection[B] =
    new CollectionView.Map(self, f)
  
  override def filter(p: A => Boolean): Collection[A] =
    new CollectionView.Filter(self, p)
  
  override def dropWhile(p: A => Boolean): Collection[A] =
    new CollectionView.DropWhile(self, p)
  
  override def takeWhile(p: A => Boolean): Collection[A] =
    new CollectionView.TakeWhile(self, p)
  
  override def span(p: A => Boolean): (Collection[A], Collection[A]) =
    (takeWhile(p), dropWhile(p))
  
  override def drop(lower: Int): Collection[A] =
    new CollectionView.Drop(self, lower)
  
  override def take(upper: Int): Collection[A] =
    new CollectionView.Take(self, upper)
  
  override def slice(lower: Int, upper: Int): Collection[A] =
    new CollectionView.Slice(self, lower, upper)
}

class CollectionView[+A, +From](override val self: Collection[A])
  extends AnyVal with CollectionOps[A, From] {
  
  override def collect[B](q: PartialFunction[A, B]): Collection[B] =
    new CollectionView.Collect(self, q)
  
  override def map[B](f: A => B): Collection[B] =
    new CollectionView.Map(self, f)
  
  /** Returns a view concatenating all elements returned by a function
    * applied to each element in this $collection.
    * 
    * @param  f   the $collection-yielding function to apply to each element.
    * @return a non-strict view concatenating all elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => Collection[B]): Collection[B] =
    new CollectionView.FlatMap(self, f)
  
  override def filter(p: A => Boolean): Collection[A] =
    new CollectionView.Filter(self, p)
  
  override def dropWhile(p: A => Boolean): Collection[A] =
    new CollectionView.DropWhile(self, p)
  
  override def takeWhile(p: A => Boolean): Collection[A] =
    new CollectionView.TakeWhile(self, p)
  
  override def span(p: A => Boolean): (Collection[A], Collection[A]) =
    (takeWhile(p), dropWhile(p))
  
  override def drop(lower: Int): Collection[A] =
    new CollectionView.Drop(self, lower)
  
  override def take(upper: Int): Collection[A] =
    new CollectionView.Take(self, upper)
  
  override def slice(lower: Int, upper: Int): Collection[A] =
    new CollectionView.Slice(self, lower, upper)
  
  /** Returns a view concatenating this and another $collection.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: Collection[B]): Collection[B] =
    new CollectionView.++(self, those)
}

private[nonstrict] object CollectionView {
  class Collect[-A, +B](base: Collection[A], q: PartialFunction[A, B])
    extends EnumeratorView.Collect[A, B](base, q) with Collection[B]
  
  class Map[-A, +B](base: Collection[A], f: A => B)
    extends EnumeratorView.Map[A, B](base, f) with Collection[B]
  
  class FlatMap[-A, +B](base: Collection[A], f: A => Collection[B])
    extends EnumeratorView.FlatMap[A, B](base, f) with Collection[B]
  
  class Filter[+A](base: Collection[A], p: A => Boolean)
    extends EnumeratorView.Filter[A](base, p) with Collection[A]
  
  class DropWhile[+A](base: Collection[A], p: A => Boolean)
    extends EnumeratorView.DropWhile[A](base, p) with Collection[A]
  
  class TakeWhile[+A](base: Collection[A], p: A => Boolean)
    extends EnumeratorView.TakeWhile[A](base, p) with Collection[A]
  
  class Drop[+A](base: Collection[A], lower: Int)
    extends EnumeratorView.Drop[A](base, lower) with Collection[A]
  
  class Take[+A](base: Collection[A], upper: Int)
    extends EnumeratorView.Take[A](base, upper) with Collection[A]
  
  class Slice[+A](base: Collection[A], lower: Int, upper: Int)
    extends EnumeratorView.Slice[A](base, lower, upper) with Collection[A]
  
  class ++[+A](xs: Collection[A], ys: Collection[A])
    extends EnumeratorView.++[A](xs, ys) with Collection[A]
}
