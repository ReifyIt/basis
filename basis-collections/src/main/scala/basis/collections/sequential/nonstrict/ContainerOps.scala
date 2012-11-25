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

/** Non-strictly evaluated container operations.
  * 
  * @groupprio  Traversing  -6
  * @groupprio  Reducing    -5
  * @groupprio  Querying    -4
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  * 
  * @define collection  container
  */
trait ContainerOps[+A, +From]
  extends Any
    with CollectionOps[A, From]
    with general.ContainerOps[A, From] {
  
  protected[this] override def self: Container[A]
  
  override def collect[B](q: PartialFunction[A, B]): Container[B] =
    new ContainerView.Collect(self, q)
  
  override def map[B](f: A => B): Container[B] =
    new ContainerView.Map(self, f)
  
  override def filter(p: A => Boolean): Container[A] =
    new ContainerView.Filter(self, p)
  
  override def dropWhile(p: A => Boolean): Container[A] =
    new ContainerView.DropWhile(self, p)
  
  override def takeWhile(p: A => Boolean): Container[A] =
    new ContainerView.TakeWhile(self, p)
  
  override def span(p: A => Boolean): (Container[A], Container[A]) =
    (takeWhile(p), dropWhile(p))
  
  override def drop(lower: Int): Container[A] =
    new ContainerView.Drop(self, lower)
  
  override def take(upper: Int): Container[A] =
    new ContainerView.Take(self, upper)
  
  override def slice(lower: Int, upper: Int): Container[A] =
    new ContainerView.Slice(self, lower, upper)
}

class ContainerView[+A, +From](override val self: Container[A])
  extends AnyVal with ContainerOps[A, From] {
  
  override def collect[B](q: PartialFunction[A, B]): Container[B] =
    new ContainerView.Collect(self, q)
  
  override def map[B](f: A => B): Container[B] =
    new ContainerView.Map(self, f)
  
  /** Returns a view concatenating all elements returned by a function
    * applied to each element in this $collection.
    * 
    * @param  f   the $collection-yielding function to apply to each element.
    * @return a non-strict view concatenating all elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => Container[B]): Container[B] =
    new ContainerView.FlatMap(self, f)
  
  override def filter(p: A => Boolean): Container[A] =
    new ContainerView.Filter(self, p)
  
  override def dropWhile(p: A => Boolean): Container[A] =
    new ContainerView.DropWhile(self, p)
  
  override def takeWhile(p: A => Boolean): Container[A] =
    new ContainerView.TakeWhile(self, p)
  
  override def span(p: A => Boolean): (Container[A], Container[A]) =
    (takeWhile(p), dropWhile(p))
  
  override def drop(lower: Int): Container[A] =
    new ContainerView.Drop(self, lower)
  
  override def take(upper: Int): Container[A] =
    new ContainerView.Take(self, upper)
  
  override def slice(lower: Int, upper: Int): Container[A] =
    new ContainerView.Slice(self, lower, upper)
  
  /** Returns a view of pairs of elemnts from this and another $collection.
    * 
    * @param  those   the $collection whose elements to lazily pair with these elements.
    * @return a non-strict view of the pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B](those: Container[B]): Container[(A, B)] =
    new ContainerView.Zip(self, those)
  
  /** Returns a view concatenating this and another $collection.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: Container[B]): Container[B] =
    new ContainerView.++(self, those)
}

private[nonstrict] object ContainerView {
  class Collect[-A, +B](
      protected[this] override val base: Container[A],
      protected[this] override val q: PartialFunction[A, B])
    extends CollectionView.Collect[A, B](base, q) with Container[B] {
    
    override def iterator: Iterator[B] =
      new IteratorView.Collect(base.iterator, q)
  }
  
  class Map[-A, +B](
      protected[this] override val base: Container[A],
      protected[this] override val f: A => B)
    extends CollectionView.Map[A, B](base, f) with Container[B] {
    
    override def iterator: Iterator[B] =
      new IteratorView.Map(base.iterator, f)
  }
  
  class FlatMap[-A, +B](
      protected[this] override val base: Container[A],
      protected[this] override val f: A => Container[B])
    extends CollectionView.FlatMap[A, B](base, f) with Container[B] {
    
    override def iterator: Iterator[B] =
      new IteratorView.FlatMapContainer(base.iterator, f)
  }
  
  class Filter[+A](
      protected[this] override val base: Container[A],
      protected[this] override val p: A => Boolean)
    extends CollectionView.Filter[A](base, p) with Container[A] {
    
    override def iterator: Iterator[A] =
      new IteratorView.Filter(base.iterator, p)
  }
  
  class DropWhile[+A](
      protected[this] override val base: Container[A],
      protected[this] override val p: A => Boolean)
    extends CollectionView.DropWhile[A](base, p) with Container[A] {
    
    override def iterator: Iterator[A] =
      new IteratorView.DropWhile(base.iterator, p)
  }
  
  class TakeWhile[+A](
      protected[this] override val base: Container[A],
      protected[this] override val p: A => Boolean)
    extends CollectionView.TakeWhile[A](base, p) with Container[A] {
    
    override def iterator: Iterator[A] =
      new IteratorView.TakeWhile(base.iterator, p)
  }
  
  class Drop[+A](
      protected[this] override val base: Container[A],
      protected[this] override val lower: Int)
    extends CollectionView.Drop[A](base, lower) with Container[A] {
    
    override def iterator: Iterator[A] =
      new IteratorView.Drop(base.iterator, lower)
  }
  
  class Take[+A](
      protected[this] override val base: Container[A],
      protected[this] override val upper: Int)
    extends CollectionView.Take[A](base, upper) with Container[A] {
    
    override def iterator: Iterator[A] =
      new IteratorView.Take(base.iterator, upper)
  }
  
  class Slice[+A](
      protected[this] override val base: Container[A],
      protected[this] override val lower: Int,
      protected[this] override val upper: Int)
    extends CollectionView.Slice[A](base, lower, upper) with Container[A] {
    
    override def iterator: Iterator[A] =
      new IteratorView.Slice(base.iterator, lower, upper)
  }
  
  class Zip[+A, +B](
      protected[this] val xs: Container[A],
      protected[this] val ys: Container[B])
    extends Container[(A, B)] {
    
    override def iterator: Iterator[(A, B)] =
      new IteratorView.Zip(xs.iterator, ys.iterator)
  }
  
  class ++[+A](
      protected[this] override val xs: Container[A],
      protected[this] override val ys: Container[A])
    extends CollectionView.++[A](xs, ys) with Container[A] {
    
    override def iterator: Iterator[A] =
      new IteratorView.++(xs.iterator, ys.iterator)
  }
}
