/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential
package nonstrict

import basis.collections._
import basis.collections.traversable._

/** Non-strictly evaluated collection operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
final class CollectionOps[+A](val these: Collection[A]) extends AnyVal {
  /** Returns a view that applies a partial function to each element in this
    * collection for which the function is defined.
    * 
    * @param  q   the partial function to lazily filter and map elements.
    * @return a non-strict view of the filtered and mapped elements.
    * @group  Mapping
    */
  def collect[B](q: PartialFunction[A, B]): Collection[B] =
    new CollectionOps.Collect(these, q)
  
  /** Returns a view that applies a function to each element in this collection.
    * 
    * @param  f   the function to lazily apply to each element.
    * @return a non-strict view of the mapped elements.
    * @group  Mapping
    */
  def map[B](f: A => B): Collection[B] =
    new CollectionOps.Map(these, f)
  
  /** Returns a view concatenating all elements returned by a function
    * applied to each element in this collection.
    * 
    * @param  f   the collection-yielding function to apply to each element.
    * @return a non-strict view concatenating all elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => Collection[B]): Collection[B] =
    new CollectionOps.FlatMap(these, f)
  
  /** Returns a view of all elements in this collection that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def filter(p: A => Boolean): Collection[A] =
    new CollectionOps.Filter(these, p)
  
  /** Returns a view of all elements following the longest prefix of this
    * collection for which each element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the suffix of accumulated elements beginning
    *         with the first element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: A => Boolean): Collection[A] =
    new CollectionOps.DropWhile(these, p)
  
  /** Returns a view of the longest prefix of this collection for which each
    * element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the longest prefix of elements preceding
    *         the first element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: A => Boolean): Collection[A] =
    new CollectionOps.TakeWhile(these, p)
  
  /** Returns a (prefix, suffix) pair of views with the prefix being the
    * longest one for which each element satisfies a predicate, and the suffix
    * beginning with the first element to not satisfy the predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the (predix, suffix) pair of non-strict views.
    * @group  Filtering
    */
  def span(p: A => Boolean): (Collection[A], Collection[A]) =
    (takeWhile(p), dropWhile(p))
  
  /** Returns a view of all elements in this collection following a prefix
    * up to some length.
    * 
    * @param  lower   the length of the prefix to drop; also the inclusive
    *                 lower bound for indexes of included elements.
    * @return a non-strict view of all but the first `lower` elements.
    * @group  Filtering
    */
  def drop(lower: Int): Collection[A] =
    new CollectionOps.Drop(these, lower)
  
  /** Returns a view of a prefix of this collection up to some length.
    * 
    * @param  upper   the length of the prefix to take; also the exclusive
    *                 upper bound for indexes of included elements.
    * @return a non-strict view of up to the first `upper` elements.
    * @group  Filtering
    */
  def take(upper: Int): Collection[A] =
    new CollectionOps.Take(these, upper)
  
  /** Returns a view of an interval of elements in this collection.
    * 
    * @param  lower   the inclusive lower bound for indexes of included elements.
    * @param  upper   the exclusive upper bound for indexes of included elements.
    * @return a non-strict view of the elements with indexes greater than or
    *         equal to `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int): Collection[A] =
    new CollectionOps.Slice(these, lower, upper)
  
  /** Returns a view concatenating this and another collection.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: Collection[B]): Collection[B] =
    new CollectionOps.++(these, those)
}

private[nonstrict] object CollectionOps {
  class Collect[-A, +B](base: Collection[A], q: PartialFunction[A, B])
    extends EnumeratorOps.Collect[A, B](base, q) with Collection[B]
  
  class Map[-A, +B](base: Collection[A], f: A => B)
    extends EnumeratorOps.Map[A, B](base, f) with Collection[B]
  
  class FlatMap[-A, +B](base: Collection[A], f: A => Collection[B])
    extends EnumeratorOps.FlatMap[A, B](base, f) with Collection[B]
  
  class Filter[+A](base: Collection[A], p: A => Boolean)
    extends EnumeratorOps.Filter[A](base, p) with Collection[A]
  
  class DropWhile[+A](base: Collection[A], p: A => Boolean)
    extends EnumeratorOps.DropWhile[A](base, p) with Collection[A]
  
  class TakeWhile[+A](base: Collection[A], p: A => Boolean)
    extends EnumeratorOps.TakeWhile[A](base, p) with Collection[A]
  
  class Drop[+A](base: Collection[A], lower: Int)
    extends EnumeratorOps.Drop[A](base, lower) with Collection[A]
  
  class Take[+A](base: Collection[A], upper: Int)
    extends EnumeratorOps.Take[A](base, upper) with Collection[A]
  
  class Slice[+A](base: Collection[A], lower: Int, upper: Int)
    extends EnumeratorOps.Slice[A](base, lower, upper) with Collection[A]
  
  class ++[+A](xs: Collection[A], ys: Collection[A])
    extends EnumeratorOps.++[A](xs, ys) with Collection[A]
}
