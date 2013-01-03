/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

/** Non-strictly evaluated collection operations.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * @group    NonStrict
  * 
  * @groupprio  Mapping     1
  * @groupprio  Filtering   2
  * @groupprio  Combining   3
  */
final class NonStrictCollectionOps[+A](val these: Collection[A]) extends AnyVal {
  /** Returns a view that applies a partial function to each element in this
    * collection for which the function is defined.
    * 
    * @param  q   the partial function to lazily filter and map elements.
    * @return a non-strict view of the filtered and mapped elements.
    * @group  Mapping
    */
  def collect[B](q: PartialFunction[A, B]): Collection[B] =
    new NonStrictCollectionOps.Collect(these, q)
  
  /** Returns a view that applies a function to each element in this collection.
    * 
    * @param  f   the function to lazily apply to each element.
    * @return a non-strict view of the mapped elements.
    * @group  Mapping
    */
  def map[B](f: A => B): Collection[B] =
    new NonStrictCollectionOps.Map(these, f)
  
  /** Returns a view concatenating all elements returned by a function
    * applied to each element in this collection.
    * 
    * @param  f   the collection-yielding function to apply to each element.
    * @return a non-strict view concatenating all elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => Collection[B]): Collection[B] =
    new NonStrictCollectionOps.FlatMap(these, f)
  
  /** Returns a view of all elements in this collection that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def filter(p: A => Boolean): Collection[A] =
    new NonStrictCollectionOps.Filter(these, p)
  
  /** Returns a view of all elements in this collection that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def withFilter(p: A => Boolean): Collection[A] =
    new NonStrictCollectionOps.Filter(these, p)
  
  /** Returns a view of all elements following the longest prefix of this
    * collection for which each element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the suffix of accumulated elements beginning
    *         with the first element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: A => Boolean): Collection[A] =
    new NonStrictCollectionOps.DropWhile(these, p)
  
  /** Returns a view of the longest prefix of this collection for which each
    * element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the longest prefix of elements preceding
    *         the first element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: A => Boolean): Collection[A] =
    new NonStrictCollectionOps.TakeWhile(these, p)
  
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
    new NonStrictCollectionOps.Drop(these, lower)
  
  /** Returns a view of a prefix of this collection up to some length.
    * 
    * @param  upper   the length of the prefix to take; also the exclusive
    *                 upper bound for indexes of included elements.
    * @return a non-strict view of up to the first `upper` elements.
    * @group  Filtering
    */
  def take(upper: Int): Collection[A] =
    new NonStrictCollectionOps.Take(these, upper)
  
  /** Returns a view of an interval of elements in this collection.
    * 
    * @param  lower   the inclusive lower bound for indexes of included elements.
    * @param  upper   the exclusive upper bound for indexes of included elements.
    * @return a non-strict view of the elements with indexes greater than or
    *         equal to `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int): Collection[A] =
    new NonStrictCollectionOps.Slice(these, lower, upper)
  
  /** Returns a view concatenating this and another collection.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: Collection[B]): Collection[B] =
    new NonStrictCollectionOps.++(these, those)
}

private[sequential] object NonStrictCollectionOps {
  class Collect[-A, +B](these: Collection[A], q: PartialFunction[A, B])
    extends NonStrictEnumeratorOps.Collect[A, B](these, q) with Collection[B]
  
  class Map[-A, +B](these: Collection[A], f: A => B)
    extends NonStrictEnumeratorOps.Map[A, B](these, f) with Collection[B]
  
  class FlatMap[-A, +B](these: Collection[A], f: A => Collection[B])
    extends NonStrictEnumeratorOps.FlatMap[A, B](these, f) with Collection[B]
  
  class Filter[+A](these: Collection[A], p: A => Boolean)
    extends NonStrictEnumeratorOps.Filter[A](these, p) with Collection[A]
  
  class DropWhile[+A](these: Collection[A], p: A => Boolean)
    extends NonStrictEnumeratorOps.DropWhile[A](these, p) with Collection[A]
  
  class TakeWhile[+A](these: Collection[A], p: A => Boolean)
    extends NonStrictEnumeratorOps.TakeWhile[A](these, p) with Collection[A]
  
  class Drop[+A](these: Collection[A], lower: Int)
    extends NonStrictEnumeratorOps.Drop[A](these, lower) with Collection[A]
  
  class Take[+A](these: Collection[A], upper: Int)
    extends NonStrictEnumeratorOps.Take[A](these, upper) with Collection[A]
  
  class Slice[+A](these: Collection[A], lower: Int, upper: Int)
    extends NonStrictEnumeratorOps.Slice[A](these, lower, upper) with Collection[A]
  
  class ++[+A](these: Collection[A], those: Collection[A])
    extends NonStrictEnumeratorOps.++[A](these, those) with Collection[A]
}
