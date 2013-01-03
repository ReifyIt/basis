/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

/** Strictly evaluated collection operations.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * @group    Strict
  * 
  * @groupprio  Mapping     1
  * @groupprio  Filtering   2
  * @groupprio  Combining   3
  */
final class StrictCollectionOps[+A, +From](val these: Collection[A]) extends AnyVal {
  /** Returns the applications of a partial function to each element in this
    * collection for which the function is defined.
    * 
    * @param  q         the partial function to filter and transform elements.
    * @param  builder   the implicit accumulator for collected elements.
    * @return the accumulated elements filtered and transformed by `q`.
    * @group  Mapping
    */
  def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[From, B]): builder.State =
    new StrictEnumeratorOps[A, From](these).collect[B](q)(builder)
  
  /** Returns the applications of a function to each element in this collection.
    * 
    * @param  f         the function to apply to each element.
    * @param  builder   the implicit accumulator for transformed elements.
    * @return the accumulated elements transformed by `f`.
    * @group  Mapping
    */
  def map[B](f: A => B)(implicit builder: Builder[From, B]): builder.State =
    new StrictEnumeratorOps[A, From](these).map[B](f)(builder)
  
  /** Returns the concatenation of all elements returned by a function applied
    * to each element in this collection.
    * 
    * @param  f         the enumerator-yielding function to apply to each element.
    * @param  builder   the implicit accumulator for flattened elements.
    * @return the concatenation of all accumulated elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => Enumerator[B])(implicit builder: Builder[From, B]): builder.State =
    new StrictEnumeratorOps[A, From](these).flatMap[B](f)(builder)
  
  /** Returns all elements in this collection that satisfy a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for filtered elements.
    * @return the accumulated elements filtered by `p`.
    * @group  Filtering
    */
  def filter(p: A => Boolean)(implicit builder: Builder[From, A]): builder.State =
    new StrictEnumeratorOps[A, From](these).filter(p)(builder)
  
  /** Returns a view of all elements in this collection that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def withFilter(p: A => Boolean): Collection[A] =
    new NonStrictCollectionOps.Filter(these, p)
  
  /** Returns all elements following the longest prefix of this collection
    * for which each element satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for non-dropped elements.
    * @return the suffix of accumulated elements beginning with the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: A => Boolean)(implicit builder: Builder[From, A]): builder.State =
    new StrictEnumeratorOps[A, From](these).dropWhile(p)(builder)
  
  /** Returns the longest prefix of this collection for which each element
    * satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for taken elements.
    * @return the longest prefix of accumulated elements preceding the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: A => Boolean)(implicit builder: Builder[From, A]): builder.State =
    new StrictEnumeratorOps[A, From](these).takeWhile(p)(builder)
  
  /** Returns a (prefix, suffix) pair with the prefix being the longest one for
    * which each element satisfies a predicate, and the suffix beginning with
    * the first element to not satisfy the predicate.
    * 
    * @param  p           the predicate to test elements against.
    * @param  builder1    the implicit accumulator for prefix elements.
    * @param  builder2    the implicit accumilator for suffix elements.
    * @return the pair of accumulated prefix and suffix elements.
    * @group  Filtering
    */
  def span(p: A => Boolean)
      (implicit builder1: Builder[From, A], builder2: Builder[From, A])
    : (builder1.State, builder2.State) =
    new StrictEnumeratorOps[A, From](these).span(p)(builder1, builder2)
  
  /** Returns all elements in this collection following a prefix up to some length.
    * 
    * @param  lower     the length of the prefix to drop; also the inclusive
    *                   lower bound for indexes of elements to keep.
    * @param  builder   the implicit accumulator for non-dropped elements.
    * @return all but the first `lower` accumulated elements.
    * @group  Filtering
    */
  def drop(lower: Int)(implicit builder: Builder[From, A]): builder.State =
    new StrictEnumeratorOps[A, From](these).drop(lower)(builder)
  
  /** Returns a prefix of this collection up to some length.
    * 
    * @param  upper     the length of the prefix to take; also the exclusive
    *                   upper bound for indexes of elements to keep.
    * @param  builder   the implicit accumulator for taken elements.
    * @return up to the first `upper` accumulated elements.
    * @group  Filtering
    */
  def take(upper: Int)(implicit builder: Builder[From, A]): builder.State =
    new StrictEnumeratorOps[A, From](these).take(upper)(builder)
  
  /** Returns an interval of elements in this collection.
    * 
    * @param  lower     the inclusive lower bound for indexes of elements to keep.
    * @param  upper     the exclusive upper bound for indexes of elements to keep.
    * @param  builder   the implicit accumulator for kept elements.
    * @return the accumulated elements with indexes greater than or equal to
    *         `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int)(implicit builder: Builder[From, A]): builder.State =
    new StrictEnumeratorOps[A, From](these).slice(lower, upper)(builder)
  
  /** Returns the concatenation of this and another collection.
    * 
    * @param  those     the elements to append to these elements.
    * @param  builder   the implicit accumulator for concatenated elements.
    * @return the accumulated elements of both collections.
    * @group  Combining
    */
  def ++ [B >: A](those: Enumerator[B])(implicit builder: Builder[From, B]): builder.State =
    new StrictEnumeratorOps[B, From](these).++[B](those)(builder)
  //new StrictEnumeratorOps[A, From](these).++[B](those)(builder) // SI-6482
}
