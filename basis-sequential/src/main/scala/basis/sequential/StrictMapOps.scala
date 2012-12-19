/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential

import basis.collections._

/** Strictly evaluated map operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
final class StrictMapOps[+A, +T, +From](these: Map[A, T]) {
  /** Returns the applications of a partial function to each entry in this
    * map for which the function is defined.
    * 
    * @param  q         the partial function to filter and transform entries.
    * @param  builder   the implicit accumulator for collected elements.
    * @return the accumulated elements filtered and transformed by `q`.
    * @group  Mapping
    */
  def collect[B](q: PartialFunction[(A, T), B])(implicit builder: Builder[From, B]): builder.State =
    macro StrictContainerOps.collect[(A, T), B]
  
  /** Returns the applications of a function to each entry in this map.
    * 
    * @param  f         the function to apply to each entry.
    * @param  builder   the implicit accumulator for transformed entries.
    * @return the accumulated elements transformed by `f`.
    * @group  Mapping
    */
  def map[B](f: ((A, T)) => B)(implicit builder: Builder[From, B]): builder.State =
    macro StrictContainerOps.map[(A, T), B]
  
  /** Returns the concatenation of all elements returned by a function applied
    * to each entry in this map.
    * 
    * @param  f         the enumerator-yielding function to apply to each entry.
    * @param  builder   the implicit accumulator for flattened elements.
    * @return the concatenation of all accumulated elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: ((A, T)) => Enumerator[B])(implicit builder: Builder[From, B]): builder.State =
    macro StrictContainerOps.flatMap[(A, T), B]
  
  /** Returns all entries in this map that satisfy a predicate.
    * 
    * @param  p         the predicate to test entries against.
    * @param  builder   the implicit accumulator for filtered entries.
    * @return the accumulated entries filtered by `p`.
    * @group  Filtering
    */
  def filter(p: ((A, T)) => Boolean)(implicit builder: Builder[From, (A, T)]): builder.State =
    macro StrictContainerOps.filter[(A, T)]
  
  /** Returns a view of all entries in this map that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test entries against.
    * @return a non-strict view of the filtered entries.
    * @group  Filtering
    */
  def withFilter(p: ((A, T)) => Boolean): Map[A, T] =
    new NonStrictMapOps.Filter(these, p)
  
  /** Returns all entries following the longest prefix of this map
    * for which each entry satisfies a predicate.
    * 
    * @param  p         the predicate to test entries against.
    * @param  builder   the implicit accumulator for non-dropped entries.
    * @return the suffix of accumulated entries beginning with the first
    *         entry to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: ((A, T)) => Boolean)(implicit builder: Builder[From, (A, T)]): builder.State =
    macro StrictContainerOps.dropWhile[(A, T)]
  
  /** Returns the longest prefix of this map for which each entry
    * satisfies a predicate.
    * 
    * @param  p         the predicate to test entries against.
    * @param  builder   the implicit accumulator for taken entries.
    * @return the longest prefix of accumulated entries preceding the first
    *         entry to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: ((A, T)) => Boolean)(implicit builder: Builder[From, (A, T)]): builder.State =
    macro StrictContainerOps.takeWhile[(A, T)]
  
  /** Returns a (prefix, suffix) pair with the prefix being the longest one for
    * which each entry satisfies a predicate, and the suffix beginning with
    * the first entry to not satisfy the predicate.
    * 
    * @param  p           the predicate to test entries against.
    * @param  builder1    the implicit accumulator for prefix entries.
    * @param  builder2    the implicit accumilator for suffix entries.
    * @return the pair of accumulated prefix and suffix entries.
    * @group  Filtering
    */
  //FIXME: SI-6447
  //def span(p: ((A, T)) => Boolean)
  //    (implicit builder1: Builder[From, (A, T)], builder2: Builder[From, (A, T)])
  //  : (builder1.State, builder2.State) =
  //  macro StrictContainerOps.span[(A, T)]
  
  /** Returns all entries in this map following a prefix up to some length.
    * 
    * @param  lower     the length of the prefix to drop; also the inclusive
    *                   lower bound for indexes of entries to keep.
    * @param  builder   the implicit accumulator for non-dropped entries.
    * @return all but the first `lower` accumulated entries.
    * @group  Filtering
    */
  def drop(lower: Int)(implicit builder: Builder[From, (A, T)]): builder.State =
    macro StrictContainerOps.drop[(A, T)]
  
  /** Returns a prefix of this map up to some length.
    * 
    * @param  upper     the length of the prefix to take; also the exclusive
    *                   upper bound for indexes of entries to keep.
    * @param  builder   the implicit accumulator for taken entries.
    * @return up to the first `upper` accumulated entries.
    * @group  Filtering
    */
  def take(upper: Int)(implicit builder: Builder[From, (A, T)]): builder.State =
    macro StrictContainerOps.take[(A, T)]
  
  /** Returns an interval of entries in this map.
    * 
    * @param  lower     the inclusive lower bound for indexes of entries to keep.
    * @param  upper     the exclusive upper bound for indexes of entries to keep.
    * @param  builder   the implicit accumulator for kept entries.
    * @return the accumulated entries with indexes greater than or equal to
    *         `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int)(implicit builder: Builder[From, (A, T)]): builder.State =
    macro StrictContainerOps.slice[(A, T)]
  
  /** Returns the concatenation of this and another collection.
    * 
    * @param  those     the entries to append to these entries.
    * @param  builder   the implicit accumulator for concatenated entries.
    * @return the accumulated entries of both collections.
    * @group  Combining
    */
  def ++ [B >: (A, T)](those: Enumerator[B])(implicit builder: Builder[From, B]): builder.State =
    macro StrictEnumeratorOps.++[B]
}
