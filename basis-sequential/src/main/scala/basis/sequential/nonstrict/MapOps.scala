/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential
package nonstrict

import basis.collections._

/** Non-strictly evaluated map operations.
  * 
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
final class MapOps[+A, +T](val these: Map[A, T]) extends AnyVal {
  /** Returns a view of all entries in this map that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test entries against.
    * @return a non-strict view of the filtered entries.
    * @group  Filtering
    */
  def filter(p: ((A, T)) => Boolean): Map[A, T] =
    new MapOps.Filter(these, p)
  
  /** Returns a view of all entries following the longest prefix of this
    * map for which each entry satisfies a predicate.
    * 
    * @param  p   the predicate to test entries against.
    * @return a non-strict view of the suffix of accumulated entries beginning
    *         with the first entry to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: ((A, T)) => Boolean): Map[A, T] =
    new MapOps.DropWhile(these, p)
  
  /** Returns a view of the longest prefix of this map for which each
    * entry satisfies a predicate.
    * 
    * @param  p   the predicate to test entries against.
    * @return a non-strict view of the longest prefix of entries preceding
    *         the first entry to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: ((A, T)) => Boolean): Map[A, T] =
    new MapOps.TakeWhile(these, p)
  
  /** Returns a (prefix, suffix) pair of views with the prefix being the
    * longest one for which each entry satisfies a predicate, and the suffix
    * beginning with the first entry to not satisfy the predicate.
    * 
    * @param  p   the predicate to test entries against.
    * @return the (predix, suffix) pair of non-strict views.
    * @group  Filtering
    */
  def span(p: ((A, T)) => Boolean): (Map[A, T], Map[A, T]) =
    (takeWhile(p), dropWhile(p))
  
  /** Returns a view of all entries in this map following a prefix
    * up to some length.
    * 
    * @param  lower   the length of the prefix to drop; also the inclusive
    *                 lower bound for indexes of included entries.
    * @return a non-strict view of all but the first `lower` entries.
    * @group  Filtering
    */
  def drop(lower: Int): Map[A, T] =
    new MapOps.Drop(these, lower)
  
  /** Returns a view of a prefix of this map up to some length.
    * 
    * @param  upper   the length of the prefix to take; also the exclusive
    *                 upper bound for indexes of included entries.
    * @return a non-strict view of up to the first `upper` entries.
    * @group  Filtering
    */
  def take(upper: Int): Map[A, T] =
    new MapOps.Take(these, upper)
  
  /** Returns a view of an interval of entries in this map.
    * 
    * @param  lower   the inclusive lower bound for indexes of included entries.
    * @param  upper   the exclusive upper bound for indexes of included entries.
    * @return a non-strict view of the entries with indexes greater than or
    *         equal to `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int): Map[A, T] =
    new MapOps.Slice(these, lower, upper)
}

private[nonstrict] object MapOps {
  import scala.annotation.unchecked.uncheckedVariance
  
  class Filter[+A, +T](
      protected[this] override val base: Map[A, T],
      protected[this] override val p: ((A, T)) => Boolean)
    extends ContainerOps.Filter[(A, T)](base, p) with Map[A, T]
  
  class DropWhile[+A, +T](
      protected[this] override val base: Map[A, T],
      protected[this] override val p: ((A, T)) => Boolean)
    extends ContainerOps.DropWhile[(A, T)](base, p) with Map[A, T]
  
  class TakeWhile[+A, +T](
      protected[this] override val base: Map[A, T],
      protected[this] override val p: ((A, T)) => Boolean)
    extends ContainerOps.TakeWhile[(A, T)](base, p) with Map[A, T]
  
  class Drop[+A, +T](
      protected[this] override val base: Map[A, T],
      protected[this] override val lower: Int)
    extends ContainerOps.Drop[(A, T)](base, lower) with Map[A, T]
  
  class Take[+A, +T](
      protected[this] override val base: Map[A, T],
      protected[this] override val upper: Int)
    extends ContainerOps.Take[(A, T)](base, upper) with Map[A, T]
  
  class Slice[+A, +T](
      protected[this] override val base: Map[A, T],
      protected[this] override val lower: Int,
      protected[this] override val upper: Int)
    extends ContainerOps.Slice[(A, T)](base, lower, upper) with Map[A, T]
}
