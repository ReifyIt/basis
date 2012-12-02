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

/** Non-strictly evaluated sequence operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
final class SeqOps[+A](val these: Seq[A]) extends AnyVal {
  /** Returns a view that applies a partial function to each element in this
    * sequence for which the function is defined.
    * 
    * @param  q   the partial function to lazily filter and map elements.
    * @return a non-strict view of the filtered and mapped elements.
    * @group  Mapping
    */
  def collect[B](q: PartialFunction[A, B]): Seq[B] =
    new SeqOps.Collect(these, q)
  
  /** Returns a view that applies a function to each element in this sequence.
    * 
    * @param  f   the function to lazily apply to each element.
    * @return a non-strict view of the mapped elements.
    * @group  Mapping
    */
  def map[B](f: A => B): Seq[B] =
    new SeqOps.Map(these, f)
  
  /** Returns a view concatenating all elements returned by a function
    * applied to each element in this sequence.
    * 
    * @param  f   the sequence-yielding function to apply to each element.
    * @return a non-strict view concatenating all elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => Seq[B]): Seq[B] =
    new SeqOps.FlatMap(these, f)
  
  /** Returns a view of all elements in this sequence that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def filter(p: A => Boolean): Seq[A] =
    new SeqOps.Filter(these, p)
  
  /** Returns a view of all elements following the longest prefix of this
    * sequence for which each element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the suffix of accumulated elements beginning
    *         with the first element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: A => Boolean): Seq[A] =
    new SeqOps.DropWhile(these, p)
  
  /** Returns a view of the longest prefix of this sequence for which each
    * element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the longest prefix of elements preceding
    *         the first element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: A => Boolean): Seq[A] =
    new SeqOps.TakeWhile(these, p)
  
  /** Returns a (prefix, suffix) pair of views with the prefix being the
    * longest one for which each element satisfies a predicate, and the suffix
    * beginning with the first element to not satisfy the predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the (predix, suffix) pair of non-strict views.
    * @group  Filtering
    */
  def span(p: A => Boolean): (Seq[A], Seq[A]) =
    (takeWhile(p), dropWhile(p))
  
  /** Returns a view of all elements in this sequence following a prefix
    * up to some length.
    * 
    * @param  lower   the length of the prefix to drop; also the inclusive
    *                 lower bound for indexes of included elements.
    * @return a non-strict view of all but the first `lower` elements.
    * @group  Filtering
    */
  def drop(lower: Int): Seq[A] =
    new SeqOps.Drop(these, lower)
  
  /** Returns a view of a prefix of this sequence up to some length.
    * 
    * @param  upper   the length of the prefix to take; also the exclusive
    *                 upper bound for indexes of included elements.
    * @return a non-strict view of up to the first `upper` elements.
    * @group  Filtering
    */
  def take(upper: Int): Seq[A] =
    new SeqOps.Take(these, upper)
  
  /** Returns a view of an interval of elements in this sequence.
    * 
    * @param  lower   the inclusive lower bound for indexes of included elements.
    * @param  upper   the exclusive upper bound for indexes of included elements.
    * @return a non-strict view of the elements with indexes greater than or
    *         equal to `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int): Seq[A] =
    new SeqOps.Slice(these, lower, upper)
  
  /** Returns a view of pairs of elemnts from this and another sequence.
    * 
    * @param  those   the sequence whose elements to lazily pair with these elements.
    * @return a non-strict view of the pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B](those: Seq[B]): Seq[(A, B)] =
    new SeqOps.Zip(these, those)
  
  /** Returns a view concatenating this and another sequence.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: Seq[B]): Seq[B] =
    new SeqOps.++(these, those)
}

private[nonstrict] object SeqOps {
  class Collect[-A, +B](
      protected[this] override val base: Seq[A],
      protected[this] override val q: PartialFunction[A, B])
    extends ContainerOps.Collect[A, B](base, q) with Seq[B]
  
  class Map[-A, +B](
      protected[this] override val base: Seq[A],
      protected[this] override val f: A => B)
    extends ContainerOps.Map[A, B](base, f) with Seq[B]
  
  class FlatMap[-A, +B](
      protected[this] override val base: Seq[A],
      protected[this] override val f: A => Seq[B])
    extends ContainerOps.FlatMap[A, B](base, f) with Seq[B]
  
  class Filter[+A](
      protected[this] override val base: Seq[A],
      protected[this] override val p: A => Boolean)
    extends ContainerOps.Filter[A](base, p) with Seq[A]
  
  class DropWhile[+A](
      protected[this] override val base: Seq[A],
      protected[this] override val p: A => Boolean)
    extends ContainerOps.DropWhile[A](base, p) with Seq[A]
  
  class TakeWhile[+A](
      protected[this] override val base: Seq[A],
      protected[this] override val p: A => Boolean)
    extends ContainerOps.TakeWhile[A](base, p) with Seq[A]
  
  class Drop[+A](
      protected[this] override val base: Seq[A],
      protected[this] override val lower: Int)
    extends ContainerOps.Drop[A](base, lower) with Seq[A]
  
  class Take[+A](
      protected[this] override val base: Seq[A],
      protected[this] override val upper: Int)
    extends ContainerOps.Take[A](base, upper) with Seq[A]
  
  class Slice[+A](
      protected[this] override val base: Seq[A],
      protected[this] override val lower: Int,
      protected[this] override val upper: Int)
    extends ContainerOps.Slice[A](base, lower, upper) with Seq[A]
  
  class Zip[+A, +B](
      protected[this] override val xs: Seq[A],
      protected[this] override val ys: Seq[B])
    extends ContainerOps.Zip[A, B](xs, ys) with Seq[(A, B)]
  
  class ++[+A](
      protected[this] override val xs: Seq[A],
      protected[this] override val ys: Seq[A])
    extends ContainerOps.++[A](xs, ys) with Seq[A]
}
