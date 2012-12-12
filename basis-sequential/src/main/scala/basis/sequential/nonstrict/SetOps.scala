/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential
package nonstrict

import basis.collections._

/** Non-strictly evaluated set operations.
  * 
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
final class SetOps[+A](val these: Set[A]) extends AnyVal {
  /** Returns a view of all elements in this set that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def filter(p: A => Boolean): Set[A] =
    new SetOps.Filter(these, p)
  
  /** Returns a view of all elements following the longest prefix of this
    * set for which each element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the suffix of accumulated elements beginning
    *         with the first element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: A => Boolean): Set[A] =
    new SetOps.DropWhile(these, p)
  
  /** Returns a view of the longest prefix of this set for which each
    * element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the longest prefix of elements preceding
    *         the first element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: A => Boolean): Set[A] =
    new SetOps.TakeWhile(these, p)
  
  /** Returns a (prefix, suffix) pair of views with the prefix being the
    * longest one for which each element satisfies a predicate, and the suffix
    * beginning with the first element to not satisfy the predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the (predix, suffix) pair of non-strict views.
    * @group  Filtering
    */
  def span(p: A => Boolean): (Set[A], Set[A]) =
    (takeWhile(p), dropWhile(p))
  
  /** Returns a view of all elements in this set following a prefix
    * up to some length.
    * 
    * @param  lower   the length of the prefix to drop; also the inclusive
    *                 lower bound for indexes of included elements.
    * @return a non-strict view of all but the first `lower` elements.
    * @group  Filtering
    */
  def drop(lower: Int): Set[A] =
    new SetOps.Drop(these, lower)
  
  /** Returns a view of a prefix of this set up to some length.
    * 
    * @param  upper   the length of the prefix to take; also the exclusive
    *                 upper bound for indexes of included elements.
    * @return a non-strict view of up to the first `upper` elements.
    * @group  Filtering
    */
  def take(upper: Int): Set[A] =
    new SetOps.Take(these, upper)
  
  /** Returns a view of an interval of elements in this set.
    * 
    * @param  lower   the inclusive lower bound for indexes of included elements.
    * @param  upper   the exclusive upper bound for indexes of included elements.
    * @return a non-strict view of the elements with indexes greater than or
    *         equal to `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int): Set[A] =
    new SetOps.Slice(these, lower, upper)
  
  /** Returns a view concatenating this and another set.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: Set[B]): Set[B] =
    new SetOps.++(these, those)
}

private[nonstrict] object SetOps {
  import scala.annotation.tailrec
  import scala.annotation.unchecked.uncheckedVariance
  
  class Filter[+A](
      protected[this] override val base: Set[A],
      protected[this] override val p: A => Boolean)
    extends ContainerOps.Filter[A](base, p) with Set[A] {
    
    override def contains(element: A @uncheckedVariance): Boolean =
      (base contains element) && p(element)
  }
  
  class DropWhile[+A](
      protected[this] override val base: Set[A],
      protected[this] override val p: A => Boolean)
    extends ContainerOps.DropWhile[A](base, p) with Set[A]
  
  class TakeWhile[+A](
      protected[this] override val base: Set[A],
      protected[this] override val p: A => Boolean)
    extends ContainerOps.TakeWhile[A](base, p) with Set[A]
  
  class Drop[+A](
      protected[this] override val base: Set[A],
      protected[this] override val lower: Int)
    extends ContainerOps.Drop[A](base, lower) with Set[A]
  
  class Take[+A](
      protected[this] override val base: Set[A],
      protected[this] override val upper: Int)
    extends ContainerOps.Take[A](base, upper) with Set[A]
  
  class Slice[+A](
      protected[this] override val base: Set[A],
      protected[this] override val lower: Int,
      protected[this] override val upper: Int)
    extends ContainerOps.Slice[A](base, lower, upper) with Set[A]
  
  class ++[+A](
      protected[this] override val xs: Set[A],
      protected[this] override val ys: Set[A])
    extends ContainerOps.++[A](xs, ys) with Set[A] {
    
    override def contains(element: A @uncheckedVariance): Boolean =
      xs.contains(element) || ys.contains(element)
    
    override def iterator: Iterator[A] = new UnionIterator(xs, ys)
  }
  
  final class UnionIterator[+A] private (
      private[this] val base: Set[A],
      private[this] val xs: Iterator[A],
      private[this] val ys: Iterator[A],
      private[this] var segment: Int)
    extends Iterator[A] {
    
    def this(xs: Set[A], ys: Set[A]) = this(xs, xs.iterator, ys.iterator, 0)
    
    @tailrec override def isEmpty: Boolean = segment match {
      case 0 => xs.isEmpty && { segment = 1; isEmpty }
      case 1 => ys.isEmpty || base.contains(ys.head) || { ys.step(); isEmpty }
    }
    
    @tailrec override def head: A = segment match {
      case 0 => if (!xs.isEmpty) xs.head else { segment = 1; head }
      case 1 =>
        val y = ys.head
        if (!base.contains(y)) y else { ys.step(); head }
    }
    
    @tailrec override def step(): Unit = segment match {
      case 0 => if (!xs.isEmpty) xs.step() else { segment = 1; step() }
      case 1 => ys.step()
    }
    
    override def dup: Iterator[A] = new UnionIterator(base, xs, ys, segment)
  }
}
