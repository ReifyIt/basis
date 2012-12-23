/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

/** Non-strictly evaluated set operations.
  * 
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
final class NonStrictSetOps[+A](val these: Set[A]) extends AnyVal {
  /** Returns a view of all elements in this set that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def filter(p: A => Boolean): Set[A] =
    new NonStrictSetOps.Filter(these, p)
  
  /** Returns a view of all elements in this set that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def withFilter(p: A => Boolean): Set[A] =
    new NonStrictSetOps.Filter(these, p)
  
  /** Returns a view of all elements following the longest prefix of this
    * set for which each element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the suffix of accumulated elements beginning
    *         with the first element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: A => Boolean): Set[A] =
    new NonStrictSetOps.DropWhile(these, p)
  
  /** Returns a view of the longest prefix of this set for which each
    * element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the longest prefix of elements preceding
    *         the first element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: A => Boolean): Set[A] =
    new NonStrictSetOps.TakeWhile(these, p)
  
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
    new NonStrictSetOps.Drop(these, lower)
  
  /** Returns a view of a prefix of this set up to some length.
    * 
    * @param  upper   the length of the prefix to take; also the exclusive
    *                 upper bound for indexes of included elements.
    * @return a non-strict view of up to the first `upper` elements.
    * @group  Filtering
    */
  def take(upper: Int): Set[A] =
    new NonStrictSetOps.Take(these, upper)
  
  /** Returns a view of an interval of elements in this set.
    * 
    * @param  lower   the inclusive lower bound for indexes of included elements.
    * @param  upper   the exclusive upper bound for indexes of included elements.
    * @return a non-strict view of the elements with indexes greater than or
    *         equal to `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int): Set[A] =
    new NonStrictSetOps.Slice(these, lower, upper)
  
  /** Returns a view concatenating this and another set.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: Set[B]): Set[B] =
    new NonStrictSetOps.++(these, those)
}

private[sequential] object NonStrictSetOps {
  import scala.annotation.tailrec
  import scala.annotation.unchecked.uncheckedVariance
  
  class Filter[+A](
      protected[this] override val these: Set[A],
      protected[this] override val p: A => Boolean)
    extends NonStrictContainerOps.Filter[A](these, p) with Set[A] {
    
    override def contains(element: A @uncheckedVariance): Boolean =
      (these contains element) && p(element)
  }
  
  class DropWhile[+A](
      protected[this] override val these: Set[A],
      protected[this] override val p: A => Boolean)
    extends NonStrictContainerOps.DropWhile[A](these, p) with Set[A]
  
  class TakeWhile[+A](
      protected[this] override val these: Set[A],
      protected[this] override val p: A => Boolean)
    extends NonStrictContainerOps.TakeWhile[A](these, p) with Set[A]
  
  class Drop[+A](
      protected[this] override val these: Set[A],
      protected[this] override val lower: Int)
    extends NonStrictContainerOps.Drop[A](these, lower) with Set[A]
  
  class Take[+A](
      protected[this] override val these: Set[A],
      protected[this] override val upper: Int)
    extends NonStrictContainerOps.Take[A](these, upper) with Set[A]
  
  class Slice[+A](
      protected[this] override val these: Set[A],
      protected[this] override val lower: Int,
      protected[this] override val upper: Int)
    extends NonStrictContainerOps.Slice[A](these, lower, upper) with Set[A]
  
  class ++[+A](
      protected[this] override val these: Set[A],
      protected[this] override val those: Set[A])
    extends NonStrictContainerOps.++[A](these, those) with Set[A] {
    
    override def contains(element: A @uncheckedVariance): Boolean =
      these.contains(element) || those.contains(element)
    
    override def iterator: Iterator[A] = new UnionIterator(these, those)
  }
  
  final class UnionIterator[+A] private (
      private[this] val base: Set[A],
      private[this] val these: Iterator[A],
      private[this] val those: Iterator[A],
      private[this] var segment: Int)
    extends Iterator[A] {
    
    def this(these: Set[A], those: Set[A]) = this(these, these.iterator, those.iterator, 0)
    
    @tailrec override def isEmpty: Boolean = segment match {
      case 0 => these.isEmpty && { segment = 1; isEmpty }
      case 1 => those.isEmpty || base.contains(those.head) || { those.step(); isEmpty }
    }
    
    @tailrec override def head: A = segment match {
      case 0 => if (!these.isEmpty) these.head else { segment = 1; head }
      case 1 =>
        val y = those.head
        if (!base.contains(y)) y else { those.step(); head }
    }
    
    @tailrec override def step(): Unit = segment match {
      case 0 => if (!these.isEmpty) these.step() else { segment = 1; step() }
      case 1 => those.step()
    }
    
    override def dup: Iterator[A] = new UnionIterator(base, these, those, segment)
  }
}
