/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

/** Non-strictly evaluated queue operations.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    NonStrict
  * 
  * @groupprio  Mapping     1
  * @groupprio  Filtering   2
  * @groupprio  Combining   3
  * 
  * @define collection  queue
  */
final class NonStrictQueueOps[+A](val these: Queue[A]) extends AnyVal {
  /** Returns a view that applies a partial function to each element in this
    * $collection for which the function is defined.
    * 
    * @param  q   the partial function to lazily filter and map elements.
    * @return a non-strict view of the filtered and mapped elements.
    * @group  Mapping
    */
  def collect[B](q: PartialFunction[A, B]): Queue[B] =
    new NonStrictQueueOps.Collect(these, q)
  
  /** Returns a view that applies a function to each element in this $collection.
    * 
    * @param  f   the function to lazily apply to each element.
    * @return a non-strict view of the mapped elements.
    * @group  Mapping
    */
  def map[B](f: A => B): Queue[B] =
    new NonStrictQueueOps.Map(these, f)
  
  /** Returns a view concatenating all elements returned by a function
    * applied to each element in this $collection.
    * 
    * @param  f   the $collection-yielding function to apply to each element.
    * @return a non-strict view concatenating all elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => Queue[B]): Queue[B] =
    new NonStrictQueueOps.FlatMap(these, f)
  
  /** Returns a view of all elements in this $collection that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def filter(p: A => Boolean): Queue[A] =
    new NonStrictQueueOps.Filter(these, p)
  
  /** Returns a view of all elements in this $collection that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def withFilter(p: A => Boolean): Queue[A] =
    new NonStrictQueueOps.Filter(these, p)
  
  /** Returns a view of all elements following the longest prefix of this
    * $collection for which each element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the suffix of accumulated elements beginning
    *         with the first element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: A => Boolean): Queue[A] =
    new NonStrictQueueOps.DropWhile(these, p)
  
  /** Returns a view of the longest prefix of this $collection for which each
    * element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the longest prefix of elements preceding
    *         the first element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: A => Boolean): Queue[A] =
    new NonStrictQueueOps.TakeWhile(these, p)
  
  /** Returns a (prefix, suffix) pair of views with the prefix being the
    * longest one for which each element satisfies a predicate, and the suffix
    * beginning with the first element to not satisfy the predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the (predix, suffix) pair of non-strict views.
    * @group  Filtering
    */
  def span(p: A => Boolean): (Queue[A], Queue[A]) =
    (takeWhile(p), dropWhile(p))
  
  /** Returns a view of all elements in this $collection following a prefix
    * up to some length.
    * 
    * @param  lower   the length of the prefix to drop; also the inclusive
    *                 lower bound for indexes of included elements.
    * @return a non-strict view of all but the first `lower` elements.
    * @group  Filtering
    */
  def drop(lower: Int): Queue[A] =
    new NonStrictQueueOps.Drop(these, lower)
  
  /** Returns a view of a prefix of this $collection up to some length.
    * 
    * @param  upper   the length of the prefix to take; also the exclusive
    *                 upper bound for indexes of included elements.
    * @return a non-strict view of up to the first `upper` elements.
    * @group  Filtering
    */
  def take(upper: Int): Queue[A] =
    new NonStrictQueueOps.Take(these, upper)
  
  /** Returns a view of an interval of elements in this $collection.
    * 
    * @param  lower   the inclusive lower bound for indexes of included elements.
    * @param  upper   the exclusive upper bound for indexes of included elements.
    * @return a non-strict view of the elements with indexes greater than or
    *         equal to `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int): Queue[A] =
    new NonStrictQueueOps.Slice(these, lower, upper)
  
  /** Returns a view of pairs of elemnts from this and another $collection.
    * 
    * @param  those   the $collection whose elements to lazily pair with these elements.
    * @return a non-strict view of the pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B](those: Queue[B]): Queue[(A, B)] =
    new NonStrictQueueOps.Zip(these, those)
  
  /** Returns a view concatenating this and another $collection.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: Queue[B]): Queue[B] =
    new NonStrictQueueOps.++(these, those)
}

private[sequential] object NonStrictQueueOps {
  import scala.annotation.tailrec
  import basis.util.IntOps
  
  object Empty extends Queue[Nothing] {
    override def isEmpty: Boolean = true
    
    override def head: Nothing = throw new NoSuchElementException
    
    override def tail: Queue[Nothing] = throw new UnsupportedOperationException
  }
  
  final class Collect[-A, +B](
      private[this] var these: Queue[A],
      private[this] val q: PartialFunction[A, B])
    extends Queue[B] {
    
    @tailrec override def isEmpty: Boolean =
      these.isEmpty || !q.isDefinedAt(these.head) && { these = these.tail; isEmpty }
    
    @tailrec override def head: B = {
      val x = these.head
      if (q.isDefinedAt(x)) q(x)
      else { these = these.tail; head }
    }
    
    @tailrec override def tail: Queue[B] = {
      if (!these.isEmpty && q.isDefinedAt(these.head)) new Collect(these.tail, q)
      else { these = these.tail; tail }
    }
  }
  
  final class Map[-A, +B](
      private[this] val these: Queue[A],
      private[this] val f: A => B)
    extends Queue[B] {
    
    override def isEmpty: Boolean = these.isEmpty
    
    override def head: B = f(these.head)
    
    override def tail: Queue[B] = new Map(these.tail, f)
  }
  
  final class FlatMap[-A, +B] private (
      private[this] var these: Queue[A],
      private[this] val f: A => Queue[B],
      private[this] var inner: Queue[B])
    extends Queue[B] {
    
    def this(these: Queue[A], f: A => Queue[B]) = this(these, f, Empty)
    
    @tailrec override def isEmpty: Boolean =
      inner.isEmpty && (these.isEmpty || { inner = f(these.head); these = these.tail; isEmpty })
    
    @tailrec override def head: B = {
      if (!inner.isEmpty) inner.head
      else if (!these.isEmpty) { inner = f(these.head); these = these.tail; head }
      else Empty.head
    }
    
    override def tail: Queue[B] = {
      if (!inner.isEmpty) new FlatMap(these, f, inner.tail)
      else if (!these.isEmpty) new FlatMap(these.tail, f, f(these.head))
      else Empty.tail
    }
  }
  
  final class Filter[+A](
      private[this] var these: Queue[A],
      private[this] val p: A => Boolean)
    extends Queue[A] {
    
    @tailrec override def isEmpty: Boolean =
      these.isEmpty || !p(these.head) && { these = these.tail; isEmpty }
    
    @tailrec override def head: A = {
      val x = these.head
      if (p(x)) x else { these = these.tail; head }
    }
    
    @tailrec override def tail: Queue[A] = {
      if (!these.isEmpty && p(these.head)) new Filter(these.tail, p)
      else { these = these.tail; tail }
    }
  }
  
  final class DropWhile[+A](
      private[this] var these: Queue[A],
      private[this] val p: A => Boolean)
    extends Queue[A] {
    
    private[this] var dropped: Boolean = false
    
    @tailrec override def isEmpty: Boolean =
      these.isEmpty || (!dropped && (if (p(these.head)) { these = these.tail; isEmpty } else { dropped = true; false }))
    
    @tailrec override def head: A = {
      if (dropped) these.head
      else {
        val x = these.head
        if (!p(x)) { dropped = true; x } else { these = these.tail; head }
      }
    }
    
    @tailrec override def tail: Queue[A] = {
      if (dropped) these.tail
      else if (!p(these.head)) { dropped = true; tail }
      else { these = these.tail; tail }
    }
  }
  
  final class TakeWhile[+A](
      private[this] val these: Queue[A],
      private[this] val p: A => Boolean)
    extends Queue[A] {
    
    private[this] lazy val taking: Boolean = !these.isEmpty && p(these.head)
    
    override def isEmpty: Boolean = !taking
    
    override def head: A = {
      if (taking) these.head
      else Empty.head
    }
    
    override def tail: Queue[A] = {
      if (taking) new TakeWhile(these.tail, p)
      else Empty.tail
    }
  }
  
  final class Drop[+A] private (
      private[this] var these: Queue[A],
      private[this] val lower: Int,
      private[this] var index: Int)
    extends Queue[A] {
    
    def this(these: Queue[A], lower: Int) = this(these, lower, 0)
    
    @tailrec override def isEmpty: Boolean =
      these.isEmpty || index < lower && { these = these.tail; index += 1; isEmpty }
    
    @tailrec override def head: A = {
      if (index >= lower) these.head
      else { these = these.tail; index += 1; head }
    }
    
    @tailrec override def tail: Queue[A] = {
      if (index >= lower) these.tail
      else { these = these.tail; index += 1; tail }
    }
  }
  
  final class Take[+A] private (
      private[this] val these: Queue[A],
      private[this] val upper: Int,
      private[this] val index: Int)
    extends Queue[A] {
    
    def this(these: Queue[A], upper: Int) = this(these, upper, 0)
    
    override def isEmpty: Boolean =
      index >= upper || these.isEmpty
    
    override def head: A = {
      if (index < upper) these.head
      else Empty.head
    }
    
    override def tail: Queue[A] = {
      if (index < upper) new Take(these.tail, upper, index + 1)
      else Empty.tail
    }
  }
  
  final class Slice[+A] private (
      private[this] var these: Queue[A],
      private[this] val lower: Int,
      private[this] val upper: Int,
      private[this] var index: Int)
    extends Queue[A] {
    
    def this(these: Queue[A], lower: Int, upper: Int) =
      this(these,0 max lower, 0 max lower max upper, 0)
    
    @tailrec override def isEmpty: Boolean =
      index >= upper || these.isEmpty || index < lower && { these = these.tail; index += 1; isEmpty }
    
    @tailrec override def head: A = {
      if (index < lower) { these = these.tail; index += 1; head }
      else if (index < upper) these.head
      else Empty.head
    }
    
    @tailrec override def tail: Queue[A] = {
      if (index < lower) { these = these.tail; index += 1; tail }
      else if (index < upper) new Slice(these.tail, lower, upper, index + 1)
      else Empty.tail
    }
  }
  
  final class Zip[+A, +B](
      private[this] val these: Queue[A],
      private[this] val those: Queue[B])
    extends Queue[(A, B)] {
    
    override def isEmpty: Boolean = these.isEmpty || those.isEmpty
    
    override def head: (A, B) = (these.head, those.head)
    
    override def tail: Queue[(A, B)] = new Zip(these.tail, those.tail)
  }
  
  final class ++[+A] private (
      private[this] val these: Queue[A],
      private[this] val those: Queue[A],
      private[this] var segment: Int)
    extends Queue[A] {
    
    def this(these: Queue[A], those: Queue[A]) = this(these, those, 0)
    
    @tailrec override def isEmpty: Boolean = segment match {
      case 0 => these.isEmpty && { segment = 1; isEmpty }
      case 1 => those.isEmpty
    }
    
    @tailrec override def head: A = segment match {
      case 0 => if (!these.isEmpty) these.head else { segment = 1; head }
      case 1 => those.head
    }
    
    override def tail: Queue[A] = segment match {
      case 0 if !these.isEmpty => new ++(these.tail, those, 0)
      case _ => those.tail
    }
  }
}
