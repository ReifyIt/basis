/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential

import basis.collections._

/** Non-strictly evaluated linear sequence operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
final class NonStrictStackOps[+A](val these: Stack[A]) extends AnyVal {
  /** Returns a view that applies a partial function to each element in this
    * sequence for which the function is defined.
    * 
    * @param  q   the partial function to lazily filter and map elements.
    * @return a non-strict view of the filtered and mapped elements.
    * @group  Mapping
    */
  def collect[B](q: PartialFunction[A, B]): Stack[B] =
    new NonStrictStackOps.Collect(these, q)
  
  /** Returns a view that applies a function to each element in this sequence.
    * 
    * @param  f   the function to lazily apply to each element.
    * @return a non-strict view of the mapped elements.
    * @group  Mapping
    */
  def map[B](f: A => B): Stack[B] =
    new NonStrictStackOps.Map(these, f)
  
  /** Returns a view concatenating all elements returned by a function
    * applied to each element in this sequence.
    * 
    * @param  f   the sequence-yielding function to apply to each element.
    * @return a non-strict view concatenating all elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => Stack[B]): Stack[B] =
    new NonStrictStackOps.FlatMap(these, f)
  
  /** Returns a view of all elements in this sequence that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def filter(p: A => Boolean): Stack[A] =
    new NonStrictStackOps.Filter(these, p)
  
  /** Returns a view of all elements following the longest prefix of this
    * sequence for which each element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the suffix of accumulated elements beginning
    *         with the first element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: A => Boolean): Stack[A] =
    new NonStrictStackOps.DropWhile(these, p)
  
  /** Returns a view of the longest prefix of this sequence for which each
    * element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the longest prefix of elements preceding
    *         the first element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: A => Boolean): Stack[A] =
    new NonStrictStackOps.TakeWhile(these, p)
  
  /** Returns a (prefix, suffix) pair of views with the prefix being the
    * longest one for which each element satisfies a predicate, and the suffix
    * beginning with the first element to not satisfy the predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the (predix, suffix) pair of non-strict views.
    * @group  Filtering
    */
  def span(p: A => Boolean): (Stack[A], Stack[A]) =
    (takeWhile(p), dropWhile(p))
  
  /** Returns a view of all elements in this sequence following a prefix
    * up to some length.
    * 
    * @param  lower   the length of the prefix to drop; also the inclusive
    *                 lower bound for indexes of included elements.
    * @return a non-strict view of all but the first `lower` elements.
    * @group  Filtering
    */
  def drop(lower: Int): Stack[A] =
    new NonStrictStackOps.Drop(these, lower)
  
  /** Returns a view of a prefix of this sequence up to some length.
    * 
    * @param  upper   the length of the prefix to take; also the exclusive
    *                 upper bound for indexes of included elements.
    * @return a non-strict view of up to the first `upper` elements.
    * @group  Filtering
    */
  def take(upper: Int): Stack[A] =
    new NonStrictStackOps.Take(these, upper)
  
  /** Returns a view of an interval of elements in this sequence.
    * 
    * @param  lower   the inclusive lower bound for indexes of included elements.
    * @param  upper   the exclusive upper bound for indexes of included elements.
    * @return a non-strict view of the elements with indexes greater than or
    *         equal to `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int): Stack[A] =
    new NonStrictStackOps.Slice(these, lower, upper)
  
  /** Returns a view of pairs of elemnts from this and another sequence.
    * 
    * @param  those   the sequence whose elements to lazily pair with these elements.
    * @return a non-strict view of the pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B](those: Stack[B]): Stack[(A, B)] =
    new NonStrictStackOps.Zip(these, those)
  
  /** Returns a view concatenating this and another sequence.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: Stack[B]): Stack[B] =
    new NonStrictStackOps.++(these, those)
}

private[sequential] object NonStrictStackOps {
  import scala.annotation.tailrec
  import basis.util.IntOps
  
  object Empty extends Stack[Nothing] {
    override def isEmpty: Boolean = true
    
    override def head: Nothing = throw new NoSuchElementException
    
    override def tail: Stack[Nothing] = throw new UnsupportedOperationException
  }
  
  final class Collect[-A, +B](
      private[this] var base: Stack[A],
      private[this] val q: PartialFunction[A, B])
    extends Stack[B] {
    
    @tailrec override def isEmpty: Boolean =
      base.isEmpty || !q.isDefinedAt(base.head) && { base = base.tail; isEmpty }
    
    @tailrec override def head: B = {
      val x = base.head
      if (q.isDefinedAt(x)) q(x)
      else { base = base.tail; head }
    }
    
    @tailrec override def tail: Stack[B] = {
      if (!base.isEmpty && q.isDefinedAt(base.head)) new Collect(base.tail, q)
      else { base = base.tail; tail }
    }
  }
  
  final class Map[-A, +B](
      private[this] val base: Stack[A],
      private[this] val f: A => B)
    extends Stack[B] {
    
    override def isEmpty: Boolean = base.isEmpty
    
    override def head: B = f(base.head)
    
    override def tail: Stack[B] = new Map(base.tail, f)
  }
  
  final class FlatMap[-A, +B] private (
      private[this] var base: Stack[A],
      private[this] val f: A => Stack[B],
      private[this] var inner: Stack[B])
    extends Stack[B] {
    
    def this(base: Stack[A], f: A => Stack[B]) = this(base, f, Empty)
    
    @tailrec override def isEmpty: Boolean =
      inner.isEmpty && (base.isEmpty || { inner = f(base.head); base = base.tail; isEmpty })
    
    @tailrec override def head: B = {
      if (!inner.isEmpty) inner.head
      else if (!base.isEmpty) { inner = f(base.head); base = base.tail; head }
      else Empty.head
    }
    
    override def tail: Stack[B] = {
      if (!inner.isEmpty) new FlatMap(base, f, inner.tail)
      else if (!base.isEmpty) new FlatMap(base.tail, f, f(base.head))
      else Empty.tail
    }
  }
  
  final class Filter[+A](
      private[this] var base: Stack[A],
      private[this] val p: A => Boolean)
    extends Stack[A] {
    
    @tailrec override def isEmpty: Boolean =
      base.isEmpty || !p(base.head) && { base = base.tail; isEmpty }
    
    @tailrec override def head: A = {
      val x = base.head
      if (p(x)) x else { base = base.tail; head }
    }
    
    @tailrec override def tail: Stack[A] = {
      if (!base.isEmpty && p(base.head)) new Filter(base.tail, p)
      else { base = base.tail; tail }
    }
  }
  
  final class DropWhile[+A](
      private[this] var base: Stack[A],
      private[this] val p: A => Boolean)
    extends Stack[A] {
    
    private[this] var dropped: Boolean = false
    
    @tailrec override def isEmpty: Boolean =
      base.isEmpty || (!dropped && (if (p(base.head)) { base = base.tail; isEmpty } else { dropped = true; false }))
    
    @tailrec override def head: A = {
      if (dropped) base.head
      else {
        val x = base.head
        if (!p(x)) { dropped = true; x } else { base = base.tail; head }
      }
    }
    
    @tailrec override def tail: Stack[A] = {
      if (dropped) base.tail
      else if (!p(base.head)) { dropped = true; tail }
      else { base = base.tail; tail }
    }
  }
  
  final class TakeWhile[+A](
      private[this] val base: Stack[A],
      private[this] val p: A => Boolean)
    extends Stack[A] {
    
    private[this] lazy val taking: Boolean = !base.isEmpty && p(base.head)
    
    override def isEmpty: Boolean = !taking
    
    override def head: A = {
      if (taking) base.head
      else Empty.head
    }
    
    override def tail: Stack[A] = {
      if (taking) new TakeWhile(base.tail, p)
      else Empty.tail
    }
  }
  
  final class Drop[+A] private (
      private[this] var base: Stack[A],
      private[this] val lower: Int,
      private[this] var index: Int)
    extends Stack[A] {
    
    def this(base: Stack[A], lower: Int) = this(base, lower, 0)
    
    @tailrec override def isEmpty: Boolean =
      base.isEmpty || index < lower && { base = base.tail; index += 1; isEmpty }
    
    @tailrec override def head: A = {
      if (index >= lower) base.head
      else { base = base.tail; index += 1; head }
    }
    
    @tailrec override def tail: Stack[A] = {
      if (index >= lower) base.tail
      else { base = base.tail; index += 1; tail }
    }
  }
  
  final class Take[+A] private (
      private[this] val base: Stack[A],
      private[this] val upper: Int,
      private[this] val index: Int)
    extends Stack[A] {
    
    def this(base: Stack[A], upper: Int) = this(base, upper, 0)
    
    override def isEmpty: Boolean =
      index >= upper || base.isEmpty
    
    override def head: A = {
      if (index < upper) base.head
      else Empty.head
    }
    
    override def tail: Stack[A] = {
      if (index < upper) new Take(base.tail, upper, index + 1)
      else Empty.tail
    }
  }
  
  final class Slice[+A] private (
      private[this] var base: Stack[A],
      private[this] val lower: Int,
      private[this] val upper: Int,
      private[this] var index: Int)
    extends Stack[A] {
    
    def this(base: Stack[A], lower: Int, upper: Int) =
      this(base,0 max lower, 0 max lower max upper, 0)
    
    @tailrec override def isEmpty: Boolean =
      index >= upper || base.isEmpty || index < lower && { base = base.tail; index += 1; isEmpty }
    
    @tailrec override def head: A = {
      if (index < lower) { base = base.tail; index += 1; head }
      else if (index < upper) base.head
      else Empty.head
    }
    
    @tailrec override def tail: Stack[A] = {
      if (index < lower) { base = base.tail; index += 1; tail }
      else if (index < upper) new Slice(base.tail, lower, upper, index + 1)
      else Empty.tail
    }
  }
  
  final class Zip[+A, +B](
      private[this] val xs: Stack[A],
      private[this] val ys: Stack[B])
    extends Stack[(A, B)] {
    
    override def isEmpty: Boolean = xs.isEmpty || ys.isEmpty
    
    override def head: (A, B) = (xs.head, ys.head)
    
    override def tail: Stack[(A, B)] = new Zip(xs.tail, ys.tail)
  }
  
  final class ++[+A] private (
      private[this] val xs: Stack[A],
      private[this] val ys: Stack[A],
      private[this] var segment: Int)
    extends Stack[A] {
    
    def this(xs: Stack[A], ys: Stack[A]) = this(xs, ys, 0)
    
    @tailrec override def isEmpty: Boolean = segment match {
      case 0 => xs.isEmpty && { segment = 1; isEmpty }
      case 1 => ys.isEmpty
    }
    
    @tailrec override def head: A = segment match {
      case 0 => if (!xs.isEmpty) xs.head else { segment = 1; head }
      case 1 => ys.head
    }
    
    override def tail: Stack[A] = segment match {
      case 0 if !xs.isEmpty => new ++(xs.tail, ys, 0)
      case _ => ys.tail
    }
  }
}
