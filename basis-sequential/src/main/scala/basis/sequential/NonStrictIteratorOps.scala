/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

/** Non-strictly evaluated iterator operations.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * @group    NonStrict
  * 
  * @groupprio  Mapping     1
  * @groupprio  Filtering   2
  * @groupprio  Combining   3
  * 
  * @define collection  iterator
  */
final class NonStrictIteratorOps[+A](val these: Iterator[A]) extends AnyVal {
  /** Returns a view that applies a partial function to each element in this
    * $collection for which the function is defined.
    * 
    * @param  q   the partial function to lazily filter and map elements.
    * @return a non-strict view of the filtered and mapped elements.
    * @group  Mapping
    */
  def collect[B](q: PartialFunction[A, B]): Iterator[B] =
    new NonStrictIteratorOps.Collect(these.dup, q)
  
  /** Returns a view that applies a function to each element in this $collection.
    * 
    * @param  f   the function to lazily apply to each element.
    * @return a non-strict view of the mapped elements.
    * @group  Mapping
    */
  def map[B](f: A => B): Iterator[B] =
    new NonStrictIteratorOps.Map(these.dup, f)
  
  /** Returns a view concatenating all elements returned by a function
    * applied to each element in this $collection.
    * 
    * @param  f   the $collection-yielding function to apply to each element.
    * @return a non-strict view concatenating all elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => Iterator[B]): Iterator[B] =
    new NonStrictIteratorOps.FlatMap(these.dup, f)
  
  /** Returns a view of all elements in this $collection that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def filter(p: A => Boolean): Iterator[A] =
    new NonStrictIteratorOps.Filter(these.dup, p)
  
  /** Returns a view of all elements in this $collection that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def withFilter(p: A => Boolean): Iterator[A] =
    new NonStrictIteratorOps.Filter(these.dup, p)
  
  /** Returns a view of all elements following the longest prefix of this
    * $collection for which each element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the suffix of accumulated elements beginning
    *         with the first element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: A => Boolean): Iterator[A] =
    new NonStrictIteratorOps.DropWhile(these.dup, p)
  
  /** Returns a view of the longest prefix of this $collection for which each
    * element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the longest prefix of elements preceding
    *         the first element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: A => Boolean): Iterator[A] =
    new NonStrictIteratorOps.TakeWhile(these.dup, p)
  
  /** Returns a (prefix, suffix) pair of views with the prefix being the
    * longest one for which each element satisfies a predicate, and the suffix
    * beginning with the first element to not satisfy the predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the (predix, suffix) pair of non-strict views.
    * @group  Filtering
    */
  def span(p: A => Boolean): (Iterator[A], Iterator[A]) =
    (takeWhile(p), dropWhile(p))
  
  /** Returns a view of all elements in this $collection following a prefix
    * up to some length.
    * 
    * @param  lower   the length of the prefix to drop; also the inclusive
    *                 lower bound for indexes of included elements.
    * @return a non-strict view of all but the first `lower` elements.
    * @group  Filtering
    */
  def drop(lower: Int): Iterator[A] =
    new NonStrictIteratorOps.Drop(these.dup, lower)
  
  /** Returns a view of a prefix of this $collection up to some length.
    * 
    * @param  upper   the length of the prefix to take; also the exclusive
    *                 upper bound for indexes of included elements.
    * @return a non-strict view of up to the first `upper` elements.
    * @group  Filtering
    */
  def take(upper: Int): Iterator[A] =
    new NonStrictIteratorOps.Take(these.dup, upper)
  
  /** Returns a view of an interval of elements in this $collection.
    * 
    * @param  lower   the inclusive lower bound for indexes of included elements.
    * @param  upper   the exclusive upper bound for indexes of included elements.
    * @return a non-strict view of the elements with indexes greater than or
    *         equal to `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int): Iterator[A] =
    new NonStrictIteratorOps.Slice(these.dup, lower, upper)
  
  /** Returns a view of pairs of elemnts from this and another $collection.
    * 
    * @param  those   the $collection whose elements to lazily pair with these elements.
    * @return a non-strict view of the pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B](those: Iterator[B]): Iterator[(A, B)] =
    new NonStrictIteratorOps.Zip(these.dup, those.dup)
  
  /** Returns a view concatenating this and another $collection.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: Iterator[B]): Iterator[B] =
    new NonStrictIteratorOps.++(these.dup, those.dup)
}

private[sequential] object NonStrictIteratorOps {
  import scala.annotation.{switch, tailrec}
  import basis.util.IntOps
  
  object Done extends Iterator[Nothing] {
    override def isDone: Boolean = true
    
    override def isEmpty: Boolean = true
    
    override def head: Nothing =
      throw new NoSuchElementException("Head of empty iterator.")
    
    override def step(): Unit =
      throw new UnsupportedOperationException("Empty iterator step.")
    
    override def dup: Done.type = this
    
    override def traverse(f: Nothing => Unit): Unit = ()
  }
  
  final class Collect[-A, +B](these: Iterator[A], q: PartialFunction[A, B]) extends Iterator[B] {
    @tailrec override def isEmpty: Boolean =
      these.isEmpty || !q.isDefinedAt(these.head) && { these.step(); isEmpty }
    
    @tailrec override def head: B = {
      val x = these.head
      if (q.isDefinedAt(x)) q(x)
      else { these.step(); head }
    }
    
    override def step(): Unit = these.step()
    
    override def dup: Iterator[B] = new Collect(these.dup, q)
  }
  
  final class Map[-A, +B](these: Iterator[A], f: A => B) extends Iterator[B] {
    override def isEmpty: Boolean = these.isEmpty
    
    override def head: B = f(these.head)
    
    override def step(): Unit = these.step()
    
    override def dup: Iterator[B] = new Map(these.dup, f)
  }
  
  final class FlatMap[-A, +B] private
      (these: Iterator[A], f: A => Iterator[B], private[this] var inner: Iterator[B])
    extends Iterator[B] {
    
    def this(these: Iterator[A], f: A => Iterator[B]) = this(these, f, Done)
    
    @tailrec override def isEmpty: Boolean =
      inner.isEmpty && (these.isEmpty || { inner = f(these.head); these.step(); isEmpty })
    
    @tailrec override def head: B = {
      if (!inner.isEmpty) inner.head
      else if (!these.isEmpty) { inner = f(these.head); these.step(); head }
      else Done.head
    }
    
    @tailrec override def step() {
      if (!inner.isEmpty) inner.step()
      else if (!these.isEmpty) { inner = f(these.head); these.step(); step() }
      else Done.step()
    }
    
    override def dup: Iterator[B] = new FlatMap(these.dup, f, inner.dup)
  }
  
  final class FlatMapContainer[-A, +B] private
      (these: Iterator[A], f: A => Container[B], protected[this] var inner: Iterator[B])
    extends Iterator[B] {
    
    def this(these: Iterator[A], f: A => Container[B]) = this(these, f, Done)
    
    @tailrec override def isEmpty: Boolean =
      inner.isEmpty && (these.isEmpty || { inner = f(these.head).iterator; these.step(); isEmpty })
    
    @tailrec override def head: B = {
      if (!inner.isEmpty) inner.head
      else if (!these.isEmpty) { inner = f(these.head).iterator; these.step(); head }
      else Done.head
    }
    
    @tailrec override def step() {
      if (!inner.isEmpty) inner.step()
      else if (!these.isEmpty) { inner = f(these.head).iterator; these.step(); step() }
      else Done.step()
    }
    
    override def dup: Iterator[B] = new FlatMapContainer(these.dup, f, inner.dup)
  }
  
  final class Filter[+A](these: Iterator[A], p: A => Boolean) extends Iterator[A] {
    @tailrec override def isEmpty: Boolean =
      these.isEmpty || !p(these.head) && { these.step(); isEmpty }
    
    @tailrec override def head: A = {
      val x = these.head
      if (p(x)) x else { these.step(); head }
    }
    
    override def step(): Unit = these.step()
    
    override def dup: Iterator[A] = new Filter(these.dup, p)
  }
  
  final class DropWhile[+A] private
      (these: Iterator[A], p: A => Boolean, private[this] var dropped: Boolean)
    extends Iterator[A] {
    
    def this(these: Iterator[A], p: A => Boolean) = this(these, p, false)
    
    @tailrec override def isEmpty: Boolean =
      these.isEmpty || (!dropped && (if (p(these.head)) { these.step(); isEmpty } else { dropped = true; false }))
    
    @tailrec override def head: A = {
      if (dropped) these.head
      else {
        val x = these.head
        if (!p(x)) { dropped = true; x } else { these.step(); head }
      }
    }
    
    @tailrec override def step() {
      if (dropped) these.step()
      else if (!p(these.head)) { dropped = true; these.step() }
      else { these.step(); step() }
    }
    
    override def dup: Iterator[A] = new DropWhile(these.dup, p, dropped)
  }
  
  final class TakeWhile[+A] private
      (these: Iterator[A], p: A => Boolean, private[this] var taking: Boolean)
    extends Iterator[A] {
    
    def this(these: Iterator[A], p: A => Boolean) = this(these, p, true)
    
    override def isEmpty: Boolean =
      !taking && (these.isEmpty || !p(these.head) && { taking = false; true })
    
    @tailrec override def head: A = {
      if (taking) {
        val x = these.head
        if (p(x)) x else { taking = false; head }
      }
      else Done.head
    }
    
    @tailrec override def step() {
      if (taking) {
        if (p(these.head)) these.step()
        else { taking = false; step() }
      }
      else Done.step()
    }
    
    override def dup: Iterator[A] = new TakeWhile(these.dup, p, taking)
  }
  
  final class Drop[+A] private
      (these: Iterator[A], lower: Int, private[this] var index: Int)
    extends Iterator[A] {
    
    def this(these: Iterator[A], lower: Int) = this(these, lower, 0)
    
    @tailrec override def isEmpty: Boolean =
      these.isEmpty || index < lower && { these.step(); index += 1; isEmpty }
    
    @tailrec override def head: A = {
      if (index >= lower) these.head
      else { these.step(); index += 1; head }
    }
    
    @tailrec override def step() {
      if (index >= lower) these.step()
      else { these.step(); index += 1; step() }
    }
    
    override def dup: Iterator[A] = new Drop(these.dup, lower, index)
  }
  
  final class Take[+A] private
      (these: Iterator[A], upper: Int, private[this] var index: Int)
    extends Iterator[A] {
    
    def this(these: Iterator[A], upper: Int) = this(these, upper, 0)
    
    override def isEmpty: Boolean =
      index >= upper || these.isEmpty
    
    override def head: A = {
      if (index < upper) these.head
      else Done.head
    }
    
    override def step() {
      if (index < upper) { these.step(); index += 1 }
      else Done.step()
    }
    
    override def dup: Iterator[A] = new Take(these.dup, upper, index)
  }
  
  final class Slice[+A] private
      (these: Iterator[A], lower: Int, upper: Int, private[this] var index: Int)
    extends Iterator[A] {
    
    def this(these: Iterator[A], lower: Int, upper: Int) =
      this(these, 0 max lower, 0 max lower max upper, 0)
    
    @tailrec override def isEmpty: Boolean =
      index >= upper || these.isEmpty || index < lower && { these.step(); index += 1; isEmpty }
    
    @tailrec override def head: A = {
      if (index < lower) { these.step(); index += 1; head }
      else if (index < upper) these.head
      else Done.head
    }
    
    @tailrec override def step() {
      if (index < lower) { these.step(); index += 1; step() }
      else if (index < upper) these.step()
      else Done.step()
    }
    
    override def dup: Iterator[A] = new Slice(these.dup, lower, upper, index)
  }
  
  final class Zip[+A, +B](these: Iterator[A], those: Iterator[B]) extends Iterator[(A, B)] {
    override def isEmpty: Boolean = these.isEmpty || those.isEmpty
    
    override def head: (A, B) = (these.head, those.head)
    
    override def step() {
      these.step()
      those.step()
    }
    
    override def dup: Iterator[(A, B)] = new Zip(these.dup, those.dup)
  }
  
  final class ++[+A] private
      (these: Iterator[A], those: Iterator[A], private[this] var segment: Int)
    extends Iterator[A] {
    
    def this(these: Iterator[A], those: Iterator[A]) = this(these, those, 0)
    
    @tailrec override def isEmpty: Boolean = (segment: @switch) match {
      case 0 => these.isEmpty && { segment = 1; isEmpty }
      case 1 => those.isEmpty
    }
    
    @tailrec override def head: A = (segment: @switch) match {
      case 0 => if (!these.isEmpty) these.head else { segment = 1; head }
      case 1 => those.head
    }
    
    @tailrec override def step(): Unit = (segment: @switch) match {
      case 0 => if (!these.isEmpty) these.step() else { segment = 1; step() }
      case 1 => those.step()
    }
    
    override def dup: Iterator[A] = (segment: @switch) match {
      case 0 if !these.isEmpty => new ++(these.dup, those.dup, 0)
      case _ => those.dup
    }
  }
}
