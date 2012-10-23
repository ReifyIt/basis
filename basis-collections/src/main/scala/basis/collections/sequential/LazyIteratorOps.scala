/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Nonstrictly evaluated iterator operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
class LazyIteratorOps[+A](val __ : Iterator[A]) extends AnyVal {
  /** Returns a new iterator that applies a partial function to each iterated
    * element for which the function is defined, and skips all elements where
    * the function is undefined.
    * 
    * @param  q   the partial function to filter elements against and to
    *             apply to applicable elements.
    * @return an iterator that filters and maps a dup of this iterator's elements.
    * @group  Mapping
    */
  def collect[B](q: PartialFunction[A, B]): Iterator[B] =
    new LazyIteratorOps.Collect(__.dup, q)
  
  /** Returns a new iterator that applies a function to each iterated element.
    * 
    * @param  f   the function to apply to each element.
    * @return an iterator that maps a dup of this iterator's elements.
    * @group  Mapping
    */
  def map[B](f: A => B): Iterator[B] =
    new LazyIteratorOps.Map(__.dup, f)
  
  /** Returns a new iterator that concatenates the iterators returned by a
    * function applied to each iterated element.
    * 
    * @param  f   the iterator-yielding function to apply to each element.
    * @return an iterator that concatenates a dup of this iterator's mapped elements.
    * @group  Mapping
    */
  def flatMap[B](f: A => Iterator[B]): Iterator[B] =
    new LazyIteratorOps.FlatMap(__.dup, f)
  
  /** Returns a new iterator that skips all iterated elements that don't satisfy a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return an iterator that filters a dup of this iterator's elements.
    * @group  Filtering
    */
  def filter(p: A => Boolean): Iterator[A] =
    new LazyIteratorOps.Filter(__.dup, p)
  
  /** Returns a new iterator that skips all iterated elements that don't satisfy a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return an iterator that filters a dup of this iterator's elements.
    * @group  Filtering
    */
  def withFilter(p: A => Boolean): Iterator[A] = filter(p)
  
  /** Returns a new iterator that skips the longest prefix of iterated elements
    * for which each element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return an iterator over the suffix of a dup of this iterator's elements
    *         beginning with the first element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: A => Boolean): Iterator[A] =
    new LazyIteratorOps.DropWhile(__.dup, p)
  
  /** Returns a new iterator over the longest prefix of iterated elements
    * for which each element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return an iterator over the longest prefix of a dup of this iterator's
    *         elements preceding the first element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: A => Boolean): Iterator[A] =
    new LazyIteratorOps.TakeWhile(__.dup, p)
  
  /** Returns a pair of (prefix, suffix) iterators with the first iterator
    * covering the longest prefix for which each element satisfies a predicate,
    * and the second iterator beginning with the first element to not satisfy
    * the predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the (prefix, suffix) iterator pair.
    * @group  Filtering
    */
  def span(p: A => Boolean): (Iterator[A], Iterator[A]) =
    (takeWhile(p), dropWhile(p))
  
  /** Returns a new iterator over all iterated elements following a prefix up to some length.
    * 
    * @param  lower   the length of the prefix to drop;
    *                 also the inclusive lower bound for indexes of elements to keep.
    * @return an iterator over all but the first `lower` of a dup of this iterator's elements.
    * @group  Filtering
    */
  def drop(lower: Int): Iterator[A] =
    new LazyIteratorOps.Drop(__.dup, lower)
  
  /** Returns an iterator over a prefix of iterated elements up to some length.
    * 
    * @param  upper   the length of the prefix to take;
    *                 also the exclusive upper bound for indexes of elements to keep.
    * @return an iterator over up to the first `upper` of a dup of this iterator's elements.
    * @group  Filtering
    */
  def take(upper: Int): Iterator[A] =
    new LazyIteratorOps.Take(__.dup, upper)
  
  /** Returns a new iterator over an interval of iterated elements.
    * 
    * @param  lower   the inclusive lower bound for indexes of elements to keep.
    * @param  upper   the exclusive upper bound for indexes of elements to keep.
    * @return an iterator over a dup of this iterator's elements with indexes
    *         greater than or equal to `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int): Iterator[A] =
    new LazyIteratorOps.Slice(__.dup, lower, upper)
  
  /** Returns an iterator over pairs of elements from this and another iterator.
    * 
    * @param  that  the iterator whose elements to pair with these elements.
    * @return an iterator over dups of this and that iterators' elements paired together.
    * @group  Combining
    */
  def zip[B](that: Iterator[B]): Iterator[(A, B)] =
    new LazyIteratorOps.Zip(__.dup, that.dup)
  
  /** Returns a new iterator covering both this and another iterator.
    * 
    * @param  that  the iterator to append to this iterator.
    * @return an iterator over dups of this then that iterator's elements.
    * @group  Combining
    */
  def ++ [B >: A](that: Iterator[B]): Iterator[B] =
    if (__.isEmpty) that.dup else new LazyIteratorOps.++(__.dup, that.dup)
}

private object LazyIteratorOps {
  import scala.annotation.tailrec
  
  final class Collect[-A, +B](self: Iterator[A], q: PartialFunction[A, B]) extends Iterator[B] {
    @tailrec override def isEmpty: Boolean =
      self.isEmpty || !q.isDefinedAt(self.head) || { self.step(); isEmpty }
    
    @tailrec override def head: B = {
      val x = self.head
      if (q.isDefinedAt(x)) q(x)
      else { self.step(); head }
    }
    
    override def step(): Unit = self.step()
    
    override def dup: Iterator[B] = new Collect[A, B](self.dup, q)
  }
  
  final class Map[-A, +B](self: Iterator[A], f: A => B) extends Iterator[B] {
    override def isEmpty: Boolean = self.isEmpty
    
    override def head: B = f(self.head)
    
    override def step(): Unit = self.step()
    
    override def dup: Iterator[B] = new Map[A, B](self.dup, f)
  }
  
  final class FlatMap[-A, +B] private
      (outer: Iterator[A], f: A => Iterator[B], private[this] var inner: Iterator[B])
    extends Iterator[B] {
    
    def this(outer: Iterator[A], f: A => Iterator[B]) = this(outer, f, Iterator.Empty)
    
    @tailrec override def isEmpty: Boolean =
      inner.isEmpty && (outer.isEmpty || { inner = f(outer.head); outer.step(); isEmpty })
    
    @tailrec override def head: B = {
      if (!inner.isEmpty) inner.head
      else if (!outer.isEmpty) { inner = f(outer.head); outer.step(); head }
      else Iterator.Empty.head
    }
    
    @tailrec override def step() {
      if (!inner.isEmpty) inner.step()
      else if (!outer.isEmpty) { inner = f(outer.head); outer.step(); step() }
      else Iterator.Empty.step()
    }
    
    override def dup: Iterator[B] = new FlatMap[A, B](outer.dup, f, inner.dup)
  }
  
  final class Filter[+A](self: Iterator[A], p: A => Boolean) extends Iterator[A] {
    @tailrec override def isEmpty: Boolean =
      self.isEmpty || !p(self.head) || { self.step(); isEmpty }
    
    @tailrec override def head: A = {
      val x = self.head
      if (p(x)) x
      else { self.step(); head }
    }
    
    override def step(): Unit = self.step()
    
    override def dup: Iterator[A] = new Filter[A](self.dup, p)
  }
  
  final class DropWhile[+A] private
      (self: Iterator[A], p: A => Boolean, private[this] var iterating: Boolean)
    extends Iterator[A] {
    
    def this(self: Iterator[A], p: A => Boolean) = this(self, p, false)
    
    @tailrec override def isEmpty: Boolean =
      self.isEmpty || (!iterating && (if (p(self.head)) { self.step(); isEmpty } else { iterating = true; false }))
    
    @tailrec override def head: A = {
      if (iterating) self.head
      else {
        val x = self.head
        if (p(x)) { self.step(); head }
        else { iterating = true; x }
      }
    }
    
    @tailrec override def step() {
      if (iterating) self.step()
      else if (p(self.head)) { self.step(); step() }
      else { iterating = true; self.step() }
    }
    
    override def dup: Iterator[A] = new DropWhile[A](self.dup, p, iterating)
  }
  
  final class TakeWhile[+A] private
      (self: Iterator[A], p: A => Boolean, private[this] var iterating: Boolean)
    extends Iterator[A] {
    
    def this(self: Iterator[A], p: A => Boolean) = this(self, p, true)
    
    override def isEmpty: Boolean =
      !iterating || self.isEmpty || !p(self.head) || { iterating = false; true }
    
    @tailrec override def head: A = {
      if (iterating) {
        val x = self.head
        if (p(x)) x
        else { iterating = false; head }
      }
      else Iterator.Empty.head
    }
    
    @tailrec override def step() {
      if (iterating) {
        if (p(self.head)) self.step()
        else { iterating = false; step() }
      }
      else Iterator.Empty.step()
    }
    
    override def dup: Iterator[A] = new TakeWhile[A](self.dup, p, iterating)
  }
  
  final class Drop[+A] private
      (self: Iterator[A], lower: Int, private[this] var index: Int)
    extends Iterator[A] {
    
    def this(self: Iterator[A], lower: Int) = this(self, lower, 0)
    
    @tailrec override def isEmpty: Boolean =
      self.isEmpty || (index < lower && { self.step(); index += 1; isEmpty })
    
    @tailrec override def head: A = {
      if (index < lower) { self.step(); index += 1; head }
      else self.head
    }
    
    @tailrec override def step() {
      if (index < lower) { self.step(); index += 1; step() }
      else self.step()
    }
    
    override def dup: Iterator[A] = new Drop[A](self.dup, lower, index)
  }
  
  final class Take[+A] private
      (self: Iterator[A], upper: Int, private[this] var index: Int)
    extends Iterator[A] {
    
    def this(self: Iterator[A], upper: Int) = this(self, upper, 0)
    
    override def isEmpty: Boolean =
      index >= upper || self.isEmpty
    
    override def head: A = {
      if (index < upper) self.head
      else Iterator.Empty.head
    }
    
    override def step() {
      if (index < upper) { self.step(); index += 1 }
      else Iterator.Empty.step()
    }
    
    override def dup: Iterator[A] = new Take[A](self.dup, upper, index)
  }
  
  final class Slice[+A] private
      (self: Iterator[A], lower: Int, upper: Int, private[this] var index: Int)
    extends Iterator[A] {
    
    def this(self: Iterator[A], lower: Int, upper: Int) =
      this(self, scala.math.max(0, lower), scala.math.max(scala.math.max(0, lower), upper), 0)
    
    @tailrec override def isEmpty: Boolean =
      index >= upper || self.isEmpty || (index < lower && { self.step(); index += 1; isEmpty })
    
    @tailrec override def head: A = {
      if (index < lower) { self.step(); index += 1; head }
      else if (index < upper) self.head
      else Iterator.Empty.head
    }
    
    @tailrec override def step() {
      if (index < lower) { self.step(); index += 1; step() }
      else if (index < upper) self.step()
      else Iterator.Empty.step()
    }
    
    override def dup: Iterator[A] = new Slice[A](self.dup, lower, upper, index)
  }
  
  final class Zip[+A, +B](xs: Iterator[A], ys: Iterator[B]) extends Iterator[(A, B)] {
    override def isEmpty: Boolean = xs.isEmpty || ys.isEmpty
    
    override def head: (A, B) = (xs.head, ys.head)
    
    override def step() {
      xs.step()
      ys.step()
    }
    
    override def dup: Iterator[(A, B)] = new Zip[A, B](xs.dup, ys.dup)
  }
  
  final class ++ [+A] private
      (xs: Iterator[A], ys: Iterator[A], private[this] var segment: Int)
    extends Iterator[A] {
    
    def this(xs: Iterator[A], ys: Iterator[A]) = this(xs, ys, 0)
    
    @tailrec override def isEmpty: Boolean = segment match {
      case 0 => xs.isEmpty && { segment = 1; isEmpty }
      case 1 => ys.isEmpty
    }
    
    @tailrec override def head: A = segment match {
      case 0 => if (!xs.isEmpty) xs.head else { segment = 1; head }
      case 1 => ys.head
    }
    
    @tailrec override def step(): Unit = segment match {
      case 0 => if (!xs.isEmpty) xs.step else { segment = 1; step() }
      case 1 => ys.step()
    }
    
    override def dup: Iterator[A] = segment match {
      case 0 if !xs.isEmpty => new ++ [A](xs.dup, ys.dup, 0)
      case _ => ys.dup
    }
  }
}
