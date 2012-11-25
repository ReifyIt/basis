/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential
package nonstrict

import traversable._

/** Non-strictly evaluated iterator operations.
  * 
  * @groupprio  Traversing  -6
  * @groupprio  Reducing    -5
  * @groupprio  Querying    -4
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  * 
  * @define collection  iterator
  */
trait IteratorOps[+A, +From]
  extends Any
    with EnumeratorOps[A, From]
    with general.IteratorOps[A, From] {
  
  protected[this] override def self: Iterator[A]
  
  override def collect[B](q: PartialFunction[A, B]): Iterator[B] =
    new IteratorView.Collect(self, q)
  
  override def map[B](f: A => B): Iterator[B] =
    new IteratorView.Map(self, f)
  
  override def filter(p: A => Boolean): Iterator[A] =
    new IteratorView.Filter(self, p)
  
  override def dropWhile(p: A => Boolean): Iterator[A] =
    new IteratorView.DropWhile(self, p)
  
  override def takeWhile(p: A => Boolean): Iterator[A] =
    new IteratorView.TakeWhile(self, p)
  
  override def span(p: A => Boolean): (Iterator[A], Iterator[A]) =
    (takeWhile(p), dropWhile(p))
  
  override def drop(lower: Int): Iterator[A] =
    new IteratorView.Drop(self, lower)
  
  override def take(upper: Int): Iterator[A] =
    new IteratorView.Take(self, upper)
  
  override def slice(lower: Int, upper: Int): Iterator[A] =
    new IteratorView.Slice(self, lower, upper)
}

class IteratorView[+A, +From](override val self: Iterator[A])
  extends AnyVal with IteratorOps[A, From] {
  
  override def collect[B](q: PartialFunction[A, B]): Iterator[B] =
    new IteratorView.Collect(self, q)
  
  override def map[B](f: A => B): Iterator[B] =
    new IteratorView.Map(self, f)
  
  /** Returns a view concatenating all elements returned by a function
    * applied to each element in this $collection.
    * 
    * @param  f   the $collection-yielding function to apply to each element.
    * @return a non-strict view concatenating all elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => Iterator[B]): Iterator[B] =
    new IteratorView.FlatMap(self, f)
  
  override def filter(p: A => Boolean): Iterator[A] =
    new IteratorView.Filter(self, p)
  
  override def dropWhile(p: A => Boolean): Iterator[A] =
    new IteratorView.DropWhile(self, p)
  
  override def takeWhile(p: A => Boolean): Iterator[A] =
    new IteratorView.TakeWhile(self, p)
  
  override def span(p: A => Boolean): (Iterator[A], Iterator[A]) =
    (takeWhile(p), dropWhile(p))
  
  override def drop(lower: Int): Iterator[A] =
    new IteratorView.Drop(self, lower)
  
  override def take(upper: Int): Iterator[A] =
    new IteratorView.Take(self, upper)
  
  override def slice(lower: Int, upper: Int): Iterator[A] =
    new IteratorView.Slice(self, lower, upper)
  
  /** Returns a view of pairs of elemnts from this and another $collection.
    * 
    * @param  those   the $collection whose elements to lazily pair with these elements.
    * @return a non-strict view of the pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B](those: Iterator[B]): Iterator[(A, B)] =
    new IteratorView.Zip(self, those)
  
  /** Returns a view concatenating this and another $collection.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: Iterator[B]): Iterator[B] =
    new IteratorView.++(self, those)
}

private[nonstrict] object IteratorView {
  import scala.annotation.tailrec
  import basis.util.IntOps
  
  final class Collect[-A, +B](
      protected[this] override val base: Iterator[A],
      protected[this] override val q: PartialFunction[A, B])
    extends EnumeratorView.Collect[A, B](base, q) with Iterator[B] {
    
    @tailrec override def isEmpty: Boolean =
      base.isEmpty || !q.isDefinedAt(base.head) && { base.step(); isEmpty }
    
    @tailrec override def head: B = {
      val x = base.head
      if (q.isDefinedAt(x)) q(x)
      else { base.step(); head }
    }
    
    override def step(): Unit = base.step()
    
    override def dup: Iterator[B] = new Collect(base.dup, q)
  }
  
  final class Map[-A, +B](
      protected[this] override val base: Iterator[A],
      protected[this] override val f: A => B)
    extends EnumeratorView.Map[A, B](base, f) with Iterator[B] {
    
    override def isEmpty: Boolean = base.isEmpty
    
    override def head: B = f(base.head)
    
    override def step(): Unit = base.step()
    
    override def dup: Iterator[B] = new Map(base.dup, f)
  }
  
  final class FlatMap[-A, +B] private (
      protected[this] override val base: Iterator[A],
      protected[this] override val f: A => Iterator[B],
      private[this] var inner: Iterator[B])
    extends EnumeratorView.FlatMap[A, B](base, f) with Iterator[B] {
    
    def this(base: Iterator[A], f: A => Iterator[B]) = this(base, f, Done)
    
    @tailrec override def isEmpty: Boolean =
      inner.isEmpty && (base.isEmpty || { inner = f(base.head); base.step(); isEmpty })
    
    @tailrec override def head: B = {
      if (!inner.isEmpty) inner.head
      else if (!base.isEmpty) { inner = f(base.head); base.step(); head }
      else Done.head
    }
    
    @tailrec override def step() {
      if (!inner.isEmpty) inner.step()
      else if (!base.isEmpty) { inner = f(base.head); base.step(); step() }
      else Done.step()
    }
    
    override def dup: Iterator[B] = new FlatMap(base.dup, f, inner.dup)
  }
  
  final class FlatMapContainer[-A, +B] private (
      protected[this] override val base: Iterator[A],
      protected[this] override val f: A => Container[B],
      protected[this] var inner: Iterator[B])
    extends EnumeratorView.FlatMap[A, B](base, f) with Iterator[B] {
    
    def this(base: Iterator[A], f: A => Container[B]) = this(base, f, Done)
    
    @tailrec override def isEmpty: Boolean =
      inner.isEmpty && (base.isEmpty || { inner = f(base.head).iterator; base.step(); isEmpty })
    
    @tailrec override def head: B = {
      if (!inner.isEmpty) inner.head
      else if (!base.isEmpty) { inner = f(base.head).iterator; base.step(); head }
      else Done.head
    }
    
    @tailrec override def step() {
      if (!inner.isEmpty) inner.step()
      else if (!base.isEmpty) { inner = f(base.head).iterator; base.step(); step() }
      else Done.step()
    }
    
    override def dup: Iterator[B] = new FlatMapContainer(base.dup, f, inner.dup)
  }
  
  final class Filter[+A](
      protected[this] override val base: Iterator[A],
      protected[this] override val p: A => Boolean)
    extends EnumeratorView.Filter[A](base, p) with Iterator[A] {
    
    @tailrec override def isEmpty: Boolean =
      base.isEmpty || !p(base.head) && { base.step(); isEmpty }
    
    @tailrec override def head: A = {
      val x = base.head
      if (p(x)) x else { base.step(); head }
    }
    
    override def step(): Unit = base.step()
    
    override def dup: Iterator[A] = new Filter(base.dup, p)
  }
  
  final class DropWhile[+A] private (
      protected[this] override val base: Iterator[A],
      protected[this] override val p: A => Boolean,
      private[this] var dropped: Boolean)
    extends EnumeratorView.DropWhile[A](base, p) with Iterator[A] {
    
    def this(base: Iterator[A], p: A => Boolean) = this(base, p, false)
    
    @tailrec override def isEmpty: Boolean =
      base.isEmpty || (!dropped && (if (p(base.head)) { base.step(); isEmpty } else { dropped = true; false }))
    
    @tailrec override def head: A = {
      if (dropped) base.head
      else {
        val x = base.head
        if (!p(x)) { dropped = true; x } else { base.step(); head }
      }
    }
    
    @tailrec override def step() {
      if (dropped) base.step()
      else if (!p(base.head)) { dropped = true; base.step() }
      else { base.step(); step() }
    }
    
    override def dup: Iterator[A] = new DropWhile(base.dup, p, dropped)
  }
  
  final class TakeWhile[+A] private (
      protected[this] override val base: Iterator[A],
      protected[this] override val p: A => Boolean,
      private[this] var taking: Boolean)
    extends EnumeratorView.TakeWhile[A](base, p) with Iterator[A] {
    
    def this(base: Iterator[A], p: A => Boolean) = this(base, p, true)
    
    override def isEmpty: Boolean =
      !taking && (base.isEmpty || !p(base.head) && { taking = false; true })
    
    @tailrec override def head: A = {
      if (taking) {
        val x = base.head
        if (p(x)) x else { taking = false; head }
      }
      else Done.head
    }
    
    @tailrec override def step() {
      if (taking) {
        if (p(base.head)) base.step()
        else { taking = false; step() }
      }
      else Done.step()
    }
    
    override def dup: Iterator[A] = new TakeWhile(base.dup, p, taking)
  }
  
  final class Drop[+A] private (
      protected[this] override val base: Iterator[A],
      protected[this] override val lower: Int,
      private[this] var index: Int)
    extends EnumeratorView.Drop[A](base, lower) with Iterator[A] {
    
    def this(base: Iterator[A], lower: Int) = this(base, lower, 0)
    
    @tailrec override def isEmpty: Boolean =
      base.isEmpty || index < lower && { base.step(); index += 1; isEmpty }
    
    @tailrec override def head: A = {
      if (index >= lower) base.head
      else { base.step(); index += 1; head }
    }
    
    @tailrec override def step() {
      if (index >= lower) base.step()
      else { base.step(); index += 1; step() }
    }
    
    override def dup: Iterator[A] = new Drop(base.dup, lower, index)
  }
  
  final class Take[+A] private (
      protected[this] override val base: Iterator[A],
      protected[this] override val upper: Int,
      private[this] var index: Int)
    extends EnumeratorView.Take[A](base, upper) with Iterator[A] {
    
    def this(base: Iterator[A], upper: Int) = this(base, upper, 0)
    
    override def isEmpty: Boolean =
      index >= upper || base.isEmpty
    
    override def head: A = {
      if (index < upper) base.head
      else Done.head
    }
    
    override def step() {
      if (index < upper) { base.step(); index += 1 }
      else Done.step()
    }
    
    override def dup: Iterator[A] = new Take(base.dup, upper, index)
  }
  
  final class Slice[+A] private (
      protected[this] override val base: Iterator[A],
      protected[this] override val lower: Int,
      protected[this] override val upper: Int,
      private[this] var index: Int)
    extends EnumeratorView.Slice[A](base, lower, upper) with Iterator[A] {
    
    def this(base: Iterator[A], lower: Int, upper: Int) =
      this(base, 0 max lower, 0 max lower max upper, 0)
    
    @tailrec override def isEmpty: Boolean =
      index >= upper || base.isEmpty || index < lower && { base.step(); index += 1; isEmpty }
    
    @tailrec override def head: A = {
      if (index < lower) { base.step(); index += 1; head }
      else if (index < upper) base.head
      else Done.head
    }
    
    @tailrec override def step() {
      if (index < lower) { base.step(); index += 1; step() }
      else if (index < upper) base.step()
      else Done.step()
    }
    
    override def dup: Iterator[A] = new Slice(base.dup, lower, upper, index)
  }
  
  final class Zip[+A, +B](xs: Iterator[A], ys: Iterator[B]) extends Iterator[(A, B)] {
    override def isEmpty: Boolean = xs.isEmpty || ys.isEmpty
    
    override def head: (A, B) = (xs.head, ys.head)
    
    override def step() {
      xs.step()
      ys.step()
    }
    
    override def dup: Iterator[(A, B)] = new Zip(xs.dup, ys.dup)
  }
  
  final class ++[+A] private (
      protected[this] override val xs: Iterator[A],
      protected[this] override val ys: Iterator[A],
      private[this] var segment: Int)
    extends EnumeratorView.++[A](xs, ys) with Iterator[A] {
    
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
      case 0 => if (!xs.isEmpty) xs.step() else { segment = 1; step() }
      case 1 => ys.step()
    }
    
    override def dup: Iterator[A] = segment match {
      case 0 if !xs.isEmpty => new ++(xs.dup, ys.dup, 0)
      case _ => ys.dup
    }
  }
}
