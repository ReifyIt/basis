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

/** Non-strictly evaluated linear sequence operations.
  * 
  * @groupprio  Traversing  -6
  * @groupprio  Reducing    -5
  * @groupprio  Querying    -4
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  * 
  * @define collection  sequence
  */
trait LinearSeqOps[+A, +From]
  extends Any
    with SeqOps[A, From]
    with general.LinearSeqOps[A, From] {
  
  protected[this] override def self: LinearSeq[A]
  
  override def collect[B](q: PartialFunction[A, B]): LinearSeq[B] =
    new LinearSeqView.Collect(self, q)
  
  override def map[B](f: A => B): LinearSeq[B] =
    new LinearSeqView.Map(self, f)
  
  override def filter(p: A => Boolean): LinearSeq[A] =
    new LinearSeqView.Filter(self, p)
  
  override def dropWhile(p: A => Boolean): LinearSeq[A] =
    new LinearSeqView.DropWhile(self, p)
  
  override def takeWhile(p: A => Boolean): LinearSeq[A] =
    new LinearSeqView.TakeWhile(self, p)
  
  override def span(p: A => Boolean): (LinearSeq[A], LinearSeq[A]) =
    (takeWhile(p), dropWhile(p))
  
  override def drop(lower: Int): LinearSeq[A] =
    new LinearSeqView.Drop(self, lower)
  
  override def take(upper: Int): LinearSeq[A] =
    new LinearSeqView.Take(self, upper)
  
  override def slice(lower: Int, upper: Int): LinearSeq[A] =
    new LinearSeqView.Slice(self, lower, upper)
}

class LinearSeqView[+A, +From](override val self: LinearSeq[A])
  extends AnyVal with LinearSeqOps[A, From] {
  
  override def collect[B](q: PartialFunction[A, B]): LinearSeq[B] =
    new LinearSeqView.Collect(self, q)
  
  override def map[B](f: A => B): LinearSeq[B] =
    new LinearSeqView.Map(self, f)
  
  /** Returns a view concatenating all elements returned by a function
    * applied to each element in this $collection.
    * 
    * @param  f   the $collection-yielding function to apply to each element.
    * @return a non-strict view concatenating all elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => LinearSeq[B]): LinearSeq[B] =
    new LinearSeqView.FlatMap(self, f)
  
  override def filter(p: A => Boolean): LinearSeq[A] =
    new LinearSeqView.Filter(self, p)
  
  override def dropWhile(p: A => Boolean): LinearSeq[A] =
    new LinearSeqView.DropWhile(self, p)
  
  override def takeWhile(p: A => Boolean): LinearSeq[A] =
    new LinearSeqView.TakeWhile(self, p)
  
  override def span(p: A => Boolean): (LinearSeq[A], LinearSeq[A]) =
    (takeWhile(p), dropWhile(p))
  
  override def drop(lower: Int): LinearSeq[A] =
    new LinearSeqView.Drop(self, lower)
  
  override def take(upper: Int): LinearSeq[A] =
    new LinearSeqView.Take(self, upper)
  
  override def slice(lower: Int, upper: Int): LinearSeq[A] =
    new LinearSeqView.Slice(self, lower, upper)
  
  /** Returns a view of pairs of elemnts from this and another $collection.
    * 
    * @param  those   the $collection whose elements to lazily pair with these elements.
    * @return a non-strict view of the pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B](those: LinearSeq[B]): LinearSeq[(A, B)] =
    new LinearSeqView.Zip(self, those)
  
  /** Returns a view concatenating this and another $collection.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: LinearSeq[B]): LinearSeq[B] =
    new LinearSeqView.++(self, those)
}

private[nonstrict] object LinearSeqView {
  import scala.annotation.tailrec
  import basis.util.IntOps
  
  object Empty extends LinearSeq[Nothing] {
    override def isEmpty: Boolean = true
    
    override def head: Nothing = throw new NoSuchElementException
    
    override def tail: LinearSeq[Nothing] = throw new UnsupportedOperationException
  }
  
  final class Collect[-A, +B](
      private[this] var base: LinearSeq[A],
      private[this] val q: PartialFunction[A, B])
    extends LinearSeq[B] {
    
    @tailrec override def isEmpty: Boolean =
      base.isEmpty || !q.isDefinedAt(base.head) && { base = base.tail; isEmpty }
    
    @tailrec override def head: B = {
      val x = base.head
      if (q.isDefinedAt(x)) q(x)
      else { base = base.tail; head }
    }
    
    @tailrec override def tail: LinearSeq[B] = {
      if (!base.isEmpty && q.isDefinedAt(base.head)) new Collect(base.tail, q)
      else { base = base.tail; tail }
    }
  }
  
  final class Map[-A, +B](
      private[this] val base: LinearSeq[A],
      private[this] val f: A => B)
    extends LinearSeq[B] {
    
    override def isEmpty: Boolean = base.isEmpty
    
    override def head: B = f(base.head)
    
    override def tail: LinearSeq[B] = new Map(base.tail, f)
  }
  
  final class FlatMap[-A, +B] private (
      private[this] var base: LinearSeq[A],
      private[this] val f: A => LinearSeq[B],
      private[this] var inner: LinearSeq[B])
    extends LinearSeq[B] {
    
    def this(base: LinearSeq[A], f: A => LinearSeq[B]) = this(base, f, Empty)
    
    @tailrec override def isEmpty: Boolean =
      inner.isEmpty && (base.isEmpty || { inner = f(base.head); base = base.tail; isEmpty })
    
    @tailrec override def head: B = {
      if (!inner.isEmpty) inner.head
      else if (!base.isEmpty) { inner = f(base.head); base = base.tail; head }
      else Empty.head
    }
    
    override def tail: LinearSeq[B] = {
      if (!inner.isEmpty) new FlatMap(base, f, inner.tail)
      else if (!base.isEmpty) new FlatMap(base.tail, f, f(base.head))
      else Empty.tail
    }
  }
  
  final class Filter[+A](
      private[this] var base: LinearSeq[A],
      private[this] val p: A => Boolean)
    extends LinearSeq[A] {
    
    @tailrec override def isEmpty: Boolean =
      base.isEmpty || !p(base.head) && { base = base.tail; isEmpty }
    
    @tailrec override def head: A = {
      val x = base.head
      if (p(x)) x else { base = base.tail; head }
    }
    
    @tailrec override def tail: LinearSeq[A] = {
      if (!base.isEmpty && p(base.head)) new Filter(base.tail, p)
      else { base = base.tail; tail }
    }
  }
  
  final class DropWhile[+A](
      private[this] var base: LinearSeq[A],
      private[this] val p: A => Boolean)
    extends LinearSeq[A] {
    
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
    
    @tailrec override def tail: LinearSeq[A] = {
      if (dropped) base.tail
      else if (!p(base.head)) { dropped = true; tail }
      else { base = base.tail; tail }
    }
  }
  
  final class TakeWhile[+A](
      private[this] val base: LinearSeq[A],
      private[this] val p: A => Boolean)
    extends LinearSeq[A] {
    
    private[this] lazy val taking: Boolean = !base.isEmpty && p(base.head)
    
    override def isEmpty: Boolean = !taking
    
    override def head: A = {
      if (taking) base.head
      else Empty.head
    }
    
    override def tail: LinearSeq[A] = {
      if (taking) new TakeWhile(base.tail, p)
      else Empty.tail
    }
  }
  
  final class Drop[+A] private (
      private[this] var base: LinearSeq[A],
      private[this] val lower: Int,
      private[this] var index: Int)
    extends LinearSeq[A] {
    
    def this(base: LinearSeq[A], lower: Int) = this(base, lower, 0)
    
    @tailrec override def isEmpty: Boolean =
      base.isEmpty || index < lower && { base = base.tail; index += 1; isEmpty }
    
    @tailrec override def head: A = {
      if (index >= lower) base.head
      else { base = base.tail; index += 1; head }
    }
    
    @tailrec override def tail: LinearSeq[A] = {
      if (index >= lower) base.tail
      else { base = base.tail; index += 1; tail }
    }
  }
  
  final class Take[+A] private (
      private[this] val base: LinearSeq[A],
      private[this] val upper: Int,
      private[this] val index: Int)
    extends LinearSeq[A] {
    
    def this(base: LinearSeq[A], upper: Int) = this(base, upper, 0)
    
    override def isEmpty: Boolean =
      index >= upper || base.isEmpty
    
    override def head: A = {
      if (index < upper) base.head
      else Empty.head
    }
    
    override def tail: LinearSeq[A] = {
      if (index < upper) new Take(base.tail, upper, index + 1)
      else Empty.tail
    }
  }
  
  final class Slice[+A] private (
      private[this] var base: LinearSeq[A],
      private[this] val lower: Int,
      private[this] val upper: Int,
      private[this] var index: Int)
    extends LinearSeq[A] {
    
    def this(base: LinearSeq[A], lower: Int, upper: Int) =
      this(base,0 max lower, 0 max lower max upper, 0)
    
    @tailrec override def isEmpty: Boolean =
      index >= upper || base.isEmpty || index < lower && { base = base.tail; index += 1; isEmpty }
    
    @tailrec override def head: A = {
      if (index < lower) { base = base.tail; index += 1; head }
      else if (index < upper) base.head
      else Empty.head
    }
    
    @tailrec override def tail: LinearSeq[A] = {
      if (index < lower) { base = base.tail; index += 1; tail }
      else if (index < upper) new Slice(base.tail, lower, upper, index + 1)
      else Empty.tail
    }
  }
  
  final class Zip[+A, +B](
      private[this] val xs: LinearSeq[A],
      private[this] val ys: LinearSeq[B])
    extends LinearSeq[(A, B)] {
    
    override def isEmpty: Boolean = xs.isEmpty || ys.isEmpty
    
    override def head: (A, B) = (xs.head, ys.head)
    
    override def tail: LinearSeq[(A, B)] = new Zip(xs.tail, ys.tail)
  }
  
  final class ++[+A] private (
      private[this] val xs: LinearSeq[A],
      private[this] val ys: LinearSeq[A],
      private[this] var segment: Int)
    extends LinearSeq[A] {
    
    def this(xs: LinearSeq[A], ys: LinearSeq[A]) = this(xs, ys, 0)
    
    @tailrec override def isEmpty: Boolean = segment match {
      case 0 => xs.isEmpty && { segment = 1; isEmpty }
      case 1 => ys.isEmpty
    }
    
    @tailrec override def head: A = segment match {
      case 0 => if (!xs.isEmpty) xs.head else { segment = 1; head }
      case 1 => ys.head
    }
    
    override def tail: LinearSeq[A] = segment match {
      case 0 if !xs.isEmpty => new ++(xs.tail, ys, 0)
      case _ => ys.tail
    }
  }
}
