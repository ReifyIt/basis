/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

/** Non-strictly evaluated enumerator operations.
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
  * @define collection  enumerator
  */
final class NonStrictEnumeratorOps[+A](val these: Enumerator[A]) extends AnyVal {
  /** Returns a view that applies a partial function to each element in this
    * $collection for which the function is defined.
    * 
    * @param  q   the partial function to lazily filter and map elements.
    * @return a non-strict view of the filtered and mapped elements.
    * @group  Mapping
    */
  def collect[B](q: PartialFunction[A, B]): Enumerator[B] =
    new NonStrictEnumeratorOps.Collect(these, q)
  
  /** Returns a view that applies a function to each element in this $collection.
    * 
    * @param  f   the function to lazily apply to each element.
    * @return a non-strict view of the mapped elements.
    * @group  Mapping
    */
  def map[B](f: A => B): Enumerator[B] =
    new NonStrictEnumeratorOps.Map(these, f)
  
  /** Returns a view concatenating all elements returned by a function
    * applied to each element in this $collection.
    * 
    * @param  f   the $collection-yielding function to apply to each element.
    * @return a non-strict view concatenating all elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => Enumerator[B]): Enumerator[B] =
    new NonStrictEnumeratorOps.FlatMap(these, f)
  
  /** Returns a view of all elements in this $collection that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def filter(p: A => Boolean): Enumerator[A] =
    new NonStrictEnumeratorOps.Filter(these, p)
  
  /** Returns a view of all elements in this $collection that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def withFilter(p: A => Boolean): Enumerator[A] =
    new NonStrictEnumeratorOps.Filter(these, p)
  
  /** Returns a view of all elements following the longest prefix of this
    * $collection for which each element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the suffix of accumulated elements beginning
    *         with the first element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: A => Boolean): Enumerator[A] =
    new NonStrictEnumeratorOps.DropWhile(these, p)
  
  /** Returns a view of the longest prefix of this $collection for which each
    * element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the longest prefix of elements preceding
    *         the first element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: A => Boolean): Enumerator[A] =
    new NonStrictEnumeratorOps.TakeWhile(these, p)
  
  /** Returns a (prefix, suffix) pair of views with the prefix being the
    * longest one for which each element satisfies a predicate, and the suffix
    * beginning with the first element to not satisfy the predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the (predix, suffix) pair of non-strict views.
    * @group  Filtering
    */
  def span(p: A => Boolean): (Enumerator[A], Enumerator[A]) =
    (takeWhile(p), dropWhile(p))
  
  /** Returns a view of all elements in this $collection following a prefix
    * up to some length.
    * 
    * @param  lower   the length of the prefix to drop; also the inclusive
    *                 lower bound for indexes of included elements.
    * @return a non-strict view of all but the first `lower` elements.
    * @group  Filtering
    */
  def drop(lower: Int): Enumerator[A] =
    new NonStrictEnumeratorOps.Drop(these, lower)
  
  /** Returns a view of a prefix of this $collection up to some length.
    * 
    * @param  upper   the length of the prefix to take; also the exclusive
    *                 upper bound for indexes of included elements.
    * @return a non-strict view of up to the first `upper` elements.
    * @group  Filtering
    */
  def take(upper: Int): Enumerator[A] =
    new NonStrictEnumeratorOps.Take(these, upper)
  
  /** Returns a view of an interval of elements in this $collection.
    * 
    * @param  lower   the inclusive lower bound for indexes of included elements.
    * @param  upper   the exclusive upper bound for indexes of included elements.
    * @return a non-strict view of the elements with indexes greater than or
    *         equal to `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int): Enumerator[A] =
    new NonStrictEnumeratorOps.Slice(these, lower, upper)
  
  /** Returns a view concatenating this and another $collection.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: Enumerator[B]): Enumerator[B] =
    new NonStrictEnumeratorOps.++(these, those)
}

private[sequential] object NonStrictEnumeratorOps {
  import scala.runtime.AbstractFunction1
  import basis.util.IntOps
  
  class Collect[-A, +B](
      protected[this] val these: Enumerator[A],
      protected[this] val q: PartialFunction[A, B])
    extends Enumerator[B] {
    
    override def traverse(g: B => Unit): Unit =
      these traverse new CollectTraverse(q, g)
  }
  
  final class CollectTraverse[-A, +B](q: PartialFunction[A, B], g: B => Unit) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) g(q(x))
  }
  
  class Map[-A, +B](
      protected[this] val these: Enumerator[A],
      protected[this] val f: A => B)
    extends Enumerator[B] {
    
    override def traverse(g: B => Unit): Unit =
      these traverse (f andThen g)
  }
  
  class FlatMap[-A, +B](
      protected[this] val these: Enumerator[A],
      protected[this] val f: A => Enumerator[B])
    extends Enumerator[B] {
    
    override def traverse(g: B => Unit): Unit =
      these traverse new FlatMapTraverse(f, g)
  }
  
  final class FlatMapTraverse[-A, +B](f: A => Enumerator[B], g: B => Unit) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = f(x) traverse g
  }
  
  class Filter[+A](
      protected[this] val these: Enumerator[A],
      protected[this] val p: A => Boolean)
    extends Enumerator[A] {
    
    override def traverse(g: A => Unit): Unit =
      these traverse new FilterTraverse(p, g)
  }
  
  final class FilterTraverse[-A](p: A => Boolean, g: A => Unit) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) g(x)
  }
  
  class DropWhile[+A](
      protected[this] val these: Enumerator[A],
      protected[this] val p: A => Boolean)
    extends Enumerator[A] {
    
    override def traverse(g: A => Unit): Unit =
      these traverse new DropWhileTraverse(p, g)
  }
  
  final class DropWhileTraverse[-A](p: A => Boolean, g: A => Unit) extends AbstractFunction1[A, Unit] {
    private[this] var split = false
    override def apply(x: A): Unit = if (split || (!p(x) && { split = true; true })) g(x)
  }
  
  class TakeWhile[+A](
      protected[this] val these: Enumerator[A],
      protected[this] val p: A => Boolean)
    extends Enumerator[A] {
    
    override def traverse(g: A => Unit): Unit =
      begin(these traverse new TakeWhileTraverse(p, g))
  }
  
  final class TakeWhileTraverse[-A](p: A => Boolean, g: A => Unit) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) g(x) else begin.break()
  }
  
  class Drop[+A](
      protected[this] val these: Enumerator[A],
      protected[this] val lower: Int)
    extends Enumerator[A] {
    
    override def traverse(g: A => Unit): Unit =
      these traverse new DropTraverse(lower, g)
  }
  
  final class DropTraverse[-A](l: Int, g: A => Unit) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i >= l) g(x) else i += 1
  }
  
  class Take[+A](
      protected[this] val these: Enumerator[A],
      protected[this] val upper: Int)
    extends Enumerator[A] {
    
    override def traverse(g: A => Unit): Unit =
      begin(these traverse new TakeTraverse(upper, g))
  }
  
  final class TakeTraverse[-A](u: Int, g: A => Unit) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < u) { g(x); i += 1 } else begin.break()
  }
  
  class Slice[+A](
      protected[this] val these: Enumerator[A],
      protected[this] val lower: Int,
      protected[this] val upper: Int)
    extends Enumerator[A] {
    
    override def traverse(g: A => Unit) {
      var l = 0 max lower
      val u = l max upper
      if (l < u) begin(these traverse new SliceTraverse(l, u, g))
    }
  }
  
  final class SliceTraverse[-A](l: Int, u: Int, g: A => Unit) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < u) { if (i >= l) g(x); i += 1 } else begin.break()
  }
  
  class ++[+A](
      protected[this] val these: Enumerator[A],
      protected[this] val those: Enumerator[A])
    extends Enumerator[A] {
    
    override def traverse(g: A => Unit) {
      these traverse g
      those traverse g
    }
  }
}
