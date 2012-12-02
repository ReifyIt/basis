/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential
package strict

import basis.collections._

/** Strictly evaluated enumerator operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
final class EnumeratorOps[+A, +From](val these: Enumerator[A]) extends AnyVal {
  /** Returns the applications of a partial function to each element in this
    * enumerator for which the function is defined.
    * 
    * @param  q         the partial function to filter and map elements.
    * @param  builder   the implicit accumulator for mapped elements.
    * @return the accumulated elements filtered and mapped by `q`.
    * @group  Mapping
    */
  def collect[B, To](q: PartialFunction[A, B])(implicit builder: Builder[From, B, To]): To = {
    traverse(these)(new EnumeratorOps.CollectInto(q, builder))
    builder.state
  }
  
  /** Returns the applications of a function to each element in this enumerator.
    * 
    * @param  f         the function to apply to each element.
    * @param  builder   the implicit accumulator for mapped elements.
    * @return the accumulated elements mapped by `f`.
    * @group  Mapping
    */
  def map[B, To](f: A => B)(implicit builder: Builder[From, B, To]): To = {
    traverse(these)(new EnumeratorOps.MapInto(f, builder))
    builder.state
  }
  
  /** Returns the concatenation of all elements returned by a function applied
    * to each element in this enumerator.
    * 
    * @param  f         the enumerator-yielding function to apply to each element.
    * @param  builder   the implicit accumulator for flattened elements.
    * @return the concatenation of all accumulated elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B, To](f: A => Enumerator[B])(implicit builder: Builder[From, B, To]): To = {
    traverse(these)(new EnumeratorOps.FlatMapInto(f, builder))
    builder.state
  }
  
  /** Returns all elements in this enumerator that satisfy a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for filtered elements.
    * @return the accumulated elements filtered by `p`.
    * @group  Filtering
    */
  def filter[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To = {
    traverse(these)(new EnumeratorOps.FilterInto(p, builder))
    builder.state
  }
  
  /** Returns all elements following the longest prefix of this enumerator
    * for which each element satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for non-dropped elements.
    * @return the suffix of accumulated elements beginning with the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To = {
    traverse(these)(new EnumeratorOps.DropWhileInto(p, builder))
    builder.state
  }
  
  /** Returns the longest prefix of this enumerator for which each element
    * satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for taken elements.
    * @return the longest prefix of accumulated elements preceding the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To = {
    label(traverse(these)(new EnumeratorOps.TakeWhileInto(p, builder)))
    builder.state
  }
  
  /** Returns a (prefix, suffix) pair with the prefix being the longest one for
    * which each element satisfies a predicate, and the suffix beginning with
    * the first element to not satisfy the predicate.
    * 
    * @param  p           the predicate to test elements against.
    * @param  builder1    the implicit accumulator for prefix elements.
    * @param  builder2    the implicit accumilator for suffix elements.
    * @return the pair of accumulated prefix and suffix elements.
    * @group  Filtering
    */
  def span[To](p: A => Boolean)(implicit builder1: Builder[From, A, To], builder2: Builder[From, A, To]): (To, To) = {
    traverse(these)(new EnumeratorOps.SpanInto(p, builder1, builder2))
    (builder1.state, builder2.state)
  }
  
  /** Returns all elements in this enumerator following a prefix up to some length.
    * 
    * @param  lower     the length of the prefix to drop; also the inclusive
    *                   lower bound for indexes of elements to keep.
    * @param  builder   the accumulator for non-dropped elements.
    * @return all but the first `lower` accumulated elements.
    * @group  Filtering
    */
  def drop[To](lower: Int)(implicit builder: Builder[From, A, To]): To = {
    traverse(these)(new EnumeratorOps.DropInto(lower, builder))
    builder.state
  }
  
  /** Returns a prefix of this enumerator up to some length.
    * 
    * @param  upper     the length of the prefix to take; also the exclusive
    *                   upper bound for indexes of elements to keep.
    * @param  builder   the accumulator for taken elements.
    * @return up to the first `upper` accumulated elements.
    * @group  Filtering
    */
  def take[To](upper: Int)(implicit builder: Builder[From, A, To]): To = {
    label(traverse(these)(new EnumeratorOps.TakeInto(upper, builder)))
    builder.state
  }
  
  /** Returns an interval of elements in this enumerator.
    * 
    * @param  lower     the inclusive lower bound for indexes of elements to keep.
    * @param  upper     the exclusive upper bound for indexes of elements to keep.
    * @param  builder   the accumulator for kept elements.
    * @return the accumulated elements with indexes greater than or equal to
    *         `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice[To](lower: Int, upper: Int)(implicit builder: Builder[From, A, To]): To = {
    label(traverse(these)(new EnumeratorOps.SliceInto(lower, upper, builder)))
    builder.state
  }
  
  /** Returns the concatenation of this and another enumerator.
    * 
    * @param  those     the enumerator to append to these elements.
    * @param  builder   the accumulator for concatenated elements.
    * @return the accumulated elements of both enumerators.
    * @group  Combining
    */
  def ++ [B >: A, To](those: Enumerator[B])(implicit builder: Builder[From, B, To]): To = {
    val f = new EnumeratorOps.AddInto(builder)
    traverse(these)(f)
    traverse(those)(f)
    builder.state
  }
}

private[strict] object EnumeratorOps {
  import scala.runtime.AbstractFunction1
  import basis.util.IntOps
  
  final class CollectInto[-A, B](q: PartialFunction[A, B], builder: Builder[_, B, _]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) builder += q(x)
  }
  
  final class MapInto[-A, +B](f: A => B, builder: Builder[_, B, _]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = builder += f(x)
  }
  
  final class FlatMapInto[-A, +B](f: A => Enumerator[B], builder: Builder[_, B, _]) extends AbstractFunction1[A, Unit] {
    private[this] val add = new AddInto(builder)
    override def apply(x: A): Unit = traverse(f(x))(add)
  }
  
  final class FilterInto[-A](p: A => Boolean, builder: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) builder += x
  }
  
  final class DropWhileInto[-A](p: A => Boolean, builder: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    private[this] var taking: Boolean = false
    override def apply(x: A): Unit = if (taking || (!p(x) && { taking = true; true })) builder += x
  }
  
  final class TakeWhileInto[-A](p: A => Boolean, builder: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) builder += x else label.break()
  }
  
  final class SpanInto[-A](p: A => Boolean, builder1: Builder[_, A, _], builder2: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    private[this] var taking: Boolean = false
    override def apply(x: A): Unit = if (!taking && (p(x) || { taking = true; false })) builder1 += x else builder2 += x
  }
  
  final class DropInto[-A](lower: Int, builder: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i >= lower) builder += x else i += 1
  }
  
  final class TakeInto[-A](upper: Int, builder: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < upper) { builder += x; i += 1 } else label.break()
  }
  
  final class SliceInto[-A](lower: Int, upper: Int, builder: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    private[this] var l = 0 max lower
    private[this] var u = l max upper
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < u) { if (i >= l) builder += x; i += 1 } else label.break()
  }
  
  final class AddInto[-A](builder: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = builder += x
  }
}
