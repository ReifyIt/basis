/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Strictly evaluated collection operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
class StrictCollectionOps[+Self, +A](val __ : Collection[A]) extends AnyVal {
  /** Returns the applications of a partial function to each element of this
    * collection for which the function is defined.
    * 
    * @param  q       the partial function to filter elements against and to
    *                 apply to applicable elements.
    * @param  buffer  the implicit accumulator for mapped elements.
    * @return the accumulated elements filtered and mapped by `q`.
    * @group  Mapping
    */
  def collect[B](q: PartialFunction[A, B])(implicit buffer: Buffer[Self, B]): buffer.State =
    new StrictEnumeratorOps[Self, A](__).collect[B](q)(buffer)
  
  /** Returns the applications of a function to each element of this collection.
    * 
    * @param  f       the function to apply to each element.
    * @param  buffer  the implicit accumulator for mapped elements.
    * @return the accumulated elements mapped by `f`.
    * @group  Mapping
    */
  def map[B](f: A => B)(implicit buffer: Buffer[Self, B]): buffer.State =
    new StrictEnumeratorOps[Self, A](__).map[B](f)(buffer)
  
  /** Returns the concatenation of all elements returned by a function applied
    * to each element of this collection.
    * 
    * @param  f       the enumerator-yielding function to apply to each element.
    * @param  buffer  the implicit accumulator for flattened elements.
    * @return the concatenation of all accumulated elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => Enumerator[B])(implicit buffer: Buffer[Self, B]): buffer.State =
    new StrictEnumeratorOps[Self, A](__).flatMap[B](f)(buffer)
  
  /** Returns all elements of this collection that satisfy a predicate.
    * 
    * @param  p       the predicate to test elements against.
    * @param  buffer  the implicit accumulator for filtered elements.
    * @return the accumulated elements filtered by `p`.
    * @group  Filtering
    */
  def filter(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State =
    new StrictEnumeratorOps[Self, A](__).filter(p)(buffer)
  
  /** Returns all elements following the longest prefix of this collection
    * for which each element satisfies a predicate.
    * 
    * @param  p       the predicate to test elements against.
    * @param  buffer  the implicit accumulator for non-dropped elements.
    * @return the suffix of accumulated elements beginning with the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(__)(new StrictCollectionOps.DropWhileInto(p, buffer))
    buffer.state
  }
  
  /** Returns the longest prefix of this collection for which each element
    * satisfies a predicate.
    * 
    * @param  p       the predicate to test elements against.
    * @param  buffer  the implicit accumulator for taken elements.
    * @return the longest prefix of accumulated elements preceding the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(__)(new StrictCollectionOps.TakeWhileInto(p, buffer))
    buffer.state
  }
  
  /** Returns a (prefix, suffix) pair with the prefix being the longest one for
    * which each element satisfies a predicate, and the suffix beginning with
    * the first element to not satisfy the predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  bufferA   the implicit accumulator for prefix elements.
    * @param  bufferB   the implicit accumilator for suffix elements.
    * @return the pair of accumulated prefix and suffix elements.
    * @group  Filtering
    */
  def span(p: A => Boolean)(implicit bufferA: Buffer[Self, A], bufferB: Buffer[Self, A]): (bufferA.State, bufferB.State) = {
    traverse(__)(new StrictCollectionOps.SpanInto(p, bufferA, bufferB))
    (bufferA.state, bufferB.state)
  }
  
  /** Returns all elements of this collection following a prefix up to some length.
    * 
    * @param  lower   the length of the prefix to drop;
    *                 also the inclusive lower bound for indexes of elements to keep.
    * @param  buffer  the accumulator for non-dropped elements.
    * @return all but the first `lower` accumulated elements.
    * @group  Filtering
    */
  def drop(lower: Int)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(__)(new StrictCollectionOps.DropInto(lower, buffer))
    buffer.state
  }
  
  /** Returns a prefix of this collection up to some length.
    * 
    * @param  upper   the length of the prefix to take;
    *                 also the exclusive upper bound for indexes of elements to keep.
    * @param  buffer  the accumulator for taken elements.
    * @return up to the first `upper` accumulated elements.
    * @group  Filtering
    */
  def take(upper: Int)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(__)(new StrictCollectionOps.TakeInto(upper, buffer))
    buffer.state
  }
  
  /** Returns an interval of elements of this collection.
    * 
    * @param  lower   the inclusive lower bound for indexes of elements to keep.
    * @param  upper   the exclusive upper bound for indexes of elements to keep.
    * @param  buffer  the accumulator for kept elements.
    * @return the accumulated elements with indexes greater than or equal to
    *         `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(__)(new StrictCollectionOps.SliceInto(lower, upper, buffer))
    buffer.state
  }
  
  /** Returns the concatenation of this and another collection.
    * 
    * @param  that    the collection to append to this collection.
    * @param  buffer  the implicit accumulator for concatenated elements.
    * @return the accumulated elements of both collections.
    * @group  Combining
    */
  def ++ [B >: A](that: Collection[B])(implicit buffer: Buffer[Self, B]): buffer.State =
    new StrictEnumeratorOps[Self, B](__).++[B](that)(buffer)
}

private[sequential] object StrictCollectionOps {
  import scala.runtime.AbstractFunction1
  
  final class DropWhileInto[-A](p: A => Boolean, buffer: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    private[this] var taking: Boolean = false
    override def apply(x: A): Unit = if (taking || (!p(x) && { taking = true; true })) buffer += x
  }
  
  final class TakeWhileInto[-A](p: A => Boolean, buffer: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) buffer += x else throw Break
  }
  
  final class SpanInto[-A](p: A => Boolean, bufferA: Buffer[_, A], bufferB: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    private[this] var taking: Boolean = false
    override def apply(x: A): Unit = if (!taking && (p(x) || { taking = true; false })) bufferA += x else bufferB += x
  }
  
  final class DropInto[-A](lower: Int, buffer: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i >= lower) buffer += x else i += 1
  }
  
  final class TakeInto[-A](upper: Int, buffer: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < upper) { buffer += x; i += 1 } else throw Break
  }
  
  final class SliceInto[-A](lower: Int, upper: Int, buffer: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    private[this] var l = scala.math.max(0, lower)
    private[this] var u = scala.math.max(l, upper)
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < u) { if (i >= l) buffer += x; i += 1 } else throw Break
  }
}
