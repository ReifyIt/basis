/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

import basis.util._

/** Strictly evaluated collection operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
class StrictCollectionOps[A, Family](val __ : Collection[A]) extends AnyVal {
  /** Returns the applications of a partial function to each element of this
    * collection for which the function is defined.
    * 
    * @param  q         the partial function to filter elements against and
    *                   to apply to applicable elements.
    * @param  builder   the implicit accumulator for mapped elements.
    * @return the accumulated elements filtered and mapped by `q`.
    * @group  Mapping
    */
  def collect[B, To](q: PartialFunction[A, B])(implicit builder: Builder[Family, B, To]): To =
    new StrictEnumeratorOps[A, Family](__).collect[B, To](q)(builder)
  
  /** Returns the applications of a function to each element of this collection.
    * 
    * @param  f         the function to apply to each element.
    * @param  builder   the implicit accumulator for mapped elements.
    * @return the accumulated elements mapped by `f`.
    * @group  Mapping
    */
  def map[B, To](f: A => B)(implicit builder: Builder[Family, B, To]): To =
    new StrictEnumeratorOps[A, Family](__).map[B, To](f)(builder)
  
  /** Returns the concatenation of all elements returned by a function applied
    * to each element of this collection.
    * 
    * @param  f         the enumerator-yielding function to apply to each element.
    * @param  builder   the implicit accumulator for flattened elements.
    * @return the concatenation of all accumulated elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B, To](f: A => Enumerator[B])(implicit builder: Builder[Family, B, To]): To =
    new StrictEnumeratorOps[A, Family](__).flatMap[B, To](f)(builder)
  
  /** Returns all elements of this collection that satisfy a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for filtered elements.
    * @return the accumulated elements filtered by `p`.
    * @group  Filtering
    */
  def filter[To](p: A => Boolean)(implicit builder: Builder[Family, A, To]): To =
    new StrictEnumeratorOps[A, Family](__).filter[To](p)(builder)
  
  /** Returns all elements following the longest prefix of this collection
    * for which each element satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for non-dropped elements.
    * @return the suffix of accumulated elements beginning with the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile[To](p: A => Boolean)(implicit builder: Builder[Family, A, To]): To = {
    traverse(__)(new StrictCollectionOps.DropWhileInto(p, builder))
    builder.state
  }
  
  /** Returns the longest prefix of this collection for which each element
    * satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for taken elements.
    * @return the longest prefix of accumulated elements preceding the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile[To](p: A => Boolean)(implicit builder: Builder[Family, A, To]): To = {
    traverse(__)(new StrictCollectionOps.TakeWhileInto(p, builder))
    builder.state
  }
  
  /** Returns a (prefix, suffix) pair with the prefix being the longest one for
    * which each element satisfies a predicate, and the suffix beginning with
    * the first element to not satisfy the predicate.
    * 
    * @param  p           the predicate to test elements against.
    * @param  builderA    the implicit accumulator for prefix elements.
    * @param  builderB    the implicit accumilator for suffix elements.
    * @return the pair of accumulated prefix and suffix elements.
    * @group  Filtering
    */
  def span[To](p: A => Boolean)(implicit builderA: Builder[Family, A, To], builderB: Builder[Family, A, To]): (To, To) = {
    traverse(__)(new StrictCollectionOps.SpanInto(p, builderA, builderB))
    (builderA.state, builderB.state)
  }
  
  /** Returns all elements of this collection following a prefix up to some length.
    * 
    * @param  lower     the length of the prefix to drop; also the inclusive
    *                   lower bound for indexes of elements to keep.
    * @param  builder   the accumulator for non-dropped elements.
    * @return all but the first `lower` accumulated elements.
    * @group  Filtering
    */
  def drop[To](lower: Int)(implicit builder: Builder[Family, A, To]): To = {
    traverse(__)(new StrictCollectionOps.DropInto(lower, builder))
    builder.state
  }
  
  /** Returns a prefix of this collection up to some length.
    * 
    * @param  upper     the length of the prefix to take; also the exclusive
    *                   upper bound for indexes of elements to keep.
    * @param  builder   the accumulator for taken elements.
    * @return up to the first `upper` accumulated elements.
    * @group  Filtering
    */
  def take[To](upper: Int)(implicit builder: Builder[Family, A, To]): To = {
    traverse(__)(new StrictCollectionOps.TakeInto(upper, builder))
    builder.state
  }
  
  /** Returns an interval of elements of this collection.
    * 
    * @param  lower     the inclusive lower bound for indexes of elements to keep.
    * @param  upper     the exclusive upper bound for indexes of elements to keep.
    * @param  builder   the accumulator for kept elements.
    * @return the accumulated elements with indexes greater than or equal to
    *         `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice[To](lower: Int, upper: Int)(implicit builder: Builder[Family, A, To]): To = {
    traverse(__)(new StrictCollectionOps.SliceInto(lower, upper, builder))
    builder.state
  }
  
  /** Returns the concatenation of this and another collection.
    * 
    * @param  that      the collection to append to this collection.
    * @param  builder   the implicit accumulator for concatenated elements.
    * @return the accumulated elements of both collections.
    * @group  Combining
    */
  def ++ [B >: A, To](that: Collection[B])(implicit builder: Builder[Family, B, To]): To =
    new StrictEnumeratorOps[B, Family](__).++[B, To](that)(builder)
}

private[sequential] object StrictCollectionOps {
  import scala.runtime.AbstractFunction1
  
  final class DropWhileInto[-A](p: A => Boolean, builder: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    private[this] var taking: Boolean = false
    override def apply(x: A): Unit = if (taking || (!p(x) && { taking = true; true })) builder += x
  }
  
  final class TakeWhileInto[-A](p: A => Boolean, builder: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) builder += x else throw Break
  }
  
  final class SpanInto[-A](p: A => Boolean, builderA: Builder[_, A, _], builderB: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    private[this] var taking: Boolean = false
    override def apply(x: A): Unit = if (!taking && (p(x) || { taking = true; false })) builderA += x else builderB += x
  }
  
  final class DropInto[-A](lower: Int, builder: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i >= lower) builder += x else i += 1
  }
  
  final class TakeInto[-A](upper: Int, builder: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < upper) { builder += x; i += 1 } else throw Break
  }
  
  final class SliceInto[-A](lower: Int, upper: Int, builder: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    private[this] var l = 0 max lower
    private[this] var u = l max upper
    private[this] var i = 0
    override def apply(x: A): Unit = if (i < u) { if (i >= l) builder += x; i += 1 } else throw Break
  }
}
