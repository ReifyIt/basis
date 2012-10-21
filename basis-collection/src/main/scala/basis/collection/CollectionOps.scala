/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

/** Collection analyses and strict transformations.
  * 
  * @groupprio  Traversing    -6
  * @groupprio  Folding       -5
  * @groupprio  Querying      -4
  * @groupprio  Transforming  -3
  * @groupprio  Dividing      -2
  * @groupprio  Expanding     -1
  */
class CollectionOps[+Self, +A](val __ : Collection[A]) extends AnyVal {
  import Enumerator.traverse
  
  /** Sequentially applies a function to each element of this collection.
    * 
    * @param  f   the function to apply to each element.
    * @group  Traversing
    */
  def foreach[U](f: A => U): Unit = traverse(__)(f)
  
  /** Returns the repeated application of an associative binary operator
    * between an identity value and all elements of this collection.
    * 
    * @param  z   the operator's identity element.
    * @param  op  the associative binary operator to apply.
    * @return the folded value.
    * @group  Folding
    */
  def fold[B >: A](z: B)(op: (B, B) => B): B = {
    val f = new Traversers.FoldLeft(z)(op)
    traverse(__)(f)
    f.state
  }
  
  /** Returns the repeated application of an associative binary operator
    * between all elements of this non-empty collection.
    * 
    * @param  op  the associative binary operator to apply.
    * @return the reduced value.
    * @group  Folding
    */
  def reduce[B >: A](op: (B, B) => B): B = {
    val f = new Traversers.ReduceLeft(op)
    traverse(__)(f)
    if (f.isDefined) f.state else throw new java.lang.UnsupportedOperationException
  }
  
  /** Returns the repeated application of an associative binary operator
    * between all elements of this collection.
    * 
    * @param  op  the associative binary operator to apply.
    * @return some reduced value, or none if the collection is empty.
    * @group  Folding
    */
  def reduceOption[B >: A](op: (B, B) => B): Option[B] = {
    val f = new Traversers.ReduceLeft(op)
    traverse(__)(f)
    if (f.isDefined) Some(f.state) else None
  }
  
  /** Returns the left-to-right application of a binary operator between a
    * start value and all elements of this collection.
    * 
    * @param  z   the starting value.
    * @param  op  the binary operator to apply right-recursively.
    * @return the folded value.
    * @group  Folding
    */
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    val f = new Traversers.FoldLeft(z)(op)
    traverse(__)(f)
    f.state
  }
  
  /** Returns the left-to-right application of a binary operator between
    * all elements of this non-empty collection.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return the reduced value.
    * @group  Folding
    */
  def reduceLeft[B >: A](op: (B, A) => B): B = {
    val f = new Traversers.ReduceLeft(op.asInstanceOf[(B, B) => B]) // work around typer bug
    traverse(__)(f)
    if (f.isDefined) f.state else throw new java.lang.UnsupportedOperationException
  }
  
  /** Returns the left-to-right application of a binary operator between
    * all elements of this collection.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return some reduced value, or none if the collection is empty.
    * @group  Folding
    */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = {
    val f = new Traversers.ReduceLeft(op.asInstanceOf[(B, B) => B]) // work around typer bug
    traverse(__)(f)
    if (f.isDefined) Some(f.state) else None
  }
  
  /** Returns the first element of this collection that satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return some found element, or none if no element satisfies `p`.
    * @group  Querying
    */
  def find(p: A => Boolean): Option[A] = {
    val f = new Traversers.Find(p)
    try traverse(__)(f) catch { case e: Break => () }
    f.state
  }
  
  /** Returns `true` if a predicate holds for all elements of this collection.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if all elements satisfy `p`, else `false`.
    * @group  Querying
    */
  def forall(p: A => Boolean): Boolean = {
    val f = new Traversers.Forall(p)
    try traverse(__)(f) catch { case e: Break => () }
    f.state
  }
  
  /** Returns `true` if a predicate holds for some element of this collection.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if any element satisfies `p`, else `false`.
    * @group  Querying
    */
  def exists(p: A => Boolean): Boolean = {
    val f = new Traversers.Exists(p)
    try traverse(__)(f) catch { case e: Break => () }
    f.state
  }
  
  /** Returns the number of elements in this collection that satisfy a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the number of elements satisfying `p`.
    * @group  Querying
    */
  def count(p: A => Boolean): Int = {
    val f = new Traversers.Count(p)
    traverse(__)(f)
    f.state
  }
  
  /** Returns the application of a partial function to the first element
    * of this collection for which the function is defined.
    * 
    * @param  q   the partial function to test elements against and to apply
    *             to the first found element.
    * @return some found and mapped element, or none if no element applies to `q`.
    * @group  Querying
    */
  def select[B](q: PartialFunction[A, B]): Option[B] = {
    val f = new Traversers.Select(q)
    try traverse(__)(f) catch { case e: Break => () }
    f.state
  }
  
  /** Returns the applications of a partial function to each element of this
    * collection for which the function is defined.
    * 
    * @param  q       the partial function to filter elements against and to
    *                 apply to applicable elements.
    * @param  buffer  the implicit accumulator for mapped elements.
    * @return the accumulated elements filtered and mapped by `q`.
    * @group  Transforming
    */
  def collect[B](q: PartialFunction[A, B])(implicit buffer: Buffer[Self, B]): buffer.State = {
    traverse(__)(new Traversers.CollectInto(q, buffer))
    buffer.state
  }
  
  /** Returns the applications of a function to each element of this collection.
    * 
    * @param  f       the function to apply to each element.
    * @param  buffer  the implicit accumulator for mapped elements.
    * @return the accumulated elements mapped by `f`.
    * @group  Transforming
    */
  def map[B](f: A => B)(implicit buffer: Buffer[Self, B]): buffer.State = {
    traverse(__)(new Traversers.MapInto(f, buffer))
    buffer.state
  }
  
  /** Returns the concatenation of all elements returned by a function applied
    * to each element of this collection.
    * 
    * @param  f       the enumerator-yielding function to apply to each element.
    * @param  buffer  the implicit accumulator for flattened elements.
    * @return the concatenation of all accumulated elements produced by `f`.
    * @group  Transforming
    */
  def flatMap[B](f: A => Enumerator[B])(implicit buffer: Buffer[Self, B]): buffer.State = {
    traverse(__)(new Traversers.FlatMapInto(f, buffer))
    buffer.state
  }
  
  /** Returns all elements of this collection that satisfy a predicate.
    * 
    * @param  p       the predicate to test elements against.
    * @param  buffer  the implicit accumulator for filtered elements.
    * @return the accumulated elements filtered by `p`.
    * @group  Transforming
    */
  def filter(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(__)(new Traversers.FilterInto(p, buffer))
    buffer.state
  }
  
  def withFilter(p: A => Boolean): Collection[A] =
    new CollectionWithFilter(__, p)
  
  /** Returns all elements following the longest prefix of this collection
    * for which each element satisfies a predicate.
    * 
    * @param  p       the predicate to test elements against.
    * @param  buffer  the implicit accumulator for non-dropped elements.
    * @return the suffix of accumulated elements beginning with the first
    *         element to not satisfy `p`.
    * @group  Dividing
    */
  def dropWhile(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(__)(new Traversers.DropWhileInto(p, buffer))
    buffer.state
  }
  
  /** Returns the longest prefix of this collection for which each element
    * satisfies a predicate.
    * 
    * @param  p       the predicate to test elements against.
    * @param  buffer  the implicit accumulator for taken elements.
    * @return the longest prefix of accumulated elements preceding the first
    *         element to not satisfy `p`.
    * @group  Dividing
    */
  def takeWhile(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(__)(new Traversers.TakeWhileInto(p, buffer))
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
    * @group  Dividing
    */
  def span(p: A => Boolean)(implicit bufferA: Buffer[Self, A], bufferB: Buffer[Self, A])
    : (bufferA.State, bufferB.State) = {
    traverse(__)(new Traversers.SpanInto(p, bufferA, bufferB))
    (bufferA.state, bufferB.state)
  }
  
  /** Returns all elements of this collection following a prefix up to some length.
    * 
    * @param  lower   the length of the prefix to drop;
    *                 also the inclusive lower bound for indexes of elements to keep.
    * @param  buffer  the accumulator for non-dropped elements.
    * @return all but the first `lower` accumulated elements.
    * @group  Dividing
    */
  def drop(lower: Int)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(__)(new Traversers.DropInto(lower, buffer))
    buffer.state
  }
  
  /** Returns a prefix of this collection up to some length.
    * 
    * @param  upper   the length of the prefix to take;
    *                 also the exclusive upper bound for indexes of elements to keep.
    * @param  buffer  the accumulator for taken elements.
    * @return up to the first `upper` accumulated elements.
    * @group  Dividing
    */
  def take(upper: Int)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(__)(new Traversers.TakeInto(upper, buffer))
    buffer.state
  }
  
  /** Returns an interval of elements of this collection.
    * 
    * @param  lower   the inclusive lower bound for indexes of elements to keep.
    * @param  upper   the exclusive upper bound for indexes of elements to keep.
    * @param  buffer  the accumulator for kept elements.
    * @return the accumulated elements with indexes greater than or equal to
    *         `lower` and less than `upper`.
    * @group  Dividing
    */
  def slice(lower: Int, upper: Int)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(__)(new Traversers.SliceInto(lower, upper, buffer))
    buffer.state
  }
  
  /** Returns the concatenation of this and another collection.
    * 
    * @param  that    the collection to append to this collection.
    * @param  buffer  the implicit accumulator for concatenated elements.
    * @return the accumulated elements of both collections.
    * @group  Expanding
    */
  def ++ [B >: A](that: Collection[B])(implicit buffer: Buffer[Self, B]): buffer.State = {
    val f = new Traversers.AddInto(buffer)
    traverse(__)(f)
    traverse(that)(f)
    buffer.state
  }
}

private[basis] final class CollectionWithFilter[+A](self: Collection[A], p: A => Boolean) extends Collection[A] {
  protected override def foreach[U](f: A => U): Unit =
    Enumerator.traverse(self)(new Traversers.Filter(p, f))
}
