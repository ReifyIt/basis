/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

/** Container analyses and strict transformations.
  * 
  * @groupprio  Traversing    -6
  * @groupprio  Folding       -5
  * @groupprio  Querying      -4
  * @groupprio  Transforming  -3
  * @groupprio  Dividing      -2
  * @groupprio  Expanding     -1
  */
class ContainerOps[+Self, +A](self: Container[A]) {
  /** Sequentially applies a function to each element in this container.
    * 
    * @param  f   the function to apply to each element.
    * @group  Traversing
    */
  def foreach[U](f: A => U): Unit =
    macro ContainerMacros.foreach[A, U]
  
  /** Returns the repeated application of an associative binary operator
    * between an identity value and all elements in this container.
    * 
    * @param  z   the operator's identity element.
    * @param  op  the associative binary operator to apply.
    * @return the folded value.
    * @group  Folding
    */
  def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro ContainerMacros.foldLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all elements in this non-empty container.
    * 
    * @param  op  the associative binary operator to apply.
    * @return the reduced value.
    * @group  Folding
    */
  def reduce[B >: A](op: (B, B) => B): B =
    macro ContainerMacros.reduceLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all elements in this container.
    * 
    * @param  op  the associative binary operator to apply.
    * @return some reduced value, or none if the container is empty.
    * @group  Folding
    */
  def reduceOption[B >: A](op: (B, B) => B): Option[B] =
    macro ContainerMacros.reduceLeftOption[A, B]
  
  /** Returns the left-to-right application of a binary operator between a
    * start value and all elements in this container.
    * 
    * @param  z   the starting value.
    * @param  op  the binary operator to apply right-recursively.
    * @return the folded value.
    * @group  Folding
    */
  def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro ContainerMacros.foldLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all elements in this non-empty container.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return the reduced value.
    * @group  Folding
    */
  def reduceLeft[B >: A](op: (B, A) => B): B =
    macro ContainerMacros.reduceLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all elements in this container.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return some reduced value, or none if the container is empty.
    * @group  Folding
    */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    macro ContainerMacros.reduceLeftOption[A, B]
  
  /** Returns the first element in this container that satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return some found element, or none if no element satisfies `p`.
    * @group  Querying
    */
  def find(p: A => Boolean): Option[A] =
    macro ContainerMacros.find[A]
  
  /** Returns `true` if a predicate holds for all elements in this container.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if all elements satisfy `p`, else `false`.
    * @group  Querying
    */
  def forall(p: A => Boolean): Boolean =
    macro ContainerMacros.forall[A]
  
  /** Returns `true` if a predicate holds for some element in this container.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if any element satisfies `p`, else `false`.
    * @group  Querying
    */
  def exists(p: A => Boolean): Boolean =
    macro ContainerMacros.exists[A]
  
  /** Returns the number of elements in this container that satisfy a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the number of elements satisfying `p`.
    * @group  Querying
    */
  def count(p: A => Boolean): Int =
    macro ContainerMacros.count[A]
  
  /** Returns the application of a partial function to the first element
    * in this container for which the function is defined.
    * 
    * @param  q   the partial function to test elements against and to apply
    *             to the first found element.
    * @return some found and mapped element, or none if no element applies to `q`.
    * @group  Querying
    */
  def select[B](q: PartialFunction[A, B]): Option[B] =
    macro ContainerMacros.select[A, B]
  
  /** Returns the applications of a partial function to each element in this
    * container for which the function is defined.
    * 
    * @param  q       the partial function to filter elements against and to
    *                 apply to applicable elements.
    * @param  buffer  the implicit accumulator for mapped elements.
    * @return the accumulated elements filtered and mapped by `q`.
    * @group  Transforming
    */
  def collect[B](q: PartialFunction[A, B])(implicit buffer: Buffer[Self, B]): buffer.State =
    macro ContainerMacros.collect[A, B]
  
  /** Returns the applications of a function to each element in this container.
    * 
    * @param  f       the function to apply to each element.
    * @param  buffer  the implicit accumulator for mapped elements.
    * @return the accumulated elements mapped by `f`.
    * @group  Transforming
    */
  def map[B](f: A => B)(implicit buffer: Buffer[Self, B]): buffer.State =
    macro ContainerMacros.map[A, B]
  
  /** Returns the concatenation of all elements returned by a function applied
    * to each element in this container.
    * 
    * @param  f       the container-yielding function to apply to each element.
    * @param  buffer  the implicit accumulator for flattened elements.
    * @return the concatenation of all accumulated elements produced by `f`.
    * @group  Transforming
    */
  def flatMap[B](f: A => Container[B])(implicit buffer: Buffer[Self, B]): buffer.State =
    macro ContainerMacros.flatMap[A, B]
  
  /** Returns all elements in this container that satisfy a predicate.
    * 
    * @param  p       the predicate to test elements against.
    * @param  buffer  the implicit accumulator for filtered elements.
    * @return the accumulated elements filtered by `p`.
    * @group  Transforming
    */
  def filter(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ContainerMacros.filter[A]
  
  def withFilter(p: A => Boolean): Container[A] =
    new ContainerWithFilter(self, p)
  
  /** Returns all elements following the longest prefix of this container
    * for which each element satisfies a predicate.
    * 
    * @param  p       the predicate to test elements against.
    * @param  buffer  the implicit accumulator for non-dropped elements.
    * @return the suffix of accumulated elements beginning with the first
    *         element to not satisfy `p`.
    * @group  Dividing
    */
  def dropWhile(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ContainerMacros.dropWhile[A]
  
  /** Returns the longest prefix of this container for which each element
    * satisfies a predicate.
    * 
    * @param  p       the predicate to test elements against.
    * @param  buffer  the implicit accumulator for taken elements.
    * @return the longest prefix of accumulated elements preceding the first
    *         element to not satisfy `p`.
    * @group  Dividing
    */
  def takeWhile(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ContainerMacros.takeWhile[A]
  
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
  //FIXME: SI-6447
  //def span(p: A => Boolean)(
  //    implicit builderA: Buffer[Self, A],
  //             builderB: Buffer[Self, A])
  //  : (builderA.State, builderB.State) =
  //  macro ContainerMacros.span[A]
  
  /** Returns all elements in this collection following a prefix up to some length.
    * 
    * @param  lower   the length of the prefix to drop;
    *                 also the inclusive lower bound for indexes of elements to keep.
    * @param  buffer  the accumulator for non-dropped elements.
    * @return all but the first `lower` accumulated elements.
    * @group  Dividing
    */
  def drop(lower: Int)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ContainerMacros.drop[A]
  
  /** Returns a prefix of this container up to some length.
    * 
    * @param  upper   the length of the prefix to take;
    *                 also the exclusive upper bound for indexes of elements to keep.
    * @param  buffer  the accumulator for taken elements.
    * @return up to the first `upper` accumulated elements.
    * @group  Dividing
    */
  def take(upper: Int)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ContainerMacros.take[A]
  
  /** Returns an interval of elements in this container.
    * 
    * @param  lower   the inclusive lower bound for indexes of elements to keep.
    * @param  upper   the exclusive upper bound for indexes of elements to keep.
    * @param  buffer  the accumulator for kept elements.
    * @return the accumulated elements with indexes greater than or equal to
    *         `lower` and less than `upper`.
    * @group  Dividing
    */
  def slice(lower: Int, upper: Int)(implicit buffer: Buffer[Self, A]): buffer.State =
    macro ContainerMacros.slice[A]
  
  /** Returns pairs of elements from this and another container.
    * 
    * @param  that    the container of elements to pair with these elements.
    * @param  buffer  the accumulator for paired elements.
    * @return the accumulated pairs of corresponding elements.
    * @group  Transforming
    */
  def zip[B](that: Container[B])(implicit buffer: Buffer[Self, (A, B)]): buffer.State =
    macro ContainerMacros.zip[A, B]
  
  /** Returns the concatenation of this and another container.
    * 
    * @param  that    the container to append to this container.
    * @param  buffer  the implicit accumulator for concatenated elements.
    * @return the accumulated elements of both containers.
    * @group  Expanding
    */
  def ++ [B >: A](that: Container[B])(implicit buffer: Buffer[Self, B]): buffer.State =
    macro ContainerMacros.++[A, B]
}

private[basis] final class ContainerWithFilter[+A](self: Container[A], p: A => Boolean) extends Container[A] {
  protected override def foreach[U](f: A => U): Unit =
    Enumerator.traverse(self)(new Traversers.Filter(p, f))
  
  override def iterator: Iterator[A] =
    new Iterators.Filter(self.iterator, p)
}
