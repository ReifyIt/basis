/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

/** Iterator analyses and strict transformations.
  * 
  * @groupprio  Traversing    -6
  * @groupprio  Folding       -5
  * @groupprio  Querying      -4
  * @groupprio  Transforming  -3
  * @groupprio  Dividing      -2
  * @groupprio  Expanding     -1
  */
class IteratorOps[A](self: Iterator[A]) {
  /** Sequentially applies a function to each iterated element.
    * 
    * @param  f   the function to apply to each element.
    * @group  Traversing
    */
  def foreach[U](f: A => U): Unit =
    macro IteratorMacros.foreach[A, U]
  
  /** Returns the repeated application of an associative binary operator
    * between an identity value and all iterated elements.
    * 
    * @param  z   the operator's identity element.
    * @param  op  the associative binary operator to apply.
    * @return the folded value.
    * @group  Folding
    */
  def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro IteratorMacros.foldLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all iterated elements–undefined for empty iterators.
    * 
    * @param  op  the associative binary operator to apply.
    * @return the reduced value.
    * @group  Folding
    */
  def reduce[B >: A](op: (B, B) => B): B =
    macro IteratorMacros.reduceLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all iterated elements.
    * 
    * @param  op  the associative binary operator to apply.
    * @return some reduced value, or none if the iterator is empty.
    * @group  Folding
    */
  def reduceOption[B >: A](op: (B, B) => B): Option[B] =
    macro IteratorMacros.reduceLeftOption[A, B]
  
  /** Returns the left-to-right application of a binary operator between a
    * start value and all iterated elements.
    * 
    * @param  z   the starting value.
    * @param  op  the binary operator to apply right-recursively.
    * @return the folded value.
    * @group  Folding
    */
  def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro IteratorMacros.foldLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all iterated elements–undefined for empty iterators.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return the reduced value.
    * @group  Folding
    */
  def reduceLeft[B >: A](op: (B, A) => B): B =
    macro IteratorMacros.reduceLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all iterated elements.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return some reduced value, or none if the iterator is empty.
    * @group  Folding
    */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    macro IteratorMacros.reduceLeftOption[A, B]
  
  /** Returns the first iterated element that satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return some found element, or none if no element satisfies `p`.
    * @group  Querying
    */
  def find(p: A => Boolean): Option[A] =
    macro IteratorMacros.find[A]
  
  /** Returns `true` if a predicate holds for all iterated elements.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if all elements satisfy `p`, else `false`.
    * @group  Querying
    */
  def forall(p: A => Boolean): Boolean =
    macro IteratorMacros.forall[A]
  
  /** Returns `true` if a predicate holds for some iterated element.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if any element satisfies `p`, else `false`.
    * @group  Querying
    */
  def exists(p: A => Boolean): Boolean =
    macro IteratorMacros.exists[A]
  
  /** Returns the number of iterated elements that satisfy a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the number of elements satisfying `p`.
    * @group  Querying
    */
  def count(p: A => Boolean): Int =
    macro IteratorMacros.count[A]
  
  /** Returns the application of a partial function to the first iterated
    * element for which the function is defined.
    * 
    * @param  q   the partial function to test elements against and to apply
    *             to the first found element.
    * @return some found and mapped element, or none if no element applies to `q`.
    * @group  Querying
    */
  def select[B](q: PartialFunction[A, B]): Option[B] =
    macro IteratorMacros.select[A, B]
  
  /** Returns a new iterator that applies a partial function to each iterated
    * element for which the function is defined, and skips all elements where
    * the function is undefined.
    * 
    * @param  q   the partial function to filter elements against and to
    *             apply to applicable elements.
    * @return an iterator that filters and maps a dup of this iterator's elements.
    * @group  Transforming
    */
  def collect[B](q: PartialFunction[A, B]): Iterator[B] =
    new Iterators.Collect(self.dup, q)
  
  /** Returns a new iterator that applies a function to each iterated element.
    * 
    * @param  f   the function to apply to each element.
    * @return an iterator that maps a dup of this iterator's elements.
    * @group  Transforming
    */
  def map[B](f: A => B): Iterator[B] =
    new Iterators.Map(self.dup, f)
  
  /** Returns a new iterator that concatenates the iterators returned by a
    * function applied to each iterated element.
    * 
    * @param  f   the iterator-yielding function to apply to each element.
    * @return an iterator that concatenates a dup of this iterator's mapped elements.
    * @group  Transforming
    */
  def flatMap[B](f: A => Iterator[B]): Iterator[B] =
    new Iterators.FlatMap(self.dup, f)
  
  /** Returns a new iterator that skips all iterated elements that don't satisfy a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return an iterator that filters a dup of this iterator's elements.
    * @group  Transforming
    */
  def filter(p: A => Boolean): Iterator[A] =
    new Iterators.Filter(self.dup, p)
  
  def withFilter(p: A => Boolean): Iterator[A] = filter(p)
  
  /** Returns a new iterator that skips the longest prefix of iterated elements
    * for which each element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return an iterator over the suffix of a dup of this iterator's elements
    *         beginning with the first element to not satisfy `p`.
    * @group  Dividing
    */
  def dropWhile(p: A => Boolean): Iterator[A] =
    new Iterators.DropWhile(self.dup, p)
  
  /** Returns a new iterator over the longest prefix of iterated elements
    * for which each element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return an iterator over the longest prefix of a dup of this iterator's
    *         elements preceding the first element to not satisfy `p`.
    * @group  Dividing
    */
  def takeWhile(p: A => Boolean): Iterator[A] =
    new Iterators.TakeWhile(self.dup, p)
  
  /** Returns a pair of (prefix, suffix) iterators with the first iterator
    * covering the longest prefix for which each element satisfies a predicate,
    * and the second iterator beginning with the first element to not satisfy
    * the predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the (prefix, suffix) iterator pair.
    * @group  Dividing
    */
  def span(p: A => Boolean): (Iterator[A], Iterator[A]) =
    (takeWhile(p), dropWhile(p))
  
  /** Returns a new iterator over all iterated elements following a prefix up to some length.
    * 
    * @param  lower   the length of the prefix to drop;
    *                 also the inclusive lower bound for indexes of elements to keep.
    * @return an iterator over all but the first `lower` of a dup of this iterator's elements.
    * @group  Dividing
    */
  def drop(lower: Int): Iterator[A] =
    new Iterators.Drop(self.dup, lower)
  
  /** Returns an iterator over a prefix of iterated elements up to some length.
    * 
    * @param  upper   the length of the prefix to take;
    *                 also the exclusive upper bound for indexes of elements to keep.
    * @return an iterator over up to the first `upper` of a dup of this iterator's elements.
    * @group  Dividing
    */
  def take(upper: Int): Iterator[A] =
    new Iterators.Take(self.dup, upper)
  
  /** Returns a new iterator over an interval of iterated elements.
    * 
    * @param  lower   the inclusive lower bound for indexes of elements to keep.
    * @param  upper   the exclusive upper bound for indexes of elements to keep.
    * @return an iterator over a dup of this iterator's elements with indexes
    *         greater than or equal to `lower` and less than `upper`.
    * @group  Dividing
    */
  def slice(lower: Int, upper: Int): Iterator[A] =
    new Iterators.Slice(self.dup, lower, upper)
  
  /** Returns an iterator over pairs of elements from this and another iterator.
    * 
    * @param  that  the iterator whose elements to pair with these elements.
    * @return an iterator over dups of this and that iterators' elements paired together.
    * @group  Transforming
    */
  def zip[B](that: Iterator[B]): Iterator[(A, B)] =
    new Iterators.Zip(self.dup, that.dup)
  
  /** Returns a new iterator covering both this and another iterator.
    * 
    * @param  that  the iterator to append to this iterator.
    * @return an iterator over dups of this then that iterator's elements.
    * @group  Expanding
    */
  def ++ [B >: A](that: Iterator[B]): Iterator[B] =
    if (self.isEmpty) that.dup else new Iterators.++(self.dup, that.dup)
}
