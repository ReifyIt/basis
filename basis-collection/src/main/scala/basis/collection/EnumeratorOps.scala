/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

/** Enumerator analyses and strict transformations.
  * 
  * @groupprio  Traversing    -5
  * @groupprio  Folding       -4
  * @groupprio  Querying      -3
  * @groupprio  Transforming  -2
  * @groupprio  Expanding     -1
  */
class EnumeratorOps[+Self, +A](val __ : Enumerator[A]) extends AnyVal {
  import Enumerator.traverse
  
  /** Sequentially applies a function to each enumerated element.
    * 
    * @param  f   the function to apply to each element.
    * @group  Traversing
    */
  def foreach[U](f: A => U): Unit = traverse(__)(f)
  
  /** Returns the repeated application of an associative binary operator
    * between an identity value and all enumerated elements.
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
    * between all enumerated elements–undefined for empty enumerators.
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
    * between all enumerated elements.
    * 
    * @param  op  the associative binary operator to apply.
    * @return some reduced value, or none if the enumerator is empty.
    * @group  Folding
    */
  def reduceOption[B >: A](op: (B, B) => B): Option[B] = {
    val f = new Traversers.ReduceLeft(op)
    traverse(__)(f)
    if (f.isDefined) Some(f.state) else None
  }
  
  /** Returns the left-to-right application of a binary operator between a
    * start value and all enumerated elements.
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
    * all enumerated elements–undefined for empty enumerators.
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
    * all enumerated elements.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return some reduced value, or none if the enumerator is empty.
    * @group  Folding
    */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = {
    val f = new Traversers.ReduceLeft(op.asInstanceOf[(B, B) => B]) // work around typer bug
    traverse(__)(f)
    if (f.isDefined) Some(f.state) else None
  }
  
  /** Returns the first enumerated element that satisfies a predicate.
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
  
  /** Returns `true` if a predicate holds for all enumerated elements.
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
  
  /** Returns `true` if a predicate holds for some enumerated element.
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
  
  /** Returns the number of enumerated elements that satisfy a predicate.
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
  
  /** Returns the application of a partial function to the first enumerated
    * element for which the function is defined.
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
  
  /** Returns the applications of a partial function to each enumerated element
    * for which the function is defined.
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
  
  /** Returns the applications of a function to each enumerated element.
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
    * to each enumerated element.
    * 
    * @param  f       the enumerator-yielding function to apply to each element.
    * @param  buffer  the implicit accumulator for flattened elements.
    * @return the concatenation of all enumerators produced by `f`.
    * @group  Transforming
    */
  def flatMap[B](f: A => Enumerator[B])(implicit buffer: Buffer[Self, B]): buffer.State = {
    traverse(__)(new Traversers.FlatMapInto(f, buffer))
    buffer.state
  }
  
  /** Returns all enumerated elements that satisfy a predicate.
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
  
  /** Returns the concatenation of this and another enumerator.
    * 
    * @param  that    the enumerator to append to this enumerator.
    * @param  buffer  the implicit accumulator for concatenated elements.
    * @return the accumulated elements of both enumerators.
    * @group  Expanding
    */
  def ++ [B >: A](that: Enumerator[B])(implicit buffer: Buffer[Self, B]): buffer.State = {
    val f = new Traversers.AddInto(buffer)
    traverse(__)(f)
    traverse(that)(f)
    buffer.state
  }
}
