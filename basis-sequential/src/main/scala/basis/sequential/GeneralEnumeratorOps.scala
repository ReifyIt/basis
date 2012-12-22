/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

/** General enumerator operations.
  * 
  * @groupprio  Traversing    -4
  * @groupprio  Reducing      -3
  * @groupprio  Querying      -2
  * @groupprio  Transforming  -1
  */
final class GeneralEnumeratorOps[+A](val these: Enumerator[A]) extends AnyVal {
  /** Sequentially applies a function to each element of this enumerator.
    * 
    * @param  f   the function to apply to each element.
    * @group  Traversing
    */
  def foreach[U](f: A => U): Unit = traverse(these)(f)
  
  /** Returns the repeated application of an associative binary operator
    * between an identity value and all elements of this enumerator.
    * 
    * @param  z   the operator's identity element.
    * @param  op  the associative binary operator to apply.
    * @return the folded value.
    * @group  Reducing
    */
  def fold[B >: A](z: B)(op: (B, B) => B): B = {
    val f = new GeneralEnumeratorOps.FoldLeft(z)(op)
    traverse(these)(f)
    f.state
  }
  
  /** Returns the repeated application of an associative binary operator
    * between all elements of this non-empty enumerator.
    * 
    * @param  op  the associative binary operator to apply.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduce[B >: A](op: (B, B) => B): B = {
    val f = new GeneralEnumeratorOps.ReduceLeft(op)
    traverse(these)(f)
    if (f.isDefined) f.state
    else throw new UnsupportedOperationException
  }
  
  /** Returns the repeated application of an associative binary operator
    * between all elements of this enumerator.
    * 
    * @param  op  the associative binary operator to apply.
    * @return some reduced value, or none if this enumerator is empty.
    * @group  Reducing
    */
  def reduceOption[B >: A](op: (B, B) => B): Option[B] = {
    val f = new GeneralEnumeratorOps.ReduceLeft(op)
    traverse(these)(f)
    if (f.isDefined) Some(f.state)
    else None
  }
  
  /** Returns the left-to-right application of a binary operator between a
    * start value and all elements of this enumerator.
    * 
    * @param  z   the starting value.
    * @param  op  the binary operator to apply right-recursively.
    * @return the folded value.
    * @group  Reducing
    */
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    val f = new GeneralEnumeratorOps.FoldLeft(z)(op)
    traverse(these)(f)
    f.state
  }
  
  /** Returns the left-to-right application of a binary operator between
    * all elements of this non-empty enumerator.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduceLeft[B >: A](op: (B, A) => B): B = {
    val f = new GeneralEnumeratorOps.ReduceLeft(op.asInstanceOf[(B, B) => B])
  //val f = new GeneralEnumeratorOps.ReduceLeft(op) // SI-6482
    traverse(these)(f)
    if (f.isDefined) f.state
    else throw new UnsupportedOperationException
  }
  
  /** Returns the left-to-right application of a binary operator between
    * all elements of this enumerator.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return some reduced value, or none if this enumerator is empty.
    * @group  Reducing
    */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = {
    val f = new GeneralEnumeratorOps.ReduceLeft(op.asInstanceOf[(B, B) => B])
  //val f = new GeneralEnumeratorOps.ReduceLeft(op) // SI-6482
    traverse(these)(f)
    if (f.isDefined) Some(f.state)
    else None
  }
  
  /** Returns the first element of this enumerator that satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return some found element, or none if no element satisfies `p`.
    * @group  Querying
    */
  def find(p: A => Boolean): Option[A] = {
    val f = new GeneralEnumeratorOps.Find(p)
    begin(traverse(these)(f))
    f.state
  }
  
  /** Returns `true` if a predicate holds for all elements of this enumerator.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if all elements satisfy `p`, otherwise `false`.
    * @group  Querying
    */
  def forall(p: A => Boolean): Boolean = {
    val f = new GeneralEnumeratorOps.Forall(p)
    begin(traverse(these)(f))
    f.state
  }
  
  /** Returns `true` if a predicate holds for some element of this enumerator.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if any element satisfies `p`, otherwise `false`.
    * @group  Querying
    */
  def exists(p: A => Boolean): Boolean = {
    val f = new GeneralEnumeratorOps.Exists(p)
    begin(traverse(these)(f))
    f.state
  }
  
  /** Returns the number of elements in this enumerator that satisfy a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the number of elements satisfying `p`.
    * @group  Querying
    */
  def count(p: A => Boolean): Int = {
    val f = new GeneralEnumeratorOps.Count(p)
    traverse(these)(f)
    f.state
  }
  
  /** Returns the application of a partial function to the first element
    * of this enumerator for which the function is defined.
    * 
    * @param  q   the partial function to test elements against and to apply
    *             to the first found element.
    * @return some found and mapped element, or none if no element applies to `q`.
    * @group  Querying
    */
  def choose[B](q: PartialFunction[A, B]): Option[B] = {
    val f = new GeneralEnumeratorOps.Choose(q)
    begin(traverse(these)(f))
    f.state
  }
  
  /** Returns a strict operations interface to this enumerator.
    * @group Transforming */
  def eagerly: StrictEnumeratorOps[A, Enumerator[A]] =
    new StrictEnumeratorOps[A, Enumerator[A]](these)
  
  /** Returns a non-strict operations interface to this enumerator.
    * @group Transforming */
  def lazily: NonStrictEnumeratorOps[A] =
    new NonStrictEnumeratorOps[A](these)
}

private[sequential] object GeneralEnumeratorOps {
  import scala.runtime.AbstractFunction1
  
  final class FoldLeft[-A, +B](z: B)(op: (B, A) => B) extends AbstractFunction1[A, Unit] {
    private[this] var r: B = z
    override def apply(x: A): Unit = r = op(r, x)
    def state: B = r
  }
  
  final class ReduceLeft[-A, +B >: A](op: (B, A) => B) extends AbstractFunction1[A, Unit] {
    private[this] var e: Boolean = false
    private[this] var r: B = _
    override def apply(x: A): Unit = if (!e) { r = x; e = true } else r = op(r, x)
    def isDefined: Boolean = e
    def state: B = r
  }
  
  final class Find[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var r: Option[A] = None
    override def apply(x: A): Unit = if (p(x)) { r = Some(x); begin.break() }
    def state: Option[A] = r
  }
  
  final class Forall[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var r: Boolean = true
    override def apply(x: A): Unit = if (!p(x)) { r = false; begin.break() }
    def state: Boolean = r
  }
  
  final class Exists[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var r: Boolean = false
    override def apply(x: A): Unit = if (p(x)) { r = true; begin.break() }
    def state: Boolean = r
  }
  
  final class Count[A](p: A => Boolean) extends AbstractFunction1[A, Unit] {
    private[this] var t: Int = 0
    override def apply(x: A): Unit = if (p(x)) t += 1
    def state: Int = t
  }
  
  final class Choose[-A, +B](q: scala.PartialFunction[A, B]) extends AbstractFunction1[A, Unit] {
    private[this] var r: Option[B] = None
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) { r = Some(q(x)); begin.break() }
    def state: Option[B] = r
  }
}