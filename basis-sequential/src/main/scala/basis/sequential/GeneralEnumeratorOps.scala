/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._
import basis.control._

/** General enumerator operations.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    General
  * 
  * @groupprio  Traversing    1
  * @groupprio  Reducing      2
  * @groupprio  Querying      3
  * @groupprio  Transforming  4
  * 
  * @define collection  enumerator
  */
final class GeneralEnumeratorOps[+A](val these: Enumerator[A]) extends AnyVal {
  /** Sequentially applies a function to each element of this $collection.
    * 
    * @param  f   the function to apply to each element.
    * @group  Traversing
    */
  def foreach[U](f: A => U): Unit = these traverse new GeneralEnumeratorOps.Foreach(f)
  
  /** Returns the repeated application of an associative binary operator
    * between an identity value and all elements of this $collection.
    * 
    * @param  z   the operator's identity element.
    * @param  op  the associative binary operator to apply.
    * @return the folded value.
    * @group  Reducing
    */
  def fold[B >: A](z: B)(op: (B, B) => B): B = {
    val f = new GeneralEnumeratorOps.FoldLeft(z)(op)
    these traverse f
    f.state
  }
  
  /** Returns the repeated application of an associative binary operator
    * between all elements of this non-empty $collection.
    * 
    * @param  op  the associative binary operator to apply.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduce[B >: A](op: (B, B) => B): B = {
    val f = new GeneralEnumeratorOps.ReduceLeft(op)
    these traverse f
    if (f.isDefined) f.state else throw new UnsupportedOperationException
  }
  
  /** Returns the repeated application of an associative binary operator
    * between all elements of this $collection.
    * 
    * @param  op  the associative binary operator to apply.
    * @return the free reduced value, or a trap if this $collection is empty.
    * @group  Reducing
    */
  def mayReduce[B >: A](op: (B, B) => B): Maybe[B] = {
    val f = new GeneralEnumeratorOps.ReduceLeft(op)
    these traverse f
    if (f.isDefined) Bind(f.state) else Trap
  }
  
  /** Returns the left-to-right application of a binary operator between a
    * start value and all elements of this $collection.
    * 
    * @param  z   the starting value.
    * @param  op  the binary operator to apply right-recursively.
    * @return the folded value.
    * @group  Reducing
    */
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    val f = new GeneralEnumeratorOps.FoldLeft(z)(op)
    these traverse f
    f.state
  }
  
  /** Returns the left-to-right application of a binary operator between
    * all elements of this non-empty $collection.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduceLeft[B >: A](op: (B, A) => B): B = {
    val f = new GeneralEnumeratorOps.ReduceLeft(op.asInstanceOf[(B, B) => B])
  //val f = new GeneralEnumeratorOps.ReduceLeft(op) // SI-6482
    these traverse f
    if (f.isDefined) f.state else throw new UnsupportedOperationException
  }
  
  /** Returns the left-to-right application of a binary operator between
    * all elements of this $collection.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return the free reduced value, or a trap if this $collection is empty.
    * @group  Reducing
    */
  def mayReduceLeft[B >: A](op: (B, A) => B): Maybe[B] = {
    val f = new GeneralEnumeratorOps.ReduceLeft(op.asInstanceOf[(B, B) => B])
  //val f = new GeneralEnumeratorOps.ReduceLeft(op) // SI-6482
    these traverse f
    if (f.isDefined) Bind(f.state) else Trap
  }
  
  /** Returns the first element of this $collection that satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the free found element, or a trap if no element satisfies `p`.
    * @group  Querying
    */
  def find(p: A => Boolean): Maybe[A] = {
    val f = new GeneralEnumeratorOps.Find(p)
    begin(these traverse f)
    f.state
  }
  
  /** Returns `true` if a predicate holds for all elements of this $collection.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if all elements satisfy `p`, otherwise `false`.
    * @group  Querying
    */
  def forall(p: A => Boolean): Boolean = {
    val f = new GeneralEnumeratorOps.Forall(p)
    begin(these traverse f)
    f.state
  }
  
  /** Returns `true` if a predicate holds for some element of this $collection.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if any element satisfies `p`, otherwise `false`.
    * @group  Querying
    */
  def exists(p: A => Boolean): Boolean = {
    val f = new GeneralEnumeratorOps.Exists(p)
    begin(these traverse f)
    f.state
  }
  
  /** Returns the number of elements in this $collection that satisfy a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the number of elements satisfying `p`.
    * @group  Querying
    */
  def count(p: A => Boolean): Int = {
    val f = new GeneralEnumeratorOps.Count(p)
    these traverse f
    f.state
  }
  
  /** Returns the application of a partial function to the first element
    * of this $collection for which the function is defined.
    * 
    * @param  q   the partial function to test elements against and to apply
    *             to the first found element.
    * @return the free found and mapped element, or a trap if no element applies to `q`.
    * @group  Querying
    */
  def choose[B](q: PartialFunction[A, B]): Maybe[B] = {
    val f = new GeneralEnumeratorOps.Choose(q)
    begin(these traverse f)
    f.state
  }
  
  /** Returns a strict operations interface to this $collection.
    * @group Transforming */
  def eagerly: StrictEnumeratorOps[A, Enumerator[_]] =
    new StrictEnumeratorOps[A, Enumerator[_]](these)
  
  /** Returns a non-strict operations interface to this $collection.
    * @group Transforming */
  def lazily: NonStrictEnumeratorOps[A] =
    new NonStrictEnumeratorOps[A](these)
}

private[sequential] object GeneralEnumeratorOps {
  import scala.runtime.AbstractFunction1
  
  final class Foreach[-A, +U](f: A => U) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = f(x)
  }
  
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
    private[this] var r: Maybe[A] = Trap
    override def apply(x: A): Unit = if (p(x)) { r = Bind(x); begin.break() }
    def state: Maybe[A] = r
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
    private[this] var r: Maybe[B] = Trap
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) { r = Bind(q(x)); begin.break() }
    def state: Maybe[B] = r
  }
}
