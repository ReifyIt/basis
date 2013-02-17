/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._
import basis.control._

/** General sequence operations.
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
  * @define collection  sequence
  */
final class GeneralSeqOps[+A](these: Seq[A]) {
  /** Sequentially applies a function to each element of this $collection.
    * 
    * @param  f   the function to apply to each element.
    * @group  Traversing
    */
  def foreach[U](f: A => U): Unit =
    macro GeneralContainerOps.foreach[A, U]
  
  /** Returns the repeated application of an associative binary operator
    * between an identity value and all elements of this $collection.
    * 
    * @param  z   the operator's identity element.
    * @param  op  the associative binary operator to apply.
    * @return the folded value.
    * @group  Reducing
    */
  def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro GeneralContainerOps.foldLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all elements of this non-empty $collection.
    * 
    * @param  op  the associative binary operator to apply.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduce[B >: A](op: (B, B) => B): B =
    macro GeneralContainerOps.reduceLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all elements of this $collection.
    * 
    * @param  op  the associative binary operator to apply.
    * @return the free reduced value, or a trap if this $collection is empty.
    * @group  Reducing
    */
  def mayReduce[B >: A](op: (B, B) => B): Maybe[B] =
    macro GeneralContainerOps.mayReduceLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between a
    * start value and all elements of this $collection.
    * 
    * @param  z   the starting value.
    * @param  op  the binary operator to apply right-recursively.
    * @return the folded value.
    * @group  Reducing
    */
  def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro GeneralContainerOps.foldLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all elements of this non-empty $collection.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduceLeft[B >: A](op: (B, A) => B): B =
    macro GeneralContainerOps.reduceLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all elements of this $collection.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return the free reduced value, or a trap if this $collection is empty.
    * @group  Reducing
    */
  def mayReduceLeft[B >: A](op: (B, A) => B): Maybe[B] =
    macro GeneralContainerOps.mayReduceLeft[A, B]
  
  /** Returns the first element of this $collection that satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the free found element, or a trap if no element satisfies `p`.
    * @group  Querying
    */
  def find(p: A => Boolean): Maybe[A] =
    macro GeneralContainerOps.find[A]
  
  /** Returns `true` if a predicate holds for all elements of this $collection.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if all elements satisfy `p`, otherwise `false`.
    * @group  Querying
    */
  def forall(p: A => Boolean): Boolean =
    macro GeneralContainerOps.forall[A]
  
  /** Returns `true` if a predicate holds for some element of this $collection.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if any element satisfies `p`, otherwise `false`.
    * @group  Querying
    */
  def exists(p: A => Boolean): Boolean =
    macro GeneralContainerOps.exists[A]
  
  /** Returns the number of elements in this $collection that satisfy a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the number of elements satisfying `p`.
    * @group  Querying
    */
  def count(p: A => Boolean): Int =
    macro GeneralContainerOps.count[A]
  
  /** Returns the application of a partial function to the first element
    * of this $collection for which the function is defined.
    * 
    * @param  q   the partial function to test elements against and to apply
    *             to the first found element.
    * @return the free found and mapped element, or a trap if no element applies to `q`.
    * @group  Querying
    */
  def choose[B](q: PartialFunction[A, B]): Maybe[B] =
    macro GeneralContainerOps.choose[A, B]
  
  /** Returns a strict operations interface to this $collection.
    * @group Transforming */
  def eagerly: StrictSeqOps[A, Seq[_]] =
    macro GeneralSeqOps.eagerly[A]
  
  /** Returns a non-strict operations interface to this $collection.
    * @group Transforming */
  def lazily: NonStrictSeqOps[A] =
    macro GeneralSeqOps.lazily[A]
}

private[sequential] object GeneralSeqOps {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Seq[A]] = {
    import c.{Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag}
    import c.universe._
    val Apply(_, these :: Nil) = prefix.tree
    implicit val SeqATag =
      WeakTypeTag[Seq[A]](
        appliedType(
          mirror.staticClass("basis.collections.Seq").toType,
          weakTypeOf[A] :: Nil))
    Expr[Seq[A]](typeCheck(these, weakTypeOf[Seq[A]]))
  }
  
  def eagerly[A : c.WeakTypeTag](c: Context): c.Expr[StrictSeqOps[A, Seq[_]]] =
    Strict.StrictSeqOps[A](c)(unApply[A](c))
  
  def lazily[A : c.WeakTypeTag](c: Context): c.Expr[NonStrictSeqOps[A]] =
    NonStrict.NonStrictSeqOps[A](c)(unApply[A](c))
}
