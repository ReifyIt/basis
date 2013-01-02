/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

/** General linear sequence operations.
  * 
  * @groupprio  Traversing    1
  * @groupprio  Reducing      2
  * @groupprio  Querying      3
  * @groupprio  Transforming  4
  */
final class GeneralStackOps[+A](these: Stack[A]) {
  /** Sequentially applies a function to each element of this sequence.
    * 
    * @param  f   the function to apply to each element.
    * @group  Traversing
    */
  def foreach[U](f: A => U): Unit =
    macro GeneralStackOps.foreach[A, U]
  
  /** Returns the repeated application of an associative binary operator
    * between an identity value and all elements of this sequence.
    * 
    * @param  z   the operator's identity element.
    * @param  op  the associative binary operator to apply.
    * @return the folded value.
    * @group  Reducing
    */
  def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro GeneralStackOps.foldLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all elements of this non-empty sequence.
    * 
    * @param  op  the associative binary operator to apply.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduce[B >: A](op: (B, B) => B): B =
    macro GeneralStackOps.reduceLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all elements of this sequence.
    * 
    * @param  op  the associative binary operator to apply.
    * @return some reduced value, or none if this sequence is empty.
    * @group  Reducing
    */
  def reduceOption[B >: A](op: (B, B) => B): Option[B] =
    macro GeneralStackOps.reduceLeftOption[A, B]
  
  /** Returns the left-to-right application of a binary operator between a
    * start value and all elements of this sequence.
    * 
    * @param  z   the starting value.
    * @param  op  the binary operator to apply right-recursively.
    * @return the folded value.
    * @group  Reducing
    */
  def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro GeneralStackOps.foldLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all elements of this non-empty sequence.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduceLeft[B >: A](op: (B, A) => B): B =
    macro GeneralStackOps.reduceLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all elements of this sequence.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return some reduced value, or none if this sequence is empty.
    * @group  Reducing
    */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    macro GeneralStackOps.reduceLeftOption[A, B]
  
  /** Returns the first element of this sequence that satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return some found element, or none if no element satisfies `p`.
    * @group  Querying
    */
  def find(p: A => Boolean): Option[A] =
    macro GeneralStackOps.find[A]
  
  /** Returns `true` if a predicate holds for all elements of this sequence.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if all elements satisfy `p`, otherwise `false`.
    * @group  Querying
    */
  def forall(p: A => Boolean): Boolean =
    macro GeneralStackOps.forall[A]
  
  /** Returns `true` if a predicate holds for some element of this sequence.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if any element satisfies `p`, otherwise `false`.
    * @group  Querying
    */
  def exists(p: A => Boolean): Boolean =
    macro GeneralStackOps.exists[A]
  
  /** Returns the number of elements in this sequence that satisfy a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the number of elements satisfying `p`.
    * @group  Querying
    */
  def count(p: A => Boolean): Int =
    macro GeneralStackOps.count[A]
  
  /** Returns the application of a partial function to the first element
    * of this sequence for which the function is defined.
    * 
    * @param  q   the partial function to test elements against and to apply
    *             to the first found element.
    * @return some found and mapped element, or none if no element applies to `q`.
    * @group  Querying
    */
  def choose[B](q: PartialFunction[A, B]): Option[B] =
    macro GeneralStackOps.choose[A, B]
  
  /** Returns a strict operations interface for this sequence.
    * @group Transforming */
  def eagerly: StrictStackOps[A, Stack[A]] =
    macro GeneralStackOps.eagerly[A]
  
  /** Returns a non-strict operations interface for this sequence.
    * @group Transforming */
  def lazily: NonStrictStackOps[A] =
    macro GeneralStackOps.lazily[A]
}

private[sequential] object GeneralStackOps {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Stack[A]] = {
    import c.{Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag}
    import c.universe._
    val Apply(_, sequence :: Nil) = prefix.tree
    val StackType =
      appliedType(
        mirror.staticClass("basis.collections.Stack").toType,
        weakTypeOf[A] :: Nil)
    Expr(typeCheck(sequence, StackType))(WeakTypeTag(StackType))
  }
  
  def foreach[A : c.WeakTypeTag, U : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => U])
    : c.Expr[Unit] =
    new StackMacros[c.type](c).foreach[A, U](unApply[A](c))(f)
  
  def foldLeft[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (z: c.Expr[B])
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] =
    new StackMacros[c.type](c).foldLeft[A, B](unApply[A](c))(z)(op)
  
  def reduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] =
    new StackMacros[c.type](c).reduceLeft[A, B](unApply[A](c))(op)
  
  def reduceLeftOption[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[Option[B]] =
    new StackMacros[c.type](c).reduceLeftOption[A, B](unApply[A](c))(op)
  
  def find[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Option[A]] =
    new StackMacros[c.type](c).find[A](unApply[A](c))(p)
  
  def forall[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    new StackMacros[c.type](c).forall[A](unApply[A](c))(p)
  
  def exists[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    new StackMacros[c.type](c).exists[A](unApply[A](c))(p)
  
  def count[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Int] =
    new StackMacros[c.type](c).count[A](unApply[A](c))(p)
  
  def choose[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
    : c.Expr[Option[B]] =
    new StackMacros[c.type](c).choose[A, B](unApply[A](c))(q)
  
  def eagerly[A : c.WeakTypeTag](c: Context): c.Expr[StrictStackOps[A, Stack[A]]] =
    Strict.StrictStackOps[A](c)(unApply[A](c))
  
  def lazily[A : c.WeakTypeTag](c: Context): c.Expr[NonStrictStackOps[A]] =
    NonStrict.NonStrictStackOps[A](c)(unApply[A](c))
}
