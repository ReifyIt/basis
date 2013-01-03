/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

/** General container operations.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * @group    General
  * 
  * @groupprio  Traversing    1
  * @groupprio  Reducing      2
  * @groupprio  Querying      3
  * @groupprio  Transforming  4
  */
final class GeneralContainerOps[+A](these: Container[A]) {
  /** Sequentially applies a function to each element of this container.
    * 
    * @param  f   the function to apply to each element.
    * @group  Traversing
    */
  def foreach[U](f: A => U): Unit =
    macro GeneralContainerOps.foreach[A, U]
  
  /** Returns the repeated application of an associative binary operator
    * between an identity value and all elements of this container.
    * 
    * @param  z   the operator's identity element.
    * @param  op  the associative binary operator to apply.
    * @return the folded value.
    * @group  Reducing
    */
  def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro GeneralContainerOps.foldLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all elements of this non-empty container.
    * 
    * @param  op  the associative binary operator to apply.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduce[B >: A](op: (B, B) => B): B =
    macro GeneralContainerOps.reduceLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all elements of this container.
    * 
    * @param  op  the associative binary operator to apply.
    * @return some reduced value, or none if this container is empty.
    * @group  Reducing
    */
  def reduceOption[B >: A](op: (B, B) => B): Option[B] =
    macro GeneralContainerOps.reduceLeftOption[A, B]
  
  /** Returns the left-to-right application of a binary operator between a
    * start value and all elements of this container.
    * 
    * @param  z   the starting value.
    * @param  op  the binary operator to apply right-recursively.
    * @return the folded value.
    * @group  Reducing
    */
  def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro GeneralContainerOps.foldLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all elements of this non-empty container.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduceLeft[B >: A](op: (B, A) => B): B =
    macro GeneralContainerOps.reduceLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all elements of this container.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return some reduced value, or none if this container is empty.
    * @group  Reducing
    */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    macro GeneralContainerOps.reduceLeftOption[A, B]
  
  /** Returns the first element of this container that satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return some found element, or none if no element satisfies `p`.
    * @group  Querying
    */
  def find(p: A => Boolean): Option[A] =
    macro GeneralContainerOps.find[A]
  
  /** Returns `true` if a predicate holds for all elements of this container.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if all elements satisfy `p`, otherwise `false`.
    * @group  Querying
    */
  def forall(p: A => Boolean): Boolean =
    macro GeneralContainerOps.forall[A]
  
  /** Returns `true` if a predicate holds for some element of this container.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if any element satisfies `p`, otherwise `false`.
    * @group  Querying
    */
  def exists(p: A => Boolean): Boolean =
    macro GeneralContainerOps.exists[A]
  
  /** Returns the number of elements in this container that satisfy a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the number of elements satisfying `p`.
    * @group  Querying
    */
  def count(p: A => Boolean): Int =
    macro GeneralContainerOps.count[A]
  
  /** Returns the application of a partial function to the first element
    * of this container for which the function is defined.
    * 
    * @param  q   the partial function to test elements against and to apply
    *             to the first found element.
    * @return some found and mapped element, or none if no element applies to `q`.
    * @group  Querying
    */
  def choose[B](q: PartialFunction[A, B]): Option[B] =
    macro GeneralContainerOps.choose[A, B]
  
  /** Returns a strict operations interface to this container.
    * @group Transforming */
  def eagerly: StrictContainerOps[A, Container[A]] =
    macro GeneralContainerOps.eagerly[A]
  
  /** Returns a non-strict operations interface to this container.
    * @group Transforming */
  def lazily: NonStrictContainerOps[A] =
    macro GeneralContainerOps.lazily[A]
}

private[sequential] object GeneralContainerOps {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Container[A]] = {
    import c.{Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag}
    import c.universe._
    val Apply(_, container :: Nil) = prefix.tree
    val ContainerType =
      appliedType(
        mirror.staticClass("basis.collections.Container").toType,
        weakTypeOf[A] :: Nil)
    Expr(typeCheck(container, ContainerType))(WeakTypeTag(ContainerType))
  }
  
  private def iterator[A : c.WeakTypeTag](c: Context)(container: c.Expr[Container[A]]): c.Expr[Iterator[A]] = {
    import c.{Expr, mirror, weakTypeOf, WeakTypeTag}
    import c.universe._
    val IteratorType =
      appliedType(
        mirror.staticClass("basis.collections.Iterator").toType,
        weakTypeOf[A] :: Nil)
    Expr(Select(container.tree, "iterator"))(WeakTypeTag(IteratorType))
  }
  
  def foreach[A : c.WeakTypeTag, U : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => U])
    : c.Expr[Unit] =
    new IteratorMacros[c.type](c).foreach[A, U](iterator(c)(unApply[A](c)))(f)
  
  def foldLeft[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (z: c.Expr[B])
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] =
    new IteratorMacros[c.type](c).foldLeft[A, B](iterator(c)(unApply[A](c)))(z)(op)
  
  def reduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] =
    new IteratorMacros[c.type](c).reduceLeft[A, B](iterator(c)(unApply[A](c)))(op)
  
  def reduceLeftOption[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[Option[B]] =
    new IteratorMacros[c.type](c).reduceLeftOption[A, B](iterator(c)(unApply[A](c)))(op)
  
  def find[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Option[A]] =
    new IteratorMacros[c.type](c).find[A](iterator(c)(unApply[A](c)))(p)
  
  def forall[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    new IteratorMacros[c.type](c).forall[A](iterator(c)(unApply[A](c)))(p)
  
  def exists[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    new IteratorMacros[c.type](c).exists[A](iterator(c)(unApply[A](c)))(p)
  
  def count[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Int] =
    new IteratorMacros[c.type](c).count[A](iterator(c)(unApply[A](c)))(p)
  
  def choose[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
    : c.Expr[Option[B]] =
    new IteratorMacros[c.type](c).choose[A, B](iterator(c)(unApply[A](c)))(q)
  
  def eagerly[A : c.WeakTypeTag](c: Context): c.Expr[StrictContainerOps[A, Container[A]]] =
    Strict.StrictContainerOps[A](c)(unApply[A](c))
  
  def lazily[A : c.WeakTypeTag](c: Context): c.Expr[NonStrictContainerOps[A]] =
    NonStrict.NonStrictContainerOps[A](c)(unApply[A](c))
}
