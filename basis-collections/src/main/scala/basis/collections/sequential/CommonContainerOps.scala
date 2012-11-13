/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

import basis.collections.general._

/** Common container operations.
  * 
  * @groupprio  Traversing    -3
  * @groupprio  Reducing      -2
  * @groupprio  Querying      -1
  */
abstract class CommonContainerOps[A, From] private[sequential] {
  /** Sequentially applies a function to each element in this container.
    * 
    * @param  f   the function to apply to each element.
    * @group  Traversing
    */
  def foreach[U](f: A => U): Unit =
    macro CommonContainerOps.foreach[A, U]
  
  /** Returns the repeated application of an associative binary operator
    * between an identity value and all elements in this container.
    * 
    * @param  z   the operator's identity element.
    * @param  op  the associative binary operator to apply.
    * @return the folded value.
    * @group  Reducing
    */
  def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro CommonContainerOps.foldLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all elements in this non-empty container.
    * 
    * @param  op  the associative binary operator to apply.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduce[B >: A](op: (B, B) => B): B =
    macro CommonContainerOps.reduceLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all elements in this container.
    * 
    * @param  op  the associative binary operator to apply.
    * @return some reduced value, or none if this container is empty.
    * @group  Reducing
    */
  def reduceOption[B >: A](op: (B, B) => B): Option[B] =
    macro CommonContainerOps.reduceLeftOption[A, B]
  
  /** Returns the left-to-right application of a binary operator between a
    * start value and all elements in this container.
    * 
    * @param  z   the starting value.
    * @param  op  the binary operator to apply right-recursively.
    * @return the folded value.
    * @group  Reducing
    */
  def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro CommonContainerOps.foldLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all elements in this non-empty container.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduceLeft[B >: A](op: (B, A) => B): B =
    macro CommonContainerOps.reduceLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all elements in this container.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return some reduced value, or none if this container is empty.
    * @group  Reducing
    */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    macro CommonContainerOps.reduceLeftOption[A, B]
  
  /** Returns the first element in this container that satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return some found element, or none if no element satisfies `p`.
    * @group  Querying
    */
  def find(p: A => Boolean): Option[A] =
    macro CommonContainerOps.find[A]
  
  /** Returns `true` if a predicate holds for all elements in this container.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if all elements satisfy `p`, else `false`.
    * @group  Querying
    */
  def forall(p: A => Boolean): Boolean =
    macro CommonContainerOps.forall[A]
  
  /** Returns `true` if a predicate holds for some element in this container.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if any element satisfies `p`, else `false`.
    * @group  Querying
    */
  def exists(p: A => Boolean): Boolean =
    macro CommonContainerOps.exists[A]
  
  /** Returns the number of elements in this container that satisfy a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the number of elements satisfying `p`.
    * @group  Querying
    */
  def count(p: A => Boolean): Int =
    macro CommonContainerOps.count[A]
  
  /** Returns the application of a partial function to the first element
    * in this container for which the function is defined.
    * 
    * @param  q   the partial function to test elements against and to apply
    *             to the first found element.
    * @return some found and mapped element, or none if no element applies to `q`.
    * @group  Querying
    */
  def select[B](q: PartialFunction[A, B]): Option[B] =
    macro CommonContainerOps.select[A, B]
  
  def eagerly: StrictContainerOps[A, From] =
    macro CommonContainerOps.eagerly[A, From]
  
  def lazily: NonStrictContainerOps[A] =
    macro CommonContainerOps.lazily[A]
}

private[sequential] object CommonContainerOps {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Container[A]] = {
    import c.universe._
    val Apply(_, container :: Nil) = c.prefix.tree
    c.Expr(container)(ContainerTag[A](c))
  }
  
  private def unApplyIterator[A : c.WeakTypeTag](c: Context): c.Expr[Iterator[A]] = {
    import c.universe._
    val Apply(_, container :: Nil) = c.prefix.tree
    c.Expr(Select(container, "iterator"))(IteratorTag[A](c))
  }
  
  def foreach[A : c.WeakTypeTag, U]
      (c: Context)
      (f: c.Expr[A => U])
    : c.Expr[Unit] =
    new IteratorMacros[c.type](c).foreach[A, U](unApplyIterator(c))(f)
  
  def foldLeft[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (z: c.Expr[B])
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] =
    new IteratorMacros[c.type](c).foldLeft[A, B](unApplyIterator(c))(z)(op)
  
  def reduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] =
    new IteratorMacros[c.type](c).reduceLeft[A, B](unApplyIterator(c))(op)
  
  def reduceLeftOption[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[Option[B]] =
    new IteratorMacros[c.type](c).reduceLeftOption[A, B](unApplyIterator(c))(op)
  
  def find[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Option[A]] =
    new IteratorMacros[c.type](c).find[A](unApplyIterator(c))(p)
  
  def forall[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    new IteratorMacros[c.type](c).forall[A](unApplyIterator(c))(p)
  
  def exists[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    new IteratorMacros[c.type](c).exists[A](unApplyIterator(c))(p)
  
  def count[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Int] =
    new IteratorMacros[c.type](c).count[A](unApplyIterator(c))(p)
  
  def select[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
    : c.Expr[Option[B]] =
    new IteratorMacros[c.type](c).select[A, B](unApplyIterator(c))(q)
  
  def eagerly[A : c.WeakTypeTag, From: c.WeakTypeTag](c: Context): c.Expr[StrictContainerOps[A, From]] = {
    import c.universe._
    c.Expr {
      Apply(
        Select(Select(Select(Select(Select(Ident(nme.ROOTPKG),
          "basis"), "collections"), "sequential"), "strict"), "StrictContainerOps"),
        unApply(c).tree :: Nil)
    } (StrictContainerOpsTag[A, From](c))
  }
  
  def lazily[A : c.WeakTypeTag](c: Context): c.Expr[NonStrictContainerOps[A]] = {
    import c.universe._
    c.Expr {
      Apply(
        Select(Select(Select(Select(Select(Ident(nme.ROOTPKG),
          "basis"), "collections"), "sequential"), "strict"), "NonStrictContainerOps"),
        unApply(c).tree :: Nil)
    } (NonStrictContainerOpsTag[A](c))
  }
  
  private def ContainerTag[A : c.WeakTypeTag](c: Context): c.WeakTypeTag[Container[A]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("basis.collections.general.Container").toType,
        weakTypeOf[A] :: Nil))
  }
  
  private def IteratorTag[A : c.WeakTypeTag](c: Context): c.WeakTypeTag[Iterator[A]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("basis.collections.general.Iterator").toType,
        weakTypeOf[A] :: Nil))
  }
  
  private def StrictContainerOpsTag[A : c.WeakTypeTag, From : c.WeakTypeTag](c: Context)
    : c.WeakTypeTag[StrictContainerOps[A, From]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("basis.collections.sequential.StrictContainerOps").toType,
        weakTypeOf[A] :: weakTypeOf[From] :: Nil))
  }
  
  private def NonStrictContainerOpsTag[A : c.WeakTypeTag](c: Context)
    : c.WeakTypeTag[NonStrictContainerOps[A]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("basis.collections.sequential.NonStrictContainerOps").toType,
        weakTypeOf[A] :: Nil))
  }
}
