/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Common container operations.
  * 
  * @groupprio  Traversing    -3
  * @groupprio  Reducing      -2
  * @groupprio  Querying      -1
  */
abstract class CommonContainerOps[+Self, +A] private[sequential] {
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
  
  def eagerly: EagerContainerOps[Self, A] =
    macro CommonContainerOps.eagerly[Self, A]
  
  def lazily: LazyContainerOps[A] =
    macro CommonContainerOps.lazily[A]
}

private[sequential] object CommonContainerOps {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def deconstruct(c: Context): c.Tree = {
    import c.universe._
    val Apply(_, container :: Nil) = c.prefix.tree
    container
  }
  
  private def iterator(c: Context): c.Tree = {
    import c.universe._
    Select(deconstruct(c), "iterator")
  }
  
  def foreach[A : c.WeakTypeTag, U]
      (c: Context)
      (f: c.Expr[A => U])
    : c.Expr[Unit] =
    c.Expr {
      IteratorMacros.foreach[A, U](c)(iterator(c), f.tree)
    } (c.TypeTag.Unit)
  
  def foldLeft[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (z: c.Expr[B])
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] =
    c.Expr {
      IteratorMacros.foldLeft[A, B](c)(iterator(c), z.tree, op.tree)
    } (c.weakTypeTag[B])
  
  def reduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] =
    c.Expr {
      IteratorMacros.reduceLeft[A, B](c)(iterator(c), op.tree)
    } (c.weakTypeTag[B])
  
  def reduceLeftOption[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[Option[B]] =
    c.Expr {
      IteratorMacros.reduceLeftOption[A, B](c)(iterator(c), op.tree)
    } (OptionTag[B](c))
  
  def find[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Option[A]] =
    c.Expr {
      IteratorMacros.find[A](c)(iterator(c), p.tree)
    } (OptionTag[A](c))
  
  def forall[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    c.Expr {
      IteratorMacros.forall[A](c)(iterator(c), p.tree)
    } (c.TypeTag.Boolean)
  
  def exists[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    c.Expr {
      IteratorMacros.exists[A](c)(iterator(c), p.tree)
    } (c.TypeTag.Boolean)
  
  def count[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Int] =
    c.Expr {
      IteratorMacros.count[A](c)(iterator(c), p.tree)
    } (c.TypeTag.Int)
  
  def select[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
    : c.Expr[Option[B]] =
    c.Expr {
      IteratorMacros.select[A, B](c)(iterator(c), q.tree)
    } (OptionTag[B](c))
  
  def eagerly[Self : c.WeakTypeTag, A : c.WeakTypeTag](c: Context): c.Expr[EagerContainerOps[Self, A]] = {
    import c.universe._
    c.Expr {
      Apply(
        Select(Select(Select(Select(Select(Ident(nme.ROOTPKG),
          "basis"), "collections"), "sequential"), "strict"), "EagerContainerOps"),
        deconstruct(c) :: Nil)
    } (EagerContainerOpsTag[Self, A](c))
  }
  
  def lazily[A : c.WeakTypeTag](c: Context): c.Expr[LazyContainerOps[A]] = {
    import c.universe._
    c.Expr {
      Apply(
        Select(Select(Select(Select(Select(Ident(nme.ROOTPKG),
          "basis"), "collections"), "sequential"), "strict"), "LazyContainerOps"),
        deconstruct(c) :: Nil)
    } (LazyContainerOpsTag[A](c))
  }
  
  private def EagerContainerOpsTag[Self : c.WeakTypeTag, A : c.WeakTypeTag](c: Context)
    : c.WeakTypeTag[EagerContainerOps[Self, A]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("basis.collections.sequential.EagerContainerOps").toType,
        weakTypeOf[Self] :: weakTypeOf[A] :: Nil))
  }
  
  private def LazyContainerOpsTag[A : c.WeakTypeTag](c: Context)
    : c.WeakTypeTag[LazyContainerOps[A]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("basis.collections.sequential.LazyContainerOps").toType,
        weakTypeOf[A] :: Nil))
  }
  
  private def OptionTag[A : c.WeakTypeTag](c: Context): c.WeakTypeTag[Option[A]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("scala.Option").toType,
        weakTypeOf[A] :: Nil))
  }
}
