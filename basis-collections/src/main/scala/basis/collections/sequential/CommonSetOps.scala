/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

import basis.collections.general._

/** Common set operations.
  * 
  * @groupprio  Traversing    -3
  * @groupprio  Reducing      -2
  * @groupprio  Querying      -1
  */
abstract class CommonSetOps[A, From] private[sequential] {
  /** Sequentially applies a function to each element in this set.
    * 
    * @param  f   the function to apply to each element.
    * @group  Traversing
    */
  def foreach[U](f: A => U): Unit =
    macro CommonContainerOps.foreach[A, U]
  
  /** Returns the repeated application of an associative binary operator
    * between an identity value and all elements in this set.
    * 
    * @param  z   the operator's identity element.
    * @param  op  the associative binary operator to apply.
    * @return the folded value.
    * @group  Reducing
    */
  def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro CommonContainerOps.foldLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all elements in this non-empty set.
    * 
    * @param  op  the associative binary operator to apply.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduce[B >: A](op: (B, B) => B): B =
    macro CommonContainerOps.reduceLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all elements in this set.
    * 
    * @param  op  the associative binary operator to apply.
    * @return some reduced value, or none if this set is empty.
    * @group  Reducing
    */
  def reduceOption[B >: A](op: (B, B) => B): Option[B] =
    macro CommonContainerOps.reduceLeftOption[A, B]
  
  /** Returns the left-to-right application of a binary operator between a
    * start value and all elements in this set.
    * 
    * @param  z   the starting value.
    * @param  op  the binary operator to apply right-recursively.
    * @return the folded value.
    * @group  Reducing
    */
  def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro CommonContainerOps.foldLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all elements in this non-empty set.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduceLeft[B >: A](op: (B, A) => B): B =
    macro CommonContainerOps.reduceLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all elements in this set.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return some reduced value, or none if this set is empty.
    * @group  Reducing
    */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    macro CommonContainerOps.reduceLeftOption[A, B]
  
  /** Returns the first element in this set that satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return some found element, or none if no element satisfies `p`.
    * @group  Querying
    */
  def find(p: A => Boolean): Option[A] =
    macro CommonContainerOps.find[A]
  
  /** Returns `true` if a predicate holds for all elements in this set.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if all elements satisfy `p`, else `false`.
    * @group  Querying
    */
  def forall(p: A => Boolean): Boolean =
    macro CommonContainerOps.forall[A]
  
  /** Returns `true` if a predicate holds for some element in this set.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if any element satisfies `p`, else `false`.
    * @group  Querying
    */
  def exists(p: A => Boolean): Boolean =
    macro CommonContainerOps.exists[A]
  
  /** Returns the number of elements in this set that satisfy a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the number of elements satisfying `p`.
    * @group  Querying
    */
  def count(p: A => Boolean): Int =
    macro CommonContainerOps.count[A]
  
  /** Returns the application of a partial function to the first element
    * in this set for which the function is defined.
    * 
    * @param  q   the partial function to test elements against and to apply
    *             to the first found element.
    * @return some found and mapped element, or none if no element applies to `q`.
    * @group  Querying
    */
  def select[B](q: PartialFunction[A, B]): Option[B] =
    macro CommonContainerOps.select[A, B]
  
  def eagerly: StrictSetOps[A, From] =
    macro CommonSetOps.eagerly[A, From]
  
  def lazily: NonStrictSetOps[A] =
    macro CommonSetOps.lazily[A]
}

private[sequential] object CommonSetOps {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def deconstruct(c: Context): c.Tree = {
    import c.universe._
    val Apply(_, set :: Nil) = c.prefix.tree
    set
  }
  
  def eagerly[A : c.WeakTypeTag, From : c.WeakTypeTag](c: Context): c.Expr[StrictSetOps[A, From]] = {
    import c.universe._
    c.Expr {
      Apply(
        Select(Select(Select(Select(Select(Ident(nme.ROOTPKG),
          "basis"), "collections"), "sequential"), "strict"), "StrictSetOps"),
        deconstruct(c) :: Nil)
    } (StrictSetOpsTag[A, From](c))
  }
  
  def lazily[A : c.WeakTypeTag](c: Context): c.Expr[NonStrictSetOps[A]] = {
    import c.universe._
    c.Expr {
      Apply(
        Select(Select(Select(Select(Select(Ident(nme.ROOTPKG),
          "basis"), "collections"), "sequential"), "strict"), "NonStrictSetOps"),
        deconstruct(c) :: Nil)
    } (NonStrictSetOpsTag[A](c))
  }
  
  private def StrictSetOpsTag[A : c.WeakTypeTag, From : c.WeakTypeTag](c: Context)
    : c.WeakTypeTag[StrictSetOps[A, From]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("basis.collections.sequential.StrictSetOps").toType,
        weakTypeOf[A] :: weakTypeOf[From] :: Nil))
  }
  
  private def NonStrictSetOpsTag[A : c.WeakTypeTag](c: Context)
    : c.WeakTypeTag[NonStrictSetOps[A]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("basis.collections.sequential.NonStrictSetOps").toType,
        weakTypeOf[A] :: Nil))
  }
}
