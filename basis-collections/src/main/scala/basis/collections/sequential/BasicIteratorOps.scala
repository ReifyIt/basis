/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Operations available to all iterators.
  * 
  * @groupprio  Traversing    -3
  * @groupprio  Reducing      -2
  * @groupprio  Querying      -1
  */
abstract class BasicIteratorOps[+A] private[sequential] {
  /** Sequentially applies a function to each iterated element.
    * 
    * @param  f   the function to apply to each element.
    * @group  Traversing
    */
  def foreach[U](f: A => U): Unit =
    macro BasicIteratorOps.foreach[A, U]
  
  /** Returns the repeated application of an associative binary operator
    * between an identity value and all iterated elements.
    * 
    * @param  z   the operator's identity element.
    * @param  op  the associative binary operator to apply.
    * @return the folded value.
    * @group  Reducing
    */
  def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro BasicIteratorOps.foldLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all iterated elements–undefined for empty iterators.
    * 
    * @param  op  the associative binary operator to apply.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduce[B >: A](op: (B, B) => B): B =
    macro BasicIteratorOps.reduceLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all iterated elements.
    * 
    * @param  op  the associative binary operator to apply.
    * @return some reduced value, or none if this iterator is empty.
    * @group  Reducing
    */
  def reduceOption[B >: A](op: (B, B) => B): Option[B] =
    macro BasicIteratorOps.reduceLeftOption[A, B]
  
  /** Returns the left-to-right application of a binary operator between a
    * start value and all iterated elements.
    * 
    * @param  z   the starting value.
    * @param  op  the binary operator to apply right-recursively.
    * @return the folded value.
    * @group  Reducing
    */
  def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro BasicIteratorOps.foldLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all iterated elements–undefined for empty iterators.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduceLeft[B >: A](op: (B, A) => B): B =
    macro BasicIteratorOps.reduceLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all iterated elements.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return some reduced value, or none if this iterator is empty.
    * @group  Reducing
    */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    macro BasicIteratorOps.reduceLeftOption[A, B]
  
  /** Returns the first iterated element that satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return some found element, or none if no element satisfies `p`.
    * @group  Querying
    */
  def find(p: A => Boolean): Option[A] =
    macro BasicIteratorOps.find[A]
  
  /** Returns `true` if a predicate holds for all iterated elements.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if all elements satisfy `p`, else `false`.
    * @group  Querying
    */
  def forall(p: A => Boolean): Boolean =
    macro BasicIteratorOps.forall[A]
  
  /** Returns `true` if a predicate holds for some iterated element.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if any element satisfies `p`, else `false`.
    * @group  Querying
    */
  def exists(p: A => Boolean): Boolean =
    macro BasicIteratorOps.exists[A]
  
  /** Returns the number of iterated elements that satisfy a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the number of elements satisfying `p`.
    * @group  Querying
    */
  def count(p: A => Boolean): Int =
    macro BasicIteratorOps.count[A]
  
  /** Returns the application of a partial function to the first iterated
    * element for which the function is defined.
    * 
    * @param  q   the partial function to test elements against and to apply
    *             to the first found element.
    * @return some found and mapped element, or none if no element applies to `q`.
    * @group  Querying
    */
  def select[B](q: PartialFunction[A, B]): Option[B] =
    macro BasicIteratorOps.select[A, B]
}

private object BasicIteratorOps {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def deconstruct(c: Context): c.Tree = {
    import c.universe._
    val Apply(_, iterator :: Nil) = c.prefix.tree
    iterator
  }
  
  def foreach[A : c.WeakTypeTag, U](c: Context)(f: c.Expr[A => U]): c.Expr[Unit] =
    c.Expr(BasicIteratorMacros.foreach[A, U](c)(deconstruct(c), f.tree))(c.TypeTag.Unit)
  
  def foldLeft[A : c.WeakTypeTag, B : c.WeakTypeTag](c: Context)(z: c.Expr[B])(op: c.Expr[(B, A) => B]): c.Expr[B] =
    c.Expr(BasicIteratorMacros.foldLeft[A, B](c)(deconstruct(c), z.tree, op.tree))(c.weakTypeTag[B])
  
  def reduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag](c: Context)(op: c.Expr[(B, A) => B]): c.Expr[B] =
    c.Expr(BasicIteratorMacros.reduceLeft[A, B](c)(deconstruct(c), op.tree))(c.weakTypeTag[B])
  
  def reduceLeftOption[A : c.WeakTypeTag, B >: A : c.WeakTypeTag](c: Context)(op: c.Expr[(B, A) => B]): c.Expr[Option[B]] =
    c.Expr(BasicIteratorMacros.reduceLeftOption[A, B](c)(deconstruct(c), op.tree))(c.TypeTag.Nothing)
  
  def find[A : c.WeakTypeTag](c: Context)(p: c.Expr[A => Boolean]): c.Expr[Option[A]] =
    c.Expr(BasicIteratorMacros.find[A](c)(deconstruct(c), p.tree))(c.TypeTag.Nothing)
  
  def forall[A : c.WeakTypeTag](c: Context)(p: c.Expr[A => Boolean]): c.Expr[Boolean] =
    c.Expr(BasicIteratorMacros.forall[A](c)(deconstruct(c), p.tree))(c.TypeTag.Boolean)
  
  def exists[A : c.WeakTypeTag](c: Context)(p: c.Expr[A => Boolean]): c.Expr[Boolean] =
    c.Expr(BasicIteratorMacros.exists[A](c)(deconstruct(c), p.tree))(c.TypeTag.Boolean)
  
  def count[A : c.WeakTypeTag](c: Context)(p: c.Expr[A => Boolean]): c.Expr[Int] =
    c.Expr(BasicIteratorMacros.count[A](c)(deconstruct(c), p.tree))(c.TypeTag.Int)
  
  def select[A : c.WeakTypeTag, B : c.WeakTypeTag](c: Context)(q: c.Expr[PartialFunction[A, B]]): c.Expr[Option[B]] =
    c.Expr(BasicIteratorMacros.select[A, B](c)(deconstruct(c), q.tree))(c.TypeTag.Nothing)
}
