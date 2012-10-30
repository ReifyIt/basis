/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Common linear sequence operations.
  * 
  * @groupprio  Traversing    -3
  * @groupprio  Reducing      -2
  * @groupprio  Querying      -1
  */
abstract class CommonLinearSeqOps[+Self, +A] private[sequential] {
  /** Sequentially applies a function to each element in this sequence.
    * 
    * @param  f   the function to apply to each element.
    * @group  Traversing
    */
  def foreach[U](f: A => U): Unit =
    macro CommonLinearSeqOps.foreach[A, U]
  
  /** Returns the repeated application of an associative binary operator
    * between an identity value and all elements in this sequence.
    * 
    * @param  z   the operator's identity element.
    * @param  op  the associative binary operator to apply.
    * @return the folded value.
    * @group  Reducing
    */
  def fold[B >: A](z: B)(op: (B, B) => B): B =
    macro CommonLinearSeqOps.foldLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all elements in this non-empty sequence.
    * 
    * @param  op  the associative binary operator to apply.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduce[B >: A](op: (B, B) => B): B =
    macro CommonLinearSeqOps.reduceLeft[A, B]
  
  /** Returns the repeated application of an associative binary operator
    * between all elements in this sequence.
    * 
    * @param  op  the associative binary operator to apply.
    * @return some reduced value, or none if this sequence is empty.
    * @group  Reducing
    */
  def reduceOption[B >: A](op: (B, B) => B): Option[B] =
    macro CommonLinearSeqOps.reduceLeftOption[A, B]
  
  /** Returns the left-to-right application of a binary operator between a
    * start value and all elements in this sequence.
    * 
    * @param  z   the starting value.
    * @param  op  the binary operator to apply right-recursively.
    * @return the folded value.
    * @group  Reducing
    */
  def foldLeft[B](z: B)(op: (B, A) => B): B =
    macro CommonLinearSeqOps.foldLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all elements in this non-empty sequence.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return the reduced value.
    * @group  Reducing
    */
  def reduceLeft[B >: A](op: (B, A) => B): B =
    macro CommonLinearSeqOps.reduceLeft[A, B]
  
  /** Returns the left-to-right application of a binary operator between
    * all elements in this sequence.
    * 
    * @param  op  the binary operator to apply right-recursively.
    * @return some reduced value, or none if this sequence is empty.
    * @group  Reducing
    */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    macro CommonLinearSeqOps.reduceLeftOption[A, B]
  
  /** Returns the first element in this sequence that satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return some found element, or none if no element satisfies `p`.
    * @group  Querying
    */
  def find(p: A => Boolean): Option[A] =
    macro CommonLinearSeqOps.find[A]
  
  /** Returns `true` if a predicate holds for all elements in this sequence.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if all elements satisfy `p`, else `false`.
    * @group  Querying
    */
  def forall(p: A => Boolean): Boolean =
    macro CommonLinearSeqOps.forall[A]
  
  /** Returns `true` if a predicate holds for some element in this sequence.
    * 
    * @param  p   the predicate to test elements against.
    * @return `true` if any element satisfies `p`, else `false`.
    * @group  Querying
    */
  def exists(p: A => Boolean): Boolean =
    macro CommonLinearSeqOps.exists[A]
  
  /** Returns the number of elements in this sequence that satisfy a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the number of elements satisfying `p`.
    * @group  Querying
    */
  def count(p: A => Boolean): Int =
    macro CommonLinearSeqOps.count[A]
  
  /** Returns the application of a partial function to the first element
    * in this sequence for which the function is defined.
    * 
    * @param  q   the partial function to test elements against and to apply
    *             to the first found element.
    * @return some found and mapped element, or none if no element applies to `q`.
    * @group  Querying
    */
  def select[B](q: PartialFunction[A, B]): Option[B] =
    macro CommonLinearSeqOps.select[A, B]
  
  def eagerly: StrictLinearSeqOps[Self, A] =
    macro CommonLinearSeqOps.eagerly[Self, A]
  
  def lazily: NonStrictLinearSeqOps[A] =
    macro CommonLinearSeqOps.lazily[A]
}

private[sequential] object CommonLinearSeqOps {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[LinearSeq[A]] = {
    import c.universe._
    val Apply(_, seq :: Nil) = c.prefix.tree
    c.Expr(seq)(LinearSeqTag[A](c))
  }
  
  def foreach[A : c.WeakTypeTag, U]
      (c: Context)
      (f: c.Expr[A => U])
    : c.Expr[Unit] =
    new LinearSeqMacros[c.type](c).foreach[A, U](unApply(c))(f)
  
  def foldLeft[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (z: c.Expr[B])
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] =
    new LinearSeqMacros[c.type](c).foldLeft[A, B](unApply(c))(z)(op)
  
  def reduceLeft[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[B] =
    new LinearSeqMacros[c.type](c).reduceLeft[A, B](unApply(c))(op)
  
  def reduceLeftOption[A : c.WeakTypeTag, B >: A : c.WeakTypeTag]
      (c: Context)
      (op: c.Expr[(B, A) => B])
    : c.Expr[Option[B]] =
    new LinearSeqMacros[c.type](c).reduceLeftOption[A, B](unApply(c))(op)
  
  def find[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Option[A]] =
    new LinearSeqMacros[c.type](c).find[A](unApply(c))(p)
  
  def forall[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    new LinearSeqMacros[c.type](c).forall[A](unApply(c))(p)
  
  def exists[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Boolean] =
    new LinearSeqMacros[c.type](c).exists[A](unApply(c))(p)
  
  def count[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
    : c.Expr[Int] =
    new LinearSeqMacros[c.type](c).count[A](unApply(c))(p)
  
  def select[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
    : c.Expr[Option[B]] =
    new LinearSeqMacros[c.type](c).select[A, B](unApply(c))(q)
  
  def eagerly[Self : c.WeakTypeTag, A : c.WeakTypeTag](c: Context): c.Expr[StrictLinearSeqOps[Self, A]] = {
    import c.universe._
    c.Expr {
      Apply(
        Select(Select(Select(Select(Select(Ident(nme.ROOTPKG),
          "basis"), "collections"), "sequential"), "strict"), "StrictLinearSeqOps"),
        unApply[A](c).tree :: Nil)
    } (StrictLinearSeqOpsTag[Self, A](c))
  }
  
  def lazily[A : c.WeakTypeTag](c: Context): c.Expr[NonStrictLinearSeqOps[A]] = {
    import c.universe._
    c.Expr {
      Apply(
        Select(Select(Select(Select(Select(Ident(nme.ROOTPKG),
          "basis"), "collections"), "sequential"), "strict"), "NonStrictLinearSeqOps"),
        unApply[A](c).tree :: Nil)
    } (NonStrictLinearSeqOpsTag[A](c))
  }
  
  private def LinearSeqTag[A : c.WeakTypeTag](c: Context): c.WeakTypeTag[LinearSeq[A]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("basis.collections.LinearSeq").toType,
        weakTypeOf[A] :: Nil))
  }
  
  private def StrictLinearSeqOpsTag[Self : c.WeakTypeTag, A : c.WeakTypeTag](c: Context)
    : c.WeakTypeTag[StrictLinearSeqOps[Self, A]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("basis.collections.sequential.StrictLinearSeqOps").toType,
        weakTypeOf[Self] :: weakTypeOf[A] :: Nil))
  }
  
  private def NonStrictLinearSeqOpsTag[A : c.WeakTypeTag](c: Context)
    : c.WeakTypeTag[NonStrictLinearSeqOps[A]] = {
    import c.universe._
    c.WeakTypeTag(
      appliedType(
        c.mirror.staticClass("basis.collections.sequential.NonStrictLinearSeqOps").toType,
        weakTypeOf[A] :: Nil))
  }
}
