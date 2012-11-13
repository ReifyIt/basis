/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

import basis.collections.general._

/** Strictly evaluated indexed sequence operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
abstract class StrictIndexedSeqOps[A, From] private[sequential] {
  /** Returns the applications of a partial function to each element in this
    * sequence for which the function is defined.
    * 
    * @param  q         the partial function to filter elements against and
    *                   to apply to applicable elements.
    * @param  builder   the implicit accumulator for mapped elements.
    * @return the accumulated elements filtered and mapped by `q`.
    * @group  Mapping
    */
  def collect[B, To](q: PartialFunction[A, B])(implicit builder: Builder[From, B, To]): To =
    macro StrictIndexedSeqOps.collect[A, B, To]
  
  /** Returns the applications of a function to each element in this sequence.
    * 
    * @param  f         the function to apply to each element.
    * @param  builder   the implicit accumulator for mapped elements.
    * @return the accumulated elements mapped by `f`.
    * @group  Mapping
    */
  def map[B, To](f: A => B)(implicit builder: Builder[From, B, To]): To =
    macro StrictIndexedSeqOps.map[A, B, To]
  
  /** Returns the concatenation of all elements returned by a function applied
    * to each element in this sequence.
    * 
    * @param  f         the enumerator-yielding function to apply to each element.
    * @param  builder   the implicit accumulator for flattened elements.
    * @return the concatenation of all accumulated elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B, To](f: A => Enumerator[B])(implicit builder: Builder[From, B, To]): To =
    macro StrictIndexedSeqOps.flatMap[A, B, To]
  
  /** Returns all elements in this sequence that satisfy a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for filtered elements.
    * @return the accumulated elements filtered by `p`.
    * @group  Filtering
    */
  def filter[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro StrictIndexedSeqOps.filter[A, To]
  
  /** Returns all elements following the longest prefix of this sequence
    * for which each element satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for non-dropped elements.
    * @return the suffix of accumulated elements beginning with the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro StrictIndexedSeqOps.dropWhile[A, To]
  
  /** Returns the longest prefix of this sequence for which each element
    * satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for taken elements.
    * @return the longest prefix of accumulated elements preceding the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro StrictIndexedSeqOps.takeWhile[A, To]
  
  /** Returns a (prefix, suffix) pair with the prefix being the longest one for
    * which each element satisfies a predicate, and the suffix beginning with
    * the first element to not satisfy the predicate.
    * 
    * @param  p           the predicate to test elements against.
    * @param  builderA    the implicit accumulator for prefix elements.
    * @param  builderB    the implicit accumilator for suffix elements.
    * @return the pair of accumulated prefix and suffix elements.
    * @group  Filtering
    */
  def span[To](p: A => Boolean)(
      implicit builderA: Builder[From, A, To],
               builderB: Builder[From, A, To])
    : (To, To) =
    macro StrictIndexedSeqOps.span[A, To]
  
  /** Returns all elements in this sequence following a prefix up to some length.
    * 
    * @param  lower     the length of the prefix to drop; also the inclusive
    *                   lower bound for indexes of elements to keep.
    * @param  builder   the accumulator for non-dropped elements.
    * @return all but the first `lower` accumulated elements.
    * @group  Filtering
    */
  def drop[To](lower: Int)(implicit builder: Builder[From, A, To]): To =
    macro StrictIndexedSeqOps.drop[A, To]
  
  /** Returns a prefix of this sequence up to some length.
    * 
    * @param  upper     the length of the prefix to take; also the exclusive
    *                   upper bound for indexes of elements to keep.
    * @param  builder   the accumulator for taken elements.
    * @return up to the first `upper` accumulated elements.
    * @group  Filtering
    */
  def take[To](upper: Int)(implicit builder: Builder[From, A, To]): To =
    macro StrictIndexedSeqOps.take[A, To]
  
  /** Returns an interval of elements in this sequence.
    * 
    * @param  lower     the inclusive lower bound for indexes of elements to keep.
    * @param  upper     the exclusive upper bound for indexes of elements to keep.
    * @param  builder   the accumulator for kept elements.
    * @return the accumulated elements with indexes greater than or equal to
    *         `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice[To](lower: Int, upper: Int)(implicit builder: Builder[From, A, To]): To =
    macro StrictIndexedSeqOps.slice[A, To]
  
  /** Returns the reverse of this sequence.
    * 
    * @param  builder   the accumulator for reversed elements.
    * @return the elements in this sequence in reverse order.
    * @group  Combining
    */
  def reverse[To](implicit builder: Builder[From, A, To]): To =
    macro StrictIndexedSeqOps.reverse[A, To]
  
  /** Returns pairs of elements from this and another sequence.
    * 
    * @param  that      the container whose elements to pair with these elements.
    * @param  builder   the accumulator for paired elements.
    * @return the accumulated pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B, To](that: IndexedSeq[B])(implicit builder: Builder[From, (A, B), To]): To =
    macro StrictIndexedSeqOps.zip[A, B, To]
  
  /** Returns the concatenation of this and another sequence.
    * 
    * @param  that      the container to append to this sequence.
    * @param  builder   the implicit accumulator for concatenated elements.
    * @return the accumulated elements of both containers.
    * @group  Combining
    */
  def ++ [B >: A, To](that: IndexedSeq[B])(implicit builder: Builder[From, B, To]): To =
    macro StrictIndexedSeqOps.++[A, B, To]
}

private[sequential] object StrictIndexedSeqOps {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[IndexedSeq[A]] = {
    import c.universe._
    val Apply(_, seq :: Nil) = c.prefix.tree
    c.Expr(seq)(IndexedSeqTag[A](c))
  }
  
  def collect[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] =
    new IndexedSeqMacros[c.type](c).collect[A, B, To](unApply(c))(q)(builder)
  
  def map[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => B])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] =
    new IndexedSeqMacros[c.type](c).map[A, B, To](unApply(c))(f)(builder)
  
  def flatMap[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => Enumerator[B]])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] =
    new IndexedSeqMacros[c.type](c).flatMap[A, B, To](unApply(c))(f)(builder)
  
  def filter[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IndexedSeqMacros[c.type](c).filter[A, To](unApply(c))(p)(builder)
  
  def dropWhile[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IndexedSeqMacros[c.type](c).dropWhile[A, To](unApply(c))(p)(builder)
  
  def takeWhile[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IndexedSeqMacros[c.type](c).takeWhile[A, To](unApply(c))(p)(builder)
  
  def span[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builderA: c.Expr[Builder[_, A, To]], builderB: c.Expr[Builder[_, A, To]])
    : c.Expr[(To, To)] =
    new IndexedSeqMacros[c.type](c).span[A, To](unApply(c))(p)(builderA, builderB)
  
  def drop[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IndexedSeqMacros[c.type](c).drop[A, To](unApply(c))(lower)(builder)
  
  def take[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (upper: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IndexedSeqMacros[c.type](c).take[A, To](unApply(c))(upper)(builder)
  
  def slice[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int], upper: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IndexedSeqMacros[c.type](c).slice[A, To](unApply(c))(lower, upper)(builder)
  
  def reverse[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IndexedSeqMacros[c.type](c).reverse[A, To](unApply(c))(builder)
  
  def zip[A : c.WeakTypeTag, B : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (that: c.Expr[IndexedSeq[B]])
      (builder: c.Expr[Builder[_, (A, B), To]])
    : c.Expr[To] =
    new IndexedSeqMacros[c.type](c).zip[A, B, To](unApply(c), that)(builder)
  
  def ++ [A : c.WeakTypeTag, B >: A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (that: c.Expr[IndexedSeq[B]])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] =
    new IndexedSeqMacros[c.type](c).++[B, To](unApply[A](c), that)(builder)
  
  private def IndexedSeqTag[A : c.WeakTypeTag](c: Context): c.WeakTypeTag[IndexedSeq[A]] = {
    import c.{mirror, WeakTypeTag}
    import c.universe._
    WeakTypeTag(appliedType(mirror.staticClass("basis.collections.general.IndexedSeq").toType, weakTypeOf[A] :: Nil))
  }
}
