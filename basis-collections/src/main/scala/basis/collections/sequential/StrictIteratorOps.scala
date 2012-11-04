/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Strictly evaluated iterator operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
abstract class StrictIteratorOps[A, Family] private[sequential] {
  /** Returns the applications of a partial function to each iterated element
    * for which the function is defined.
    * 
    * @param  q         the partial function to filter elements against and
    *                   to apply to applicable elements.
    * @param  builder   the implicit accumulator for mapped elements.
    * @return the accumulated elements filtered and mapped by `q`.
    * @group  Mapping
    */
  def collect[B, To](q: PartialFunction[A, B])(implicit builder: Builder[Family, B, To]): To =
    macro StrictIteratorOps.collect[A, B, To]
  
  /** Returns the applications of a function to each iterated element.
    * 
    * @param  f         the function to apply to each element.
    * @param  builder   the implicit accumulator for mapped elements.
    * @return the accumulated elements mapped by `f`.
    * @group  Mapping
    */
  def map[B, To](f: A => B)(implicit builder: Builder[Family, B, To]): To =
    macro StrictIteratorOps.map[A, B, To]
  
  /** Returns the concatenation of all elements returned by a function applied
    * to each iterated element.
    * 
    * @param  f         the enumerator-yielding function to apply to each element.
    * @param  builder   the implicit accumulator for flattened elements.
    * @return the concatenation of all accumulated elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B, To](f: A => Enumerator[B])(implicit builder: Builder[Family, B, To]): To =
    macro StrictIteratorOps.flatMap[A, B, To]
  
  /** Returns all iterated elements that satisfy a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for filtered elements.
    * @return the accumulated elements filtered by `p`.
    * @group  Filtering
    */
  def filter[To](p: A => Boolean)(implicit builder: Builder[Family, A, To]): To =
    macro StrictIteratorOps.filter[A, To]
  
  /** Returns all iterated elements following the longest prefix for which
    * each element satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for non-dropped elements.
    * @return the suffix of accumulated elements beginning with the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile[To](p: A => Boolean)(implicit builder: Builder[Family, A, To]): To =
    macro StrictIteratorOps.dropWhile[A, To]
  
  /** Returns the longest prefix for which each iterated element satisfies
    * a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for taken elements.
    * @return the longest prefix of accumulated elements preceding the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile[To](p: A => Boolean)(implicit builder: Builder[Family, A, To]): To =
    macro StrictIteratorOps.takeWhile[A, To]
  
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
      implicit builderA: Builder[Family, A, To],
               builderB: Builder[Family, A, To])
    : (To, To) =
    macro StrictIteratorOps.span[A, To]
  
  /** Returns all iterated elements following a prefix up to some length.
    * 
    * @param  lower     the length of the prefix to drop; also the inclusive
    *                   lower bound for indexes of elements to keep.
    * @param  builder   the accumulator for non-dropped elements.
    * @return all but the first `lower` accumulated elements.
    * @group  Filtering
    */
  def drop[To](lower: Int)(implicit builder: Builder[Family, A, To]): To =
    macro StrictIteratorOps.drop[A, To]
  
  /** Returns a prefix of iterated elements up to some length.
    * 
    * @param  upper     the length of the prefix to take; also the exclusive
    *                   upper bound for indexes of elements to keep.
    * @param  builder   the accumulator for taken elements.
    * @return up to the first `upper` accumulated elements.
    * @group  Filtering
    */
  def take[To](upper: Int)(implicit builder: Builder[Family, A, To]): To =
    macro StrictIteratorOps.take[A, To]
  
  /** Returns an interval of iterated elements.
    * 
    * @param  lower     the inclusive lower bound for indexes of elements to keep.
    * @param  upper     the exclusive upper bound for indexes of elements to keep.
    * @param  builder   the accumulator for kept elements.
    * @return the accumulated elements with indexes greater than or equal to
    *         `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice[To](lower: Int, upper: Int)(implicit builder: Builder[Family, A, To]): To =
    macro StrictIteratorOps.slice[A, To]
  
  /** Returns pairs of elements from this and another iterator.
    * 
    * @param  that      the iterator whose elements to pair with these elements.
    * @param  builder   the accumulator for paired elements.
    * @return the accumulated pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B, To](that: Iterator[B])(implicit builder: Builder[Family, (A, B), To]): To =
    macro StrictIteratorOps.zip[A, B, To]
  
  /** Returns the concatenation of this and another iterator.
    * 
    * @param  that      the iterator to append to this iterator.
    * @param  builder   the implicit accumulator for concatenated elements.
    * @return the accumulated elements of both containers.
    * @group  Combining
    */
  def ++ [B >: A, To](that: Iterator[B])(implicit builder: Builder[Family, B, To]): To =
    macro StrictIteratorOps.++[A, B, To]
}

private[sequential] object StrictIteratorOps {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Iterator[A]] = {
    import c.universe._
    val Apply(_, iterator :: Nil) = c.prefix.tree
    c.Expr(iterator)(IteratorTag[A](c))
  }
  
  def collect[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).collect[A, B, To](unApply(c))(q)(builder)
  
  def map[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => B])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).map[A, B, To](unApply(c))(f)(builder)
  
  def flatMap[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => Enumerator[B]])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).flatMap[A, B, To](unApply(c))(f)(builder)
  
  def filter[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).filter[A, To](unApply(c))(p)(builder)
  
  def dropWhile[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).dropWhile[A, To](unApply(c))(p)(builder)
  
  def takeWhile[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).takeWhile[A, To](unApply(c))(p)(builder)
  
  def span[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builderA: c.Expr[Builder[_, A, To]], builderB: c.Expr[Builder[_, A, To]])
    : c.Expr[(To, To)] =
    new IteratorMacros[c.type](c).span[A, To](unApply(c))(p)(builderA, builderB)
  
  def drop[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).drop[A, To](unApply(c))(lower)(builder)
  
  def take[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (upper: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).take[A, To](unApply(c))(upper)(builder)
  
  def slice[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int], upper: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).slice[A, To](unApply(c))(lower, upper)(builder)
  
  def zip[A : c.WeakTypeTag, B : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (that: c.Expr[Iterator[B]])
      (builder: c.Expr[Builder[_, (A, B), To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).zip[A, B, To](unApply(c), that)(builder)
  
  def ++ [A : c.WeakTypeTag, B >: A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (that: c.Expr[Iterator[B]])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).++[B, To](unApply[A](c), that)(builder)
  
  private def IteratorTag[A : c.WeakTypeTag](c: Context): c.WeakTypeTag[Iterator[A]] = {
    import c.{mirror, WeakTypeTag}
    import c.universe._
    WeakTypeTag(appliedType(mirror.staticClass("basis.collections.Iterator").toType, weakTypeOf[A] :: Nil))
  }
}
