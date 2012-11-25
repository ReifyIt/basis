/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential
package strict

/** Strictly evaluated enumerator operations.
  * 
  * @groupprio  Traversing  -6
  * @groupprio  Reducing    -5
  * @groupprio  Querying    -4
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  * 
  * @define collection  enumerator
  */
trait EnumeratorOps[+A, +From] extends Any with general.EnumeratorOps[A, From] {
  /** Returns the applications of a partial function to each element in this
    * $collection for which the function is defined.
    * 
    * @param  q         the partial function to filter and map elements.
    * @param  builder   the implicit accumulator for mapped elements.
    * @return the accumulated elements filtered and mapped by `q`.
    * @group  Mapping
    */
  def collect[B, To](q: PartialFunction[A, B])(implicit builder: Builder[From, B, To]): To =
    macro EnumeratorMacros.collect[A, B, To]
  
  /** Returns the applications of a function to each element in this $collection.
    * 
    * @param  f         the function to apply to each element.
    * @param  builder   the implicit accumulator for mapped elements.
    * @return the accumulated elements mapped by `f`.
    * @group  Mapping
    */
  def map[B, To](f: A => B)(implicit builder: Builder[From, B, To]): To =
    macro EnumeratorMacros.map[A, B, To]
  
  /** Returns the concatenation of all elements returned by a function applied
    * to each element in this $collection.
    * 
    * @param  f         the enumerator-yielding function to apply to each element.
    * @param  builder   the implicit accumulator for flattened elements.
    * @return the concatenation of all accumulated elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B, To](f: A => Enumerator[B])(implicit builder: Builder[From, B, To]): To =
    macro EnumeratorMacros.flatMap[A, B, To]
  
  /** Returns all elements in this $collection that satisfy a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for filtered elements.
    * @return the accumulated elements filtered by `p`.
    * @group  Filtering
    */
  def filter[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro EnumeratorMacros.filter[A, To]
  
  /** Returns all elements following the longest prefix of this $collection
    * for which each element satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for non-dropped elements.
    * @return the suffix of accumulated elements beginning with the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro EnumeratorMacros.dropWhile[A, To]
  
  /** Returns the longest prefix of this $collection for which each element
    * satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for taken elements.
    * @return the longest prefix of accumulated elements preceding the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro EnumeratorMacros.takeWhile[A, To]
  
  /** Returns a (prefix, suffix) pair with the prefix being the longest one for
    * which each element satisfies a predicate, and the suffix beginning with
    * the first element to not satisfy the predicate.
    * 
    * @param  p           the predicate to test elements against.
    * @param  builder1    the implicit accumulator for prefix elements.
    * @param  builder2    the implicit accumilator for suffix elements.
    * @return the pair of accumulated prefix and suffix elements.
    * @group  Filtering
    */
  def span[To](p: A => Boolean)(implicit builder1: Builder[From, A, To], builder2: Builder[From, A, To]): (To, To) =
    macro EnumeratorMacros.span[A, To]
  
  /** Returns all elements in this $collection following a prefix up to some length.
    * 
    * @param  lower     the length of the prefix to drop; also the inclusive
    *                   lower bound for indexes of elements to keep.
    * @param  builder   the accumulator for non-dropped elements.
    * @return all but the first `lower` accumulated elements.
    * @group  Filtering
    */
  def drop[To](lower: Int)(implicit builder: Builder[From, A, To]): To =
    macro EnumeratorMacros.drop[A, To]
  
  /** Returns a prefix of this $collection up to some length.
    * 
    * @param  upper     the length of the prefix to take; also the exclusive
    *                   upper bound for indexes of elements to keep.
    * @param  builder   the accumulator for taken elements.
    * @return up to the first `upper` accumulated elements.
    * @group  Filtering
    */
  def take[To](upper: Int)(implicit builder: Builder[From, A, To]): To =
    macro EnumeratorMacros.take[A, To]
  
  /** Returns an interval of elements in this $collection.
    * 
    * @param  lower     the inclusive lower bound for indexes of elements to keep.
    * @param  upper     the exclusive upper bound for indexes of elements to keep.
    * @param  builder   the accumulator for kept elements.
    * @return the accumulated elements with indexes greater than or equal to
    *         `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice[To](lower: Int, upper: Int)(implicit builder: Builder[From, A, To]): To =
    macro EnumeratorMacros.slice[A, To]
  
  /** Returns the concatenation of this and another enumerator.
    * 
    * @param  those     the elements to append to these elements.
    * @param  builder   the implicit accumulator for concatenated elements.
    * @return the accumulated elements of both enumerators.
    * @group  Combining
    */
  def ++ [B >: A, To](those: Enumerator[B])(implicit builder: Builder[From, B, To]): To =
    macro EnumeratorMacros.++[B, To]
}

private[strict] object EnumeratorMacros {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Enumerator[A]] = {
    import c.universe._
    val Apply(_, enumerator :: Nil) = c.prefix.tree
    val EnumeratorTag = c.weakTypeTag[Enumerator[A]]
    c.Expr(c.typeCheck(enumerator, EnumeratorTag.tpe))(EnumeratorTag)
  }
  
  def collect[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] =
    c.universe.reify(strict.collect(unApply[A](c).splice)(q.splice)(builder.splice))
  
  def map[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => B])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] =
    c.universe.reify(strict.map(unApply[A](c).splice)(f.splice)(builder.splice))
  
  def flatMap[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => Enumerator[B]])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] =
    c.universe.reify(strict.flatMap(unApply[A](c).splice)(f.splice)(builder.splice))
  
  def filter[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    c.universe.reify(strict.filter(unApply[A](c).splice)(p.splice)(builder.splice))
  
  def dropWhile[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    c.universe.reify(strict.dropWhile(unApply[A](c).splice)(p.splice)(builder.splice))
  
  def takeWhile[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    c.universe.reify(strict.takeWhile(unApply[A](c).splice)(p.splice)(builder.splice))
  
  def span[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder1: c.Expr[Builder[_, A, To]], builder2: c.Expr[Builder[_, A, To]])
    : c.Expr[(To, To)] =
    c.universe.reify(strict.span(unApply[A](c).splice)(p.splice)(builder1.splice, builder2.splice))
  
  def drop[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    c.universe.reify(strict.drop(unApply[A](c).splice)(lower.splice)(builder.splice))
  
  def take[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (upper: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    c.universe.reify(strict.take(unApply[A](c).splice)(upper.splice)(builder.splice))
  
  def slice[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int], upper: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    c.universe.reify(strict.slice(unApply[A](c).splice)(lower.splice, upper.splice)(builder.splice))
  
  def ++ [A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (those: c.Expr[Enumerator[A]])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    c.universe.reify((builder.splice ++= unApply[A](c).splice ++= those.splice).state)
}
