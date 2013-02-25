/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

/** Strictly evaluated array operations.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Strict
  * 
  * @groupprio  Mapping     1
  * @groupprio  Filtering   2
  * @groupprio  Combining   3
  * 
  * @define collection  array
  */
final class StrictArrayOps[A, -From](these: Array[A]) {
  /** Returns the applications of a partial function to each element in this
    * $collection for which the function is defined.
    * 
    * @param  q         the partial function to filter and transform elements.
    * @param  builder   the implicit accumulator for collected elements.
    * @return the accumulated elements filtered and transformed by `q`.
    * @group  Mapping
    */
  def collect[B](q: PartialFunction[A, B])(implicit builder: Builder[B] { type Scope <: From }): builder.State =
    macro StrictArrayOps.collect[A, B]
  
  /** Returns the applications of a function to each element in this $collection.
    * 
    * @param  f         the function to apply to each element.
    * @param  builder   the implicit accumulator for transformed elements.
    * @return the accumulated elements transformed by `f`.
    * @group  Mapping
    */
  def map[B](f: A => B)(implicit builder: Builder[B] { type Scope <: From }): builder.State =
    macro StrictArrayOps.map[A, B]
  
  /** Returns the concatenation of all elements returned by a function applied
    * to each element in this $collection.
    * 
    * @param  f         the enumerator-yielding function to apply to each element.
    * @param  builder   the implicit accumulator for flattened elements.
    * @return the concatenation of all accumulated elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => Enumerator[B])(implicit builder: Builder[B] { type Scope <: From }): builder.State =
    macro StrictArrayOps.flatMap[A, B]
  
  /** Returns all elements in this $collection that satisfy a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for filtered elements.
    * @return the accumulated elements filtered by `p`.
    * @group  Filtering
    */
  def filter(p: A => Boolean)(implicit builder: Builder[A] { type Scope <: From }): builder.State =
    macro StrictArrayOps.filter[A]
  
  /** Returns a view of all elements in this $collection that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def withFilter(p: A => Boolean): Index[A] =
    new NonStrictArrayOps.Filter(these, p)
  
  /** Returns all elements following the longest prefix of this $collection
    * for which each element satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for non-dropped elements.
    * @return the suffix of accumulated elements beginning with the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: A => Boolean)(implicit builder: Builder[A] { type Scope <: From }): builder.State =
    macro StrictArrayOps.dropWhile[A]
  
  /** Returns the longest prefix of this $collection for which each element
    * satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for taken elements.
    * @return the longest prefix of accumulated elements preceding the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: A => Boolean)(implicit builder: Builder[A] { type Scope <: From }): builder.State =
    macro StrictArrayOps.takeWhile[A]
  
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
  //FIXME: SI-6447
  //def span(p: A => Boolean)
  //    (implicit builder1: Builder[A] { type Scope <: From }, builder2: Builder[A] { type Scope <: From })
  //  : (builder1.State, builder2.State) =
  //  macro StrictArrayOps.span[A]
  
  /** Returns all elements in this $collection following a prefix up to some length.
    * 
    * @param  lower     the length of the prefix to drop; also the inclusive
    *                   lower bound for indexes of elements to keep.
    * @param  builder   the implicit accumulator for non-dropped elements.
    * @return all but the first `lower` accumulated elements.
    * @group  Filtering
    */
  def drop(lower: Int)(implicit builder: Builder[A] { type Scope <: From }): builder.State =
    macro StrictArrayOps.drop[A]
  
  /** Returns a prefix of this $collection up to some length.
    * 
    * @param  upper     the length of the prefix to take; also the exclusive
    *                   upper bound for indexes of elements to keep.
    * @param  builder   the implicit accumulator for taken elements.
    * @return up to the first `upper` accumulated elements.
    * @group  Filtering
    */
  def take(upper: Int)(implicit builder: Builder[A] { type Scope <: From }): builder.State =
    macro StrictArrayOps.take[A]
  
  /** Returns an interval of elements in this $collection.
    * 
    * @param  lower     the inclusive lower bound for indexes of elements to keep.
    * @param  upper     the exclusive upper bound for indexes of elements to keep.
    * @param  builder   the implicit accumulator for kept elements.
    * @return the accumulated elements with indexes greater than or equal to
    *         `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int)(implicit builder: Builder[A] { type Scope <: From }): builder.State =
    macro StrictArrayOps.slice[A]
  
  /** Returns the reverse of this $collection.
    * 
    * @param  builder   the implicit accumulator for reversed elements.
    * @return the elements in this $collection in reverse order.
    * @group  Combining
    */
  def reverse(implicit builder: Builder[A] { type Scope <: From }): builder.State =
    macro StrictArrayOps.reverse[A]
  
  /** Returns pairs of elements from this and another $collection.
    * 
    * @param  those     the $collection whose elements to pair with these elements.
    * @param  builder   the implicit accumulator for paired elements.
    * @return the accumulated pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B](those: Array[B])(implicit builder: Builder[(A, B)] { type Scope <: From }): builder.State =
    macro StrictArrayOps.zip[A, B]
  
  /** Returns a copy of this $collection with an appended element.
    * 
    * @param  elem      the element to append to these elements.
    * @param  builder   the implicit accumulator for concatenated elements.
    * @return these elements with `elem` appended.
    * @group  Combining
    */
  def :+ (elem: A)(implicit builder: ArrayBuilder[A] { type Scope <: From }): builder.State =
    macro StrictArrayOps.:+[A]
  
  /** Returns a copy of this $collection with a prepended element.
    * 
    * @param  elem      the element to prepend to these elements.
    * @param  builder   the implicit accumulator for concatenated elements.
    * @return these elements with `elem` prepended.
    * @group  Combining
    */
  def +: (elem: A)(implicit builder: ArrayBuilder[A] { type Scope <: From }): builder.State =
    macro StrictArrayOps.+:[A]
  
  /** Returns the concatenation of this and another array.
    * 
    * @param  those     the array to append to these elements.
    * @param  builder   the implicit accumulator for concatenated elements.
    * @return the accumulated elements of both arrays.
    * @group  Combining
    */
  def ++ (those: Array[A])(implicit builder: ArrayBuilder[A] { type Scope <: From }): builder.State =
    macro StrictArrayOps.++[A]
}

private[sequential] object StrictArrayOps {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Array[A]] = {
    import c.{Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag}
    import c.universe._
    val Apply(_, these :: Nil) = prefix.tree
    implicit val ArrayATag =
      WeakTypeTag[Array[A]](
        appliedType(
          mirror.staticClass("scala.Array").toType,
          weakTypeOf[A] :: Nil))
    Expr[Array[A]](typeCheck(these, weakTypeOf[Array[A]]))
  }
  
  def collect[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
      (builder: c.Expr[Builder[B]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).collect[A, B](unApply[A](c))(q)(builder)
  
  def map[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => B])
      (builder: c.Expr[Builder[B]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).map[A, B](unApply[A](c))(f)(builder)
  
  def flatMap[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => Enumerator[B]])
      (builder: c.Expr[Builder[B]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).flatMap[A, B](unApply[A](c))(f)(builder)
  
  def filter[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).filter[A](unApply[A](c))(p)(builder)
  
  def dropWhile[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).dropWhile[A](unApply[A](c))(p)(builder)
  
  def takeWhile[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).takeWhile[A](unApply[A](c))(p)(builder)
  
  def span[A : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder1: c.Expr[Builder[A]], builder2: c.Expr[Builder[A]])
    : c.Expr[(builder1.value.State, builder2.value.State)] =
    new ArrayMacros[c.type](c).span[A](unApply[A](c))(p)(builder1, builder2)
  
  def drop[A : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).drop[A](unApply[A](c))(lower)(builder)
  
  def take[A : c.WeakTypeTag]
      (c: Context)
      (upper: c.Expr[Int])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).take[A](unApply[A](c))(upper)(builder)
  
  def slice[A : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int], upper: c.Expr[Int])
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).slice[A](unApply[A](c))(lower, upper)(builder)
  
  def reverse[A : c.WeakTypeTag]
      (c: Context)
      (builder: c.Expr[Builder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).reverse[A](unApply[A](c))(builder)
  
  def zip[A : c.WeakTypeTag, B : c.WeakTypeTag]
      (c: Context)
      (those: c.Expr[Array[B]])
      (builder: c.Expr[Builder[(A, B)]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).zip[A, B](unApply[A](c), those)(builder)
  
  def :+ [A : c.WeakTypeTag]
      (c: Context)
      (elem: c.Expr[A])
      (builder: c.Expr[ArrayBuilder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).:+[A](unApply[A](c), elem)(builder)
  
  def +: [A : c.WeakTypeTag]
      (c: Context)
      (elem: c.Expr[A])
      (builder: c.Expr[ArrayBuilder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).+:[A](elem, unApply[A](c))(builder)
  
  def ++ [A : c.WeakTypeTag]
      (c: Context)
      (those: c.Expr[Array[A]])
      (builder: c.Expr[ArrayBuilder[A]])
    : c.Expr[builder.value.State] =
    new ArrayMacros[c.type](c).++[A](unApply[A](c), those)(builder)
}