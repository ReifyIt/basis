/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential
package strict

import basis.collections._
import basis.collections.traversable._

/** Strictly evaluated container operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
final class ContainerOps[+A, +From] {
  /** Returns the applications of a partial function to each element in this
    * container for which the function is defined.
    * 
    * @param  q         the partial function to filter and map elements.
    * @param  builder   the implicit accumulator for mapped elements.
    * @return the accumulated elements filtered and mapped by `q`.
    * @group  Mapping
    */
  def collect[B, To](q: PartialFunction[A, B])(implicit builder: Builder[From, B, To]): To =
    macro ContainerOps.collect[A, B, To]
  
  /** Returns the applications of a function to each element in this container.
    * 
    * @param  f         the function to apply to each element.
    * @param  builder   the implicit accumulator for mapped elements.
    * @return the accumulated elements mapped by `f`.
    * @group  Mapping
    */
  def map[B, To](f: A => B)(implicit builder: Builder[From, B, To]): To =
    macro ContainerOps.map[A, B, To]
  
  /** Returns the concatenation of all elements returned by a function applied
    * to each element in this container.
    * 
    * @param  f         the enumerator-yielding function to apply to each element.
    * @param  builder   the implicit accumulator for flattened elements.
    * @return the concatenation of all accumulated elements produced by `f`.
    * @group  Mapping
    */
  def flatMap[B, To](f: A => Enumerator[B])(implicit builder: Builder[From, B, To]): To =
    macro ContainerOps.flatMap[A, B, To]
  
  /** Returns all elements in this container that satisfy a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for filtered elements.
    * @return the accumulated elements filtered by `p`.
    * @group  Filtering
    */
  def filter[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro ContainerOps.filter[A, To]
  
  /** Returns all elements following the longest prefix of this container
    * for which each element satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for non-dropped elements.
    * @return the suffix of accumulated elements beginning with the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro ContainerOps.dropWhile[A, To]
  
  /** Returns the longest prefix of this container for which each element
    * satisfies a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for taken elements.
    * @return the longest prefix of accumulated elements preceding the first
    *         element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To =
    macro ContainerOps.takeWhile[A, To]
  
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
    macro ContainerOps.span[A, To]
  
  /** Returns all elements in this container following a prefix up to some length.
    * 
    * @param  lower     the length of the prefix to drop; also the inclusive
    *                   lower bound for indexes of elements to keep.
    * @param  builder   the accumulator for non-dropped elements.
    * @return all but the first `lower` accumulated elements.
    * @group  Filtering
    */
  def drop[To](lower: Int)(implicit builder: Builder[From, A, To]): To =
    macro ContainerOps.drop[A, To]
  
  /** Returns a prefix of this container up to some length.
    * 
    * @param  upper     the length of the prefix to take; also the exclusive
    *                   upper bound for indexes of elements to keep.
    * @param  builder   the accumulator for taken elements.
    * @return up to the first `upper` accumulated elements.
    * @group  Filtering
    */
  def take[To](upper: Int)(implicit builder: Builder[From, A, To]): To =
    macro ContainerOps.take[A, To]
  
  /** Returns an interval of elements in this container.
    * 
    * @param  lower     the inclusive lower bound for indexes of elements to keep.
    * @param  upper     the exclusive upper bound for indexes of elements to keep.
    * @param  builder   the accumulator for kept elements.
    * @return the accumulated elements with indexes greater than or equal to
    *         `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice[To](lower: Int, upper: Int)(implicit builder: Builder[From, A, To]): To =
    macro ContainerOps.slice[A, To]
  
  /** Returns pairs of elements from this and another container.
    * 
    * @param  those     the container whose elements to pair with these elements.
    * @param  builder   the accumulator for paired elements.
    * @return the accumulated pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B, To](those: Container[B])(implicit builder: Builder[From, (A, B), To]): To =
    macro ContainerOps.zip[A, B, To]
  
  /** Returns the concatenation of this and another container.
    * 
    * @param  those     the container to append to these elements.
    * @param  builder   the accumulator for concatenated elements.
    * @return the accumulated elements of both containers.
    * @group  Combining
    */
  def ++ [B >: A, To](those: Container[B])(implicit builder: Builder[From, B, To]): To =
    macro ContainerOps.++[B, To]
}

private[strict] object ContainerOps {
  import scala.collection.immutable.{::, Nil}
  import scala.reflect.macros.Context
  
  private def unApply[A : c.WeakTypeTag](c: Context): c.Expr[Container[A]] = {
    import c.{Expr, mirror, prefix, typeCheck, weakTypeOf, WeakTypeTag}
    import c.universe._
    val Apply(_, container :: Nil) = prefix.tree
    val ContainerTag =
      WeakTypeTag[Container[A]](
        appliedType(
          mirror.staticClass("basis.collections.traversable.Container").toType,
          weakTypeOf[A] :: Nil))
    Expr(c.typeCheck(container, ContainerTag.tpe))(ContainerTag)
  }
  
  private def iterator[A : c.WeakTypeTag](c: Context)(container: c.Expr[Container[A]]): c.Expr[Iterator[A]] = {
    import c.{Expr, mirror, typeCheck, weakTypeOf, WeakTypeTag}
    import c.universe._
    val IteratorTag =
      WeakTypeTag[Iterator[A]](
        appliedType(
          mirror.staticClass("basis.collections.Iterator").toType,
          weakTypeOf[A] :: Nil))
    Expr(Select(container.tree, "iterator"))(IteratorTag)
  }
  
  def collect[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (q: c.Expr[PartialFunction[A, B]])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).collect[A, B, To](iterator(c)(unApply[A](c)))(q)(builder)
  
  def map[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => B])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).map[A, B, To](iterator(c)(unApply[A](c)))(f)(builder)
  
  def flatMap[A : c.WeakTypeTag, B, To : c.WeakTypeTag]
      (c: Context)
      (f: c.Expr[A => Enumerator[B]])
      (builder: c.Expr[Builder[_, B, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).flatMap[A, B, To](iterator(c)(unApply[A](c)))(f)(builder)
  
  def filter[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).filter[A, To](iterator(c)(unApply[A](c)))(p)(builder)
  
  def dropWhile[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).dropWhile[A, To](iterator(c)(unApply[A](c)))(p)(builder)
  
  def takeWhile[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).takeWhile[A, To](iterator(c)(unApply[A](c)))(p)(builder)
  
  def span[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (p: c.Expr[A => Boolean])
      (builder1: c.Expr[Builder[_, A, To]], builder2: c.Expr[Builder[_, A, To]])
    : c.Expr[(To, To)] =
    new IteratorMacros[c.type](c).span[A, To](iterator(c)(unApply[A](c)))(p)(builder1, builder2)
  
  def drop[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).drop[A, To](iterator(c)(unApply[A](c)))(lower)(builder)
  
  def take[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (upper: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).take[A, To](iterator(c)(unApply[A](c)))(upper)(builder)
  
  def slice[A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (lower: c.Expr[Int], upper: c.Expr[Int])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).slice[A, To](iterator(c)(unApply[A](c)))(lower, upper)(builder)
  
  def zip[A : c.WeakTypeTag, B : c.WeakTypeTag, To: c.WeakTypeTag]
      (c: Context)
      (those: c.Expr[Container[B]])
      (builder: c.Expr[Builder[_, (A, B), To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).zip[A, B, To](iterator(c)(unApply[A](c)), iterator(c)(those))(builder)
  
  def ++ [A : c.WeakTypeTag, To : c.WeakTypeTag]
      (c: Context)
      (those: c.Expr[Container[A]])
      (builder: c.Expr[Builder[_, A, To]])
    : c.Expr[To] =
    new IteratorMacros[c.type](c).++[A, To](iterator(c)(unApply[A](c)), iterator(c)(those))(builder)
}