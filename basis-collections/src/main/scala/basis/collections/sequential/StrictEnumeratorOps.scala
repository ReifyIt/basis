/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

import basis.collections.general._

/** Strictly evaluated enumerator operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
class StrictEnumeratorOps[A, From](val __ : Enumerator[A]) extends AnyVal {
  /** Returns the applications of a partial function to each enumerated element
    * for which the function is defined.
    * 
    * @param  q         the partial function to filter elements against and
    *                   to apply to applicable elements.
    * @param  builder   the implicit accumulator for mapped elements.
    * @return the accumulated elements filtered and mapped by `q`.
    * @group  Mapping
    */
  def collect[B, To](q: PartialFunction[A, B])(implicit builder: Builder[From, B, To]): To = {
    traverse(__)(new StrictEnumeratorOps.CollectInto(q, builder))
    builder.state
  }
  
  /** Returns the applications of a function to each enumerated element.
    * 
    * @param  f         the function to apply to each element.
    * @param  builder   the implicit accumulator for mapped elements.
    * @return the accumulated elements mapped by `f`.
    * @group  Mapping
    */
  def map[B, To](f: A => B)(implicit builder: Builder[From, B, To]): To = {
    traverse(__)(new StrictEnumeratorOps.MapInto(f, builder))
    builder.state
  }
  
  /** Returns the concatenation of all elements returned by a function applied
    * to each enumerated element.
    * 
    * @param  f         the enumerator-yielding function to apply to each element.
    * @param  builder   the implicit accumulator for flattened elements.
    * @return the concatenation of all enumerators produced by `f`.
    * @group  Mapping
    */
  def flatMap[B, To](f: A => Enumerator[B])(implicit builder: Builder[From, B, To]): To = {
    traverse(__)(new StrictEnumeratorOps.FlatMapInto(f, builder))
    builder.state
  }
  
  /** Returns all enumerated elements that satisfy a predicate.
    * 
    * @param  p         the predicate to test elements against.
    * @param  builder   the implicit accumulator for filtered elements.
    * @return the accumulated elements filtered by `p`.
    * @group  Filtering
    */
  def filter[To](p: A => Boolean)(implicit builder: Builder[From, A, To]): To = {
    traverse(__)(new StrictEnumeratorOps.FilterInto(p, builder))
    builder.state
  }
  
  /** Returns the concatenation of this and another enumerator.
    * 
    * @param  that      the enumerator to append to this enumerator.
    * @param  builder   the implicit accumulator for concatenated elements.
    * @return the accumulated elements of both enumerators.
    * @group  Combining
    */
  def ++ [B >: A, To](that: Enumerator[B])(implicit builder: Builder[From, B, To]): To = {
    val f = new StrictEnumeratorOps.AddInto(builder)
    traverse(__)(f)
    traverse(that)(f)
    builder.state
  }
}

private[sequential] object StrictEnumeratorOps {
  import scala.runtime.AbstractFunction1
  
  final class CollectInto[-A, B](q: PartialFunction[A, B], builder: Builder[_, B, _]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) builder += q(x)
  }
  
  final class MapInto[-A, +B](f: A => B, builder: Builder[_, B, _]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = builder += f(x)
  }
  
  final class FlatMapInto[-A, +B](f: A => Enumerator[B], builder: Builder[_, B, _]) extends AbstractFunction1[A, Unit] {
    private[this] val add = new AddInto(builder)
    override def apply(x: A): Unit = traverse(f(x))(add)
  }
  
  final class FilterInto[-A](p: A => Boolean, builder: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) builder += x
  }
  
  final class AddInto[-A](builder: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = builder += x
  }
}
