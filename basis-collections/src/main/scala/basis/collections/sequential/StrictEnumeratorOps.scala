/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential

/** Strictly evaluated enumerator operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
class StrictEnumeratorOps[+Self, +A](val __ : Enumerator[A]) extends AnyVal {
  /** Returns the applications of a partial function to each enumerated element
    * for which the function is defined.
    * 
    * @param  q       the partial function to filter elements against and to
    *                 apply to applicable elements.
    * @param  buffer  the implicit accumulator for mapped elements.
    * @return the accumulated elements filtered and mapped by `q`.
    * @group  Mapping
    */
  def collect[B](q: PartialFunction[A, B])(implicit buffer: Buffer[Self, B]): buffer.State = {
    traverse(__)(new StrictEnumeratorOps.CollectInto(q, buffer))
    buffer.state
  }
  
  /** Returns the applications of a function to each enumerated element.
    * 
    * @param  f       the function to apply to each element.
    * @param  buffer  the implicit accumulator for mapped elements.
    * @return the accumulated elements mapped by `f`.
    * @group  Mapping
    */
  def map[B](f: A => B)(implicit buffer: Buffer[Self, B]): buffer.State = {
    traverse(__)(new StrictEnumeratorOps.MapInto(f, buffer))
    buffer.state
  }
  
  /** Returns the concatenation of all elements returned by a function applied
    * to each enumerated element.
    * 
    * @param  f       the enumerator-yielding function to apply to each element.
    * @param  buffer  the implicit accumulator for flattened elements.
    * @return the concatenation of all enumerators produced by `f`.
    * @group  Mapping
    */
  def flatMap[B](f: A => Enumerator[B])(implicit buffer: Buffer[Self, B]): buffer.State = {
    traverse(__)(new StrictEnumeratorOps.FlatMapInto(f, buffer))
    buffer.state
  }
  
  /** Returns all enumerated elements that satisfy a predicate.
    * 
    * @param  p       the predicate to test elements against.
    * @param  buffer  the implicit accumulator for filtered elements.
    * @return the accumulated elements filtered by `p`.
    * @group  Filtering
    */
  def filter(p: A => Boolean)(implicit buffer: Buffer[Self, A]): buffer.State = {
    traverse(__)(new StrictEnumeratorOps.FilterInto(p, buffer))
    buffer.state
  }
  
  /** Returns the concatenation of this and another enumerator.
    * 
    * @param  that    the enumerator to append to this enumerator.
    * @param  buffer  the implicit accumulator for concatenated elements.
    * @return the accumulated elements of both enumerators.
    * @group  Combining
    */
  def ++ [B >: A](that: Enumerator[B])(implicit buffer: Buffer[Self, B]): buffer.State = {
    val f = new StrictEnumeratorOps.AddInto(buffer)
    traverse(__)(f)
    traverse(that)(f)
    buffer.state
  }
}

private[sequential] object StrictEnumeratorOps {
  import scala.runtime.AbstractFunction1
  
  final class CollectInto[-A, B](q: PartialFunction[A, B], buffer: Buffer[_, B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (q.isDefinedAt(x)) buffer += q(x)
  }
  
  final class MapInto[-A, +B](f: A => B, buffer: Buffer[_, B]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = buffer += f(x)
  }
  
  final class FlatMapInto[-A, +B](f: A => Enumerator[B], buffer: Buffer[_, B]) extends AbstractFunction1[A, Unit] {
    private[this] val add = new AddInto(buffer)
    override def apply(x: A): Unit = traverse(f(x))(add)
  }
  
  final class FilterInto[-A](p: A => Boolean, buffer: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = if (p(x)) buffer += x
  }
  
  final class AddInto[-A](buffer: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = buffer += x
  }
}
