/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package general

/** An accumulator of elements.
  * 
  * @define builder   builder
  */
trait Builder[-From, @specialized(Byte, Short, Int, Long, Float, Double, Boolean) -A, +To] {
  /** Adds a single element to this $builder. */
  def += (x: A): this.type
  
  /** Adds multiple elements to this $builder. */
  def ++= (xs: Enumerator[A]): this.type = {
    xs match {
      case xs: Container[A] =>
        val iter = xs.iterator
        while (!iter.isEmpty) {
          this += iter.head
          iter.step()
        }
      case iter: Iterator[A] =>
        while (!iter.isEmpty) {
          this += iter.head
          iter.step()
        }
      case _ => traverse(xs)(new Builder.AddInto(this))
    }
    this
  }
  
  /** Resets this $builder to its initial state. */
  def clear(): Unit
  
  /** Prepares this $builder to receive a certain number of elements. */
  def expect(count: Int): this.type
  
  /** Returns the current state of this $builder. */
  def state: To
}

private[collections] object Builder {
  import scala.runtime.AbstractFunction1
  
  final class AddInto[-A](builder: Builder[_, A, _]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = builder += x
  }
}
