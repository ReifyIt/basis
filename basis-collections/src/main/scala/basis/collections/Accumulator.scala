/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** An element accumulator.
  * 
  * @define collection  accumulator
  */
trait Accumulator[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) -A] {
  /** Adds a single element to this $collection.
    * @group Inserting */
  def += (elem: A): this.type
  
  /** Adds multiple elements to this $collection.
    * @group Inserting */
  def ++= (elems: Enumerator[A]): this.type = {
    traverse(elems)(new Accumulator.Append(this))
    this
  }
}

private[collections] object Accumulator {
  import scala.runtime.AbstractFunction1
  
  final class Append[-A](b: Accumulator[A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = b += x
  }
}
