/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** A temporary buffer of elements.
  * 
  * @groupprio  State       -2
  * @groupprio  Appending   -1
  * 
  * @define buffer  buffer
  */
trait Buffer[-Source, -A] {
  /** The type of state maintained by this $buffer.
    * @group State */
  type State
  
  /** Adds a single element to this $buffer.
    * @group Appending */
  def += (x: A): this.type
  
  /** Adds multiple elements to this $buffer.
    * @group Appending */
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
      case _ => traverse(xs)(new Buffer.AddInto(this))
    }
    this
  }
  
  /** Prepares this $buffer to receive a certain number of elements.
    * @group Appending */
  def expect(count: Int): this.type
  
  /** Returns the current state of this $buffer.
    * @group State */
  def state: State
  
  /** Resets this $buffer to its initial state.
    * @group State */
  def clear(): Unit
}

private object Buffer {
  import scala.runtime.AbstractFunction1
  
  final class AddInto[-A](buffer: Buffer[_, A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = buffer += x
  }
}
