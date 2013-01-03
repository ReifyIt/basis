/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

/** An element accumulator.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * @group    Builders
  * 
  * @groupprio  Inserting   1
  * 
  * @define collection  accumulator
  */
trait Accumulator[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) -A] {
  /** Appends a single element to this $collection.
    * @group Inserting */
  def append(elem: A): Unit
  
  /** Appends multiple elements to this $collection.
    * @group Inserting */
  def appendAll(elems: Enumerator[A]) {
    traverse(elems)(new Accumulator.Append(this))
  }
  
  /** Appends a single element to this $collection.
    * @group Inserting */
  def += (elem: A): this.type = {
    append(elem)
    this
  }
  
  /** Appends multiple elements to this $collection.
    * @group Inserting */
  def ++= (elems: Enumerator[A]): this.type = {
    appendAll(elems)
    this
  }
  
  /** Prepares this $collection to receive a certain number of elements.
    * @group Inserting */
  def expect(count: Int): this.type
}

private[collections] object Accumulator {
  import scala.runtime.AbstractFunction1
  
  final class Append[-A](b: Accumulator[A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = b += x
  }
}
