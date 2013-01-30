/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

import scala.annotation.implicitNotFound

/** A collection builder.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Builders
  * 
  * @groupprio  Inserting   1
  * @groupprio  Removing    2
  * @groupprio  Exporting   3
  * 
  * @define collection  builder
  */
@implicitNotFound("No builder available for element type ${A}.")
trait Builder[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) -A] {
  /** The implicit scope of this $collection.
    * @group Exporting */
  type Scope
  
  /** The type of state maintained by this $collection.
    * @group Exporting */
  type State
  
  /** Appends a single element to this $collection.
    * @group Inserting */
  def append(elem: A): Unit
  
  /** Appends multiple elements to this $collection.
    * @group Inserting */
  def appendAll(elems: Enumerator[A]) {
    traverse(elems)(new Builder.Append(this))
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
  
  /** Returns the current state of this $collection.
    * @group Exporting */
  def state: State
  
  /** Removes all elements from this $collection.
    * @group Removing */
  def clear(): Unit
}

private[collections] object Builder {
  import scala.runtime.AbstractFunction1
  
  final class Append[-A](b: Builder[A]) extends AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = b.append(x)
  }
}
