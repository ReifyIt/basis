/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

/** A last-in first-out sequence. The right-associative `::` operator
  * puts an element at the `head` of a stack.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * @group    Collections
  * 
  * @groupprio  Measuring     1
  * @groupprio  Decomposing   2
  * @groupprio  Combining     3
  * @groupprio  Traversing    4
  * @groupprio  Classifying   5
  * 
  * @define collection  stack
  */
trait Stack[+A] extends Any with Equals with Family[Stack[_]] with Side[A] {
  /** Returns this $collection with a prepended element.
    * @group Combining */
  def :: [B >: A](elem: B): Stack[B]
}
