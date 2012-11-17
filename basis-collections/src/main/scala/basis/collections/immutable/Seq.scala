/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package immutable

/** An immutable sequence of elements.
  * 
  * @groupprio  Examining     -5
  * @groupprio  Iterating     -4
  * @groupprio  Traversing    -3
  * @groupprio  Updating      -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  sequence
  */
trait Seq[+A]
  extends Any
    with Equals
    with Immutable
    with Family[Seq[A]]
    with Container[A]
    with traversable.Seq[A] {
  
  /** Returns a copy of this $collection with the given element appended.
    * @group Updating */
  def :+ [B >: A](elem: B): Seq[B]
  
  /** Returns a copy of this $collection with the given element prepended.
    * @group Updating */
  def +: [B >: A](elem: B): Seq[B]
}
