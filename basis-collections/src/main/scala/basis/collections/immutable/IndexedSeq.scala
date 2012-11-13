/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package immutable

/** An immutable indexed sequence.
  * 
  * @groupprio  Examining     -5
  * @groupprio  Iterating     -4
  * @groupprio  Traversing    -3
  * @groupprio  Updating      -2
  * @groupprio  Classifying   -1
  */
trait IndexedSeq[+A]
  extends Any
    with Equals
    with Immutable
    with Family[IndexedSeq[A]]
    with Seq[A]
    with general.IndexedSeq[A] {
  
  /** Returns a copy of this $collection with the given element at the given index.
    * @group Updating */
  def update[B >: A](index: Int, elem: B): IndexedSeq[B]
}
