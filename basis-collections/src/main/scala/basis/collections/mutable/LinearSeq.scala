/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package mutable

/** A mutable linear sequence.
  * 
  * @groupprio  Examining     -5
  * @groupprio  Modifying     -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  sequence
  */
trait LinearSeq[A]
  extends Equals
    with Mutable
    with Family[LinearSeq[A]]
    with Seq[A]
    with traversable.LinearSeq[A] {
  
  /** Sets the first element of this non-empty $collection.
    * @group Modifying */
  def head_= (elem: A): Unit
}
