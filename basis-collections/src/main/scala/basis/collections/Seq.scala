/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** An iterable sequence of elements.
  * 
  * @groupprio  Properties    -5
  * @groupprio  Iterating     -4
  * @groupprio  Traversing    -3
  * @groupprio  Reducing      -2
  * @groupprio  Querying      -1
  * 
  * @define collection  sequence
  */
trait Seq[+A] extends Any with Container[A] {
  override type Self <: Seq[A]
  
  /** Returns `true` if this $collection doesn't contain any elements.
    * @group Properties */
  def isEmpty: Boolean
  
  /** Returns the number of elements in this $collection.
    * @group Properties */
  def length: Int
}
