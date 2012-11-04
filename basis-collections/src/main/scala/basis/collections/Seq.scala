/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** An iterable sequence of elements.
  * 
  * @groupprio  Examining   -3
  * @groupprio  Iterating   -2
  * @groupprio  Traversing  -1
  * 
  * @define collection  sequence
  */
trait Seq[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) +A]
  extends Any with Family[Seq[A]] with Container[A] {
  
  /** Returns `true` if this $collection doesn't contain any elements.
    * @group Examining */
  def isEmpty: Boolean
  
  /** Returns the number of elements in this $collection.
    * @group Examining */
  def length: Int
}
