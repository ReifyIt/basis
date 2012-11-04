/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** An iterable container of elements.
  * 
  * @groupprio  Examining   -3
  * @groupprio  Iterating   -2
  * @groupprio  Traversing  -1
  * 
  * @define collection  container
  */
trait Container[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) +A]
  extends Any with Family[Container[A]] with Collection[A] {
  
  /** Returns a new iterator over the elements of this $collection.
    * @group Iterating */
  def iterator: Iterator[A]
  
  protected override def foreach[@specialized(Unit) U](f: A => U) {
    val xs = iterator
    while (!xs.isEmpty) { f(xs.head); xs.step() }
  }
}
