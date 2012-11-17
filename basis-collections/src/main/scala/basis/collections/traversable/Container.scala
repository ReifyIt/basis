/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package traversable

/** An iterable container of elements.
  * 
  * @groupprio  Examining     -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  container
  */
trait Container[+A] extends Any with Family[Container[A]] with Collection[A] {
  /** Returns a new iterator over the elements of this $collection.
    * @group Iterating */
  def iterator: Iterator[A]
  
  protected override def foreach[U](f: A => U) {
    val xs = iterator
    while (!xs.isEmpty) { f(xs.head); xs.step() }
  }
  
  override def toString: String = {
    val s = new java.lang.StringBuilder(stringPrefix)
    s.append('(')
    val these = iterator
    if (!these.isEmpty) {
      s.append(these.head)
      these.step()
      while (!these.isEmpty) {
        s.append(", ").append(these.head)
        these.step()
      }
    }
    s.append(')')
    s.toString
  }
}
