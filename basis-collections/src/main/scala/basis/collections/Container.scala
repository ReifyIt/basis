/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

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
    val xs = iterator
    if (!xs.isEmpty) {
      s.append(xs.head)
      xs.step()
      while (!xs.isEmpty) {
        s.append(", ").append(xs.head)
        xs.step()
      }
    }
    s.append(')')
    s.toString
  }
}
