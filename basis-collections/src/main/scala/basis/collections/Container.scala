/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.collections

/** An iterable collection. `Container` declares an `iterator` method that
  * returns a new [[Iterator]] over its elements.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * @group    Collections
  * 
  * @groupprio  Iterating     1
  * @groupprio  Traversing    2
  * @groupprio  Classifying   3
  * 
  * @define collection  container
  * @define SequentialOps
  * The following classes implement the extensions to this interface:
  * 
  *  - [[basis.sequential.GeneralContainerOps GeneralContainerOps]]
  *    implements reductive operations (`foreach`, `fold`, `reduce`, etc.).
  *  - [[basis.sequential.StrictContainerOps StrictContainerOps]]
  *    implements eager transformations (`map`, `flatMap`, `filter`, etc.).
  *  - [[basis.sequential.NonStrictContainerOps NonStrictContainerOps]]
  *    implements lazy transformations (`map`, `flatMap`, `filter`, etc.).
  */
trait Container[+A] extends Any with Family[Container[_]] with Collection[A] {
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
