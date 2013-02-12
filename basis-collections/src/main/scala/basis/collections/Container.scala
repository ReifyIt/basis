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
  * @version  0.1
  * @since    0.0
  * @group    Collections
  * 
  * @groupprio  Traversing    1
  * @groupprio  Classifying   2
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
    * @group Traversing */
  def iterator: Iterator[A]
  
  override def traverse(f: A => Unit) {
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
