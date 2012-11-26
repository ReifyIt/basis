/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package mutable

/** A mutable sequence of elements.
  * 
  * @groupprio  Examining     -5
  * @groupprio  Modifying     -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  sequence
  */
trait Seq[A]
  extends Any
    with Equals
    with Mutable
    with Family[Seq[A]]
    with Container[A]
    with traversable.Seq[A]
