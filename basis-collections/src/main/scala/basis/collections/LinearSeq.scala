/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** A linear sequence of elements.
  * 
  * @groupprio  Examining   -3
  * @groupprio  Iterating   -2
  * @groupprio  Traversing  -1
  */
trait LinearSeq[+A] extends Any with Seq[A] {
  override type Self <: LinearSeq[A]
  
  /** Returns the first element of this $collection.
    * @group Examining */
  def head: A
  
  /** Returns all except the first element of this $collection.
    * @group Examining */
  def tail: LinearSeq[A]
}
