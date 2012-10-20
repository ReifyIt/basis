/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

/** An iterable sequence of elements. Import [[basis.collection.SeqOps]] to
  * extend this interface with a full suite of optimized collection operations.
  * 
  * @author Chris Sachs
  * 
  * @define collection  sequence
  */
trait Seq[+A] extends Any with Container[A] {
  override type Self <: Seq[A]
  
  /** Returns `true` if this $collection doesn't contain any elements. */
  def isEmpty: Boolean
  
  /** Returns the number of elements in this $collection. */
  def length: Int
}
