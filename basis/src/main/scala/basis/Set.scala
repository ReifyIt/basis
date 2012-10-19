/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** A unique set of elements. Import [[basis.collection.SetOps]] to extend
  * this interface with a full suite of optimized collection operations.
  * 
  * @author Chris Sachs
  * 
  * @define collection  set
  */
trait Set[A] extends Any with Container[A] {
  override type Self <: Set[A]
  
  /** Returns `true` if this $collection doesn't contain any elements. */
  def isEmpty: Boolean
  
  /** Returns the number of elements in this $collection. */
  def size: Int
  
  /** Returns `true` if this $collection contains the given element. */
  def contains(element: A): Boolean
  
  /** Returns a copy of this $collection containing the given element. */
  def + (element: A): Set[A]
  
  /** Returns a copy of this $collection excluding the given element. */
  def - (element: A): Set[A]
}
