/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** A unique set of iterable keys with associated values.
  * 
  * @author Chris Sachs
  * 
  * @define collection  map
  */
trait Map[A, +T] extends Any with Container[(A, T)] {
  override type Self <: Map[A, T]
  
  /** Returns `true` if this $collection contains no elements. */
  def isEmpty: Boolean
  
  def size: Int
  
  def contains(key: A): Boolean
  
  def apply(key: A): T
  
  def get(key: A): Option[T]
  
  def + [U >: T](key: A, value: U): Map[A, U]
  
  def - (key: A): Map[A, T]
}
