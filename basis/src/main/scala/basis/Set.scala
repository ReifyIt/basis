/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** A unique set of iterable values.
  * 
  * @author Chris Sachs
  * 
  * @define collection  set
  */
trait Set[A] extends Any with Container[A] {
  override type Self <: Set[A]
  
  override def iterator: Iterator[A]
  
  def contains(key: A): Boolean
  
  def + (element: A): Self
  
  def - (element: A): Self
}
