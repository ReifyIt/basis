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
trait Map[A, +Z] extends Any with Container[(A, Z)] {
  override type Self <: Map[A, Z]
  
  override def iterator: Iterator[(A, Z)]
  
  def get(key: A): Option[Z]
}
