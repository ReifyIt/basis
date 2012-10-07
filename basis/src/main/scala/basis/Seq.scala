/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** An iterable sequence of elements. Import [[basis.collection.SeqOps]] to
  * extend this interface with a rich suite of optimized collection operations.
  * 
  * @author Chris Sachs
  * 
  * @define collection  sequence
  */
trait Seq[+A] extends Any with Container[A] {
  override type Self <: Seq[A]
  
  override def iterator: Iterator[A]
}
