/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** A once traversable enumeration of elements. Enumerator declares only a
  * protected `foreach` method; it has no public methods. Import
  * [[basis.collection.EnumeratorOps]] to extend this interface with a rich
  * suite of collection operations.
  * 
  * @author Chris Sachs
  * 
  * @define collection  enumeration
  */
trait Enumerator[+A] extends Any {
  type Self <: Enumerator[A]
  
  /** Applies a function to each element of this $collection. The protected
    * status of `foreach` allows optimized static implementations to override
    * this virtual method. To force a virtual `foreach` call, invoke the
    * [[traverse]] function.
    */
  protected def foreach[U](f: A => U): Unit
}

private[basis] object Enumerator {
  def traverse[A, U](self: Enumerator[A])(f: A => U): Unit = self.foreach[U](f)
}
