/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

/** A traversable collection of elements. Collection declares only a protected
  * `foreach` method; it has no public methods. Import
  * [[basis.collection.CollectionOps]] to extend this interface with a full
  * suite of collection operations.
  * 
  * @author Chris Sachs
  * 
  * @define collection  collection
  */
trait Collection[+A] extends Any with Enumerator[A] {
  override type Self <: Collection[A]
  
  protected override def foreach[U](f: A => U): Unit
}
