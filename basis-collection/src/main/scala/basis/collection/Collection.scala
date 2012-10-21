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
  * @groupprio  Traversing    -6
  * @groupprio  Folding       -5
  * @groupprio  Querying      -4
  * @groupprio  Transforming  -3
  * @groupprio  Dividing      -2
  * @groupprio  Expanding     -1
  * 
  * @define collection  collection
  */
trait Collection[+A] extends Any with Enumerator[A] {
  override type Self <: Collection[A]
  
  protected override def foreach[U](f: A => U): Unit
}

object Collection {
  @inline implicit def CollectionOps[A](self: Collection[A]): CollectionOps[self.Self, A] =
    new CollectionOps[self.Self, A](self)
}
