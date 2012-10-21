/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

/** A once traversable enumeration of elements. Enumerator declares only a
  * protected `foreach` method; it has no public methods. Import
  * [[basis.collection.EnumeratorOps]] to extend this interface with a rich
  * suite of collection operations.
  * 
  * @groupprio  Traversing    -5
  * @groupprio  Folding       -4
  * @groupprio  Querying      -3
  * @groupprio  Transforming  -2
  * @groupprio  Expanding     -1
  * 
  * @define collection  enumeration
  */
trait Enumerator[+A] extends Any {
  type Self <: Enumerator[A]
  
  /** Applies a function to each element of this $collection. The protected
    * status of `foreach` allows optimized static implementations to shadow
    * the virtual method. To force a virtual `foreach` call, invoke
    * [[basis.collection.Enumerator#traverse Enumerator.traverse]].
    */
  protected def foreach[U](f: A => U): Unit
}

/** `Enumerator` utilities. */
object Enumerator {
  /** Applies a function to each of an enumerator's elements by invoking the
    * enumerator's protected `foreach` method. */
  def traverse[A, U](self: Enumerator[A])(f: A => U): Unit = self.foreach[U](f)
  
  @inline implicit def EnumeratorOps[A](self: Enumerator[A]): EnumeratorOps[self.Self, A] =
    new EnumeratorOps[self.Self, A](self)
}
