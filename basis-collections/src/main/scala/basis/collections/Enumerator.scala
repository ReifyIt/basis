/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** A once traversable enumeration of elements. Enumerator declares only a
  * protected `foreach` method; it has no public methods.
  * 
  * @groupprio  Traversing  -3
  * @groupprio  Reducing    -2
  * @groupprio  Querying    -1
  * 
  * @define collection  enumeration
  */
trait Enumerator[+A] extends Any {
  type Self <: Enumerator[A]
  
  /** Applies a function to each element of this $collection. The protected
    * status of `foreach` allows optimized static implementations to shadow
    * the virtual method. To force a virtual `foreach` call, invoke
    * [[basis.collections#traverse traverse]].
    * 
    * @group  Traversing
    */
  protected def foreach[U](f: A => U): Unit
}

private[collections] object Enumerator {
  /** Applies a function to each of an enumerator's elements by invoking the
    * enumerator's protected `foreach` method. */
  private[collections] def traverse[A, U](xs: Enumerator[A])(f: A => U): Unit = xs.foreach[U](f)
}
