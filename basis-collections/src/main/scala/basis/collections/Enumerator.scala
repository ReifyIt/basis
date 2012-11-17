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
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  enumerator
  */
trait Enumerator[+A] extends Any with Family[Enumerator[A]] {
  /** Applies a function to each element of this $collection. The protected
    * status of `foreach` allows optimized static implementations to shadow
    * this virtual method. To force a virtual `foreach` call, invoke
    * [[basis.collections#traverse traverse]].
    * 
    * @group  Traversing
    */
  protected def foreach[U](f: A => U): Unit
}

private[collections] object Enumerator {
  /** Applies a function to each of an enumerator's elements by invoking the
    * enumerator's protected `foreach` method. */
  def traverse[A, U](xs: Enumerator[A])(f: A => U): Unit = xs.foreach[U](f)
}
