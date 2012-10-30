/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** An iterable sequence of elements.
  * 
  * @groupprio  Examining   -3
  * @groupprio  Iterating   -2
  * @groupprio  Traversing  -1
  * 
  * @define collection  sequence
  */
trait Seq[+A] extends Any with Container[A] {
  override type Self <: Seq[A]
  
  /** Returns `true` if this $collection doesn't contain any elements.
    * @group Examining */
  def isEmpty: Boolean
  
  /** Returns the number of elements in this $collection.
    * @group Examining */
  def length: Int
}

/** A generic sequence factory. */
object Seq {
  def apply[A](xs: A*)(implicit buffer: Buffer[Seq[_], A]): buffer.State =
    macro FactoryMacros.apply[A]
  
  def fill[A](count: Int)(element: => A)(implicit buffer: Buffer[Seq[_], A]): buffer.State =
    macro FactoryMacros.fill[A]
  
  def tabulate[A](count: Int)(f: Int => A)(implicit buffer: Buffer[Seq[_], A]): buffer.State =
    macro FactoryMacros.tabulate[A]
  
  def iterate[A](start: A, count: Int)(f: A => A)(implicit buffer: Buffer[Seq[_], A]): buffer.State =
    macro FactoryMacros.iterate[A]
}
