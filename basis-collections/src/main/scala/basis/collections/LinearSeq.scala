/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** A linear sequence of elements.
  * 
  * @groupprio  Examining   -3
  * @groupprio  Iterating   -2
  * @groupprio  Traversing  -1
  */
trait LinearSeq[+A] extends Any with Seq[A] {
  override type Self <: LinearSeq[A]
  
  /** Returns the first element of this $collection.
    * @group Examining */
  def head: A
  
  /** Returns all except the first element of this $collection.
    * @group Examining */
  def tail: LinearSeq[A]
  
  override def length: Int = {
    var xs = this
    var count = 0
    while (!xs.isEmpty) {
      count += 1
      xs = xs.tail
    }
    count
  }
  
  override def iterator: Iterator[A] =
    new LinearSeq.Cursor(this)
  
  protected override def foreach[U](f: A => U) {
    var xs = this
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }
}

/** A generic linear sequence factory. */
object LinearSeq {
  def apply[A](xs: A*)(implicit buffer: Buffer[LinearSeq[_], A]): buffer.State =
    macro FactoryMacros.apply[A]
  
  def fill[A](count: Int)(element: => A)(implicit buffer: Buffer[LinearSeq[_], A]): buffer.State =
    macro FactoryMacros.fill[A]
  
  def tabulate[A](count: Int)(f: Int => A)(implicit buffer: Buffer[LinearSeq[_], A]): buffer.State =
    macro FactoryMacros.tabulate[A]
  
  def iterate[A](start: A, count: Int)(f: A => A)(implicit buffer: Buffer[LinearSeq[_], A]): buffer.State =
    macro FactoryMacros.iterate[A]
  
  private[collections] final class Cursor[+A]
      (private[this] var xs: LinearSeq[A])
    extends Iterator[A] {
    
    override def isEmpty: Boolean = xs.isEmpty
    
    override def head: A = if (isEmpty) Iterator.Empty.head else xs.head
    
    override def step(): Unit = if (isEmpty) Iterator.Empty.step() else xs = xs.tail
    
    override def dup: Iterator[A] = new Cursor(xs)
  }
}
