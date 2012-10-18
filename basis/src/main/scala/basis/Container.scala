/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** An iterable container of elements. Import [[basis.collection.ContainerOps]]
  * to extend this interface with a full suite of optimized collection operations.
  * 
  * @author Chris Sachs
  * 
  * @define collection  container
  */
trait Container[+A] extends Any with Collection[A] {
  override type Self <: Container[A]
  
  protected override def foreach[U](f: A => U) {
    val xs = iterator
    while (!xs.isEmpty) { f(xs.head); xs.step() }
  }
  
  /** Returns a new iterator over the elements of this $collection. */
  def iterator: Iterator[A]
}

/** `Container` type class implementations. */
object Container {
  /* implicit */ def Show[A : Show]: Show[Container[A]] =
    new ContainerShow[A]("Container")
}

private[basis] class ContainerShow[-A]
    (stringPrefix: String)(implicit A: Show[A])
  extends Show[Container[A]] {
  
  override def show(xs: Container[A])(buffer: CharBuffer) {
    buffer.append(stringPrefix)
    buffer += '('
    val iter = xs.iterator
    if (!iter.isEmpty) {
      A.show(iter.head)(buffer)
      iter.step()
      while (!iter.isEmpty) {
        buffer += ',' += ' '
        A.show(iter.head)(buffer)
        iter.step()
      }
    }
    buffer += ')'
  }
}
