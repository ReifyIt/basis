/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections

/** An indexed sequence of elements.
  * 
  * @groupprio  Examining   -3
  * @groupprio  Iterating   -2
  * @groupprio  Traversing  -1
  */
trait IndexedSeq[+A] extends Any with Seq[A] {
  override type Self <: IndexedSeq[A]
  
  /** Returns the element at `index`.
    * @group Examining */
  def apply(index: Int): A
  
  override def isEmpty: Boolean = length == 0
  
  override def iterator: Iterator[A] =
    new IndexedSeq.Cursor(this, 0, length)
  
  protected override def foreach[U](f: A => U) {
    var i = 0
    val n = length
    while (i < n) {
      f(apply(i))
      i += 1
    }
  }
}

/** A generic indexed sequence factory. */
object IndexedSeq extends SeqFactory[IndexedSeq] {
  private[collections] final class Cursor[+A]
      (xs: IndexedSeq[A], from: Int, until: Int)
    extends Iterator[A] {
    
    import java.lang.Math.{max, min}
    
    private[this] var lower: Int = max(0, from)
    private[this] var upper: Int = min(max(lower, until), xs.length)
    private[this] var index: Int = lower
    
    override def isEmpty: Boolean = index >= upper
    
    override def head: A = if (isEmpty) Iterator.Empty.head else xs(index)
    
    override def step(): Unit = if (isEmpty) Iterator.Empty.step() else index += 1
    
    override def dup: Iterator[A] = new Cursor[A](xs, index, upper)
  }
}
