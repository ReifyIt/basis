/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait IndexedSeq[+A] extends Any with Seq[A] {
  def length: Int
  
  def apply(index: Int): A
  
  override def iterator: Iterator[A] = new IndexedSeq.ForwardIterator(this, 0, length)
  
  override def foreach[U](f: A => U) {
    var i = 0
    val until = length
    while (i < until) {
      f(apply(i))
      i += 1
    }
  }
}

object IndexedSeq {
  import scala.language.implicitConversions
  
  @inline implicit def IndexedSeqOps[A](self: IndexedSeq[A]): IndexedSeqOps[self.Kind, A] =
    new IndexedSeqOps[self.Kind, A](self)
  
  private final class ForwardIterator[+A](self: IndexedSeq[A], lower: Int, upper: Int) extends Iterator[A] {
    private[this] var index: Int = lower
    override def hasNext: Boolean = index < upper
    override def next(): A = {
      if (index >= upper) Iterator.Empty.next()
      val x = self.apply(index)
      index += 1
      x
    }
  }
  
  private final class ReverseIterator[+A](self: IndexedSeq[A], upper: Int, lower: Int) extends Iterator[A] {
    private[this] var index: Int = upper - 1
    override def hasNext: Boolean = index >= lower
    override def next(): A = {
      if (index < lower) Iterator.Empty.next()
      val x = self.apply(index)
      index -= 1
      x
    }
  }
}
