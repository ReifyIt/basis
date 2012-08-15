/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait LinearSeq[+A] extends Any with Seq[A] {
  def isEmpty: Boolean
  
  def head: A
  
  def tail: LinearSeq[A]
  
  override def iterator: Iterator[A] = new LinearSeq.ForwardIterator[A](this)
  
  override def foreach[U](f: A => U) {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }
}

object LinearSeq {
  import scala.language.implicitConversions
  
  @inline implicit def LinearSeqOps[A](self: LinearSeq[A]): LinearSeqOps[self.Kind, A] =
    new LinearSeqOps[self.Kind, A](self)
  
  private final class ForwardIterator[+A](private[this] var these: LinearSeq[A]) extends Iterator[A] {
    override def hasNext: Boolean = !these.isEmpty
    override def next(): A = {
      if (these.isEmpty) Iterator.Empty.next()
      val x = these.head
      these = these.tail
      x
    }
  }
}
