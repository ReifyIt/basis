/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

trait Array[+A] extends Any with Seq[A] {
  override type Self <: Array[A]
  
  def length: Int
  
  def apply(index: Int): A
  
  override def iterator: Iterator[A] = new ArrayIterator(this, 0, length)
  
  override protected def foreach[U](f: A => U) {
    var i = 0
    val n = length
    while (i < n) {
      f(this(i))
      i += 1
    }
  }
}

private[basis] final class ArrayIterator[+A](xs: Array[A], from: Int, until: Int) extends Iterator[A] {
  private[this] var lower: Int = scala.math.max(0, from)
  private[this] var upper: Int = scala.math.min(scala.math.max(lower, until), xs.length)
  private[this] var index: Int = lower
  
  override def hasNext: Boolean = index < upper
  
  override def next(): A = {
    if (index >= upper) throw new scala.NoSuchElementException("empty iterator")
    val x = xs(index)
    index += 1
    x
  }
}
