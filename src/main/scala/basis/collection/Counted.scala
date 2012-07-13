/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Counted[A] extends Any with Iterated[A] with Countable[A] {
  override def iterator: Iterator[A]
  
  override def contains(element: A): Boolean
  
  def + (element: A)(implicit builder: Collector[Scope, A]): builder.Product = {
    val iter = iterator
    while (iter.hasNext) builder += iter.next()
    if (!contains(element)) builder += element
    builder.result
  }
  
  def - (element: A)(implicit builder: Collector[Scope, A]): builder.Product = {
    val iter = iterator
    while (iter.hasNext) {
      val head = iter.next()
      if (head != element) builder += head
    }
    builder.result
  }
  
  def | (that: Countable[A])(implicit builder: Collector[Scope, A]): builder.Product = {
    var iter = iterator
    while (iter.hasNext) builder += iter.next()
    iter = that.iterator
    while (iter.hasNext) {
      val head = iter.next()
      if (!contains(head)) builder += head
    }
    builder.result
  }
  
  def & (that: Countable[A])(implicit builder: Collector[Scope, A]): builder.Product = {
    val iter = iterator
    while (iter.hasNext) {
      val head = iter.next()
      if (that.contains(head)) builder += head
    }
    builder.result
  }
  
  def &~ (that: Countable[A])(implicit builder: Collector[Scope, A]): builder.Product = {
    val iter = iterator
    while (iter.hasNext) {
      val head = iter.next()
      if (!that.contains(head)) builder += head
    }
    builder.result
  }
  
  override def eagerly: Counted[A] = this
}

private[basis] object Counted {
  private[basis] abstract class Abstractly[A] extends Countable.Abstractly[A] with Counted[A]
  
  private[basis] final class Projected[A](self: Countable[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator
    override def contains(element: A): Boolean = self.contains(element)
  }
}
