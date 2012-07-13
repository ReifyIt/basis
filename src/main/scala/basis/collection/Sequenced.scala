/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Sequenced[+A] extends Any with Iterated[A] with Sequential[A] {
  override def iterator: Iterator[A]
  
  def :+ [B >: A](element: B)(implicit builder: Collector[Scope, B]): builder.Product = {
    val iter = iterator
    while (iter.hasNext) builder += iter.next()
    builder += element
    builder.result
  }
  
  def +: [B >: A](element: B)(implicit builder: Collector[Scope, B]): builder.Product = {
    builder += element
    val iter = iterator
    while (iter.hasNext) builder += iter.next()
    builder.result
  }
  
  def :++ [B >: A](elements: Incremental[B])(implicit builder: Collector[Scope, B]): builder.Product = {
    val iter = iterator
    while (iter.hasNext) builder += iter.next()
    builder ++= elements
    builder.result
  }
  
  def ++: [B >: A](elements: Incremental[B])(implicit builder: Collector[Scope, B]): builder.Product = {
    builder ++= elements
    val iter = iterator
    while (iter.hasNext) builder += iter.next()
    builder.result
  }
  
  override def eagerly: Sequenced[A] = this
}

private[basis] object Sequenced {
  private[basis] abstract class Abstractly[+A] extends Sequential.Abstractly[A] with Sequenced[A]
  
  private[basis] final class Projected[+A](self: Sequential[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator
  }
}
