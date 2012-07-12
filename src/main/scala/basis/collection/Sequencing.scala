/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Sequencing[+Scope, +A] extends Any with Iterating[Scope, A] with Sequential[A] {
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
  
  override def eagerly: Sequencing[Scope, A] = this
}

object Sequencing {
  abstract class Abstractly[+Scope, +A] extends Sequential.Abstractly[A] with Sequencing[Scope, A]
  
  final class Projecting[+Scope, +A](self: Sequential[A]) extends Abstractly[Scope, A] {
    override def iterator: Iterator[A] = self.iterator
  }
}
