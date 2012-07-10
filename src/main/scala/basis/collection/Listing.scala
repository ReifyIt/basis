/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Listing[+Scope, +A] extends Any with Iterating[Scope, A] with Listable[A] {
  override def isEmpty: Boolean
  
  override def head: A
  
  override def tail: Listable[A]
  
  override def map[B](f: A => B)(implicit builder: Collector[Scope, B]): builder.Product = {
    var rest = this: Listable[A]
    while (!rest.isEmpty) {
      builder += f(rest.head)
      rest = rest.tail
    }
    builder.result
  }
  
  override def flatMap[B](f: A => Incremental[B])(implicit builder: Collector[Scope, B]): builder.Product = {
    var rest = this: Listable[A]
    while (!rest.isEmpty) {
      builder ++= f(rest.head)
      rest = rest.tail
    }
    builder.result
  }
  
  override def filter(p: A => Boolean)(implicit builder: Collector[Scope, A]): builder.Product = {
    var rest = this: Listable[A]
    while (!rest.isEmpty) {
      if (p(rest.head)) builder += rest.head
      rest = rest.tail
    }
    builder.result
  }
  
  override def collect[B](q: PartialFunction[A, B])(implicit builder: Collector[Scope, B]): builder.Product = {
    var rest = this: Listable[A]
    while (!rest.isEmpty) {
      if (q.isDefinedAt(rest.head)) builder += q(rest.head)
      rest = rest.tail
    }
    builder.result
  }
  
  override def eagerly: Listing[Scope, A] = this
}

object Listing {
  abstract class Abstractly[+Scope, +A] extends Listable.Abstractly[A] with Listing[Scope, A]
  
  final class Projecting[+Scope, +A](self: Listable[A]) extends Abstractly[Scope, A] {
    override def isEmpty: Boolean = self.isEmpty
    override def head: A = self.head
    override def tail: Listable[A] = self.tail
  }
}
