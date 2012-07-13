/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Listed[+A] extends Any with Sequenced[A] with Listable[A] {
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
  
  override def :+ [B >: A](element: B)(implicit builder: Collector[Scope, B]): builder.Product = {
    var rest = this: Listable[A]
    while (!rest.isEmpty) {
      builder += rest.head
      rest = rest.tail
    }
    builder += element
    builder.result
  }
  
  override def +: [B >: A](element: B)(implicit builder: Collector[Scope, B]): builder.Product = {
    builder += element
    var rest = this: Listable[A]
    while (!rest.isEmpty) {
      builder += rest.head
      rest = rest.tail
    }
    builder.result
  }
  
  override def :++ [B >: A](elements: Incremental[B])(implicit builder: Collector[Scope, B]): builder.Product = {
    var rest = this: Listable[A]
    while (!rest.isEmpty) {
      builder += rest.head
      rest = rest.tail
    }
    builder ++= elements
    builder.result
  }
  
  override def ++: [B >: A](elements: Incremental[B])(implicit builder: Collector[Scope, B]): builder.Product = {
    builder ++= elements
    var rest = this: Listable[A]
    while (!rest.isEmpty) {
      builder += rest.head
      rest = rest.tail
    }
    builder.result
  }
  
  override def eagerly: Listed[A] = this
}

private[basis] object Listed {
  private[basis] abstract class Abstractly[+A] extends Listable.Abstractly[A] with Listed[A]
  
  private[basis] final class Projected[+A](self: Listable[A]) extends Abstractly[A] {
    override def isEmpty: Boolean = self.isEmpty
    override def head: A = self.head
    override def tail: Listable[A] = self.tail
  }
}
