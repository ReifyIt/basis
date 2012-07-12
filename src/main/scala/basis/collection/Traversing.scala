/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Traversing[+Scope, +A] extends Any with Traversable[A] {
  override def foreach[U](f: A => U): Unit
  
  def map[B](f: A => B)(implicit builder: Collector[Scope, B]): builder.Product = {
    for (x <- this) builder += f(x)
    builder.result
  }
  
  def flatMap[B](f: A => Incremental[B])(implicit builder: Collector[Scope, B]): builder.Product = {
    for (x <- this) builder ++= f(x)
    builder.result
  }
  
  def filter(p: A => Boolean)(implicit builder: Collector[Scope, A]): builder.Product = {
    for (x <- this) if (p(x)) builder += x
    builder.result
  }
  
  def withFilter(p: A => Boolean): Traversed[A] = new Traversed.Filtered[A](this, p)
  
  def collect[B](q: PartialFunction[A, B])(implicit builder: Collector[Scope, B]): builder.Product = {
    for (x <- this) if (q.isDefinedAt(x)) builder += q(x)
    builder.result
  }
  
  def drop(lower: Int)(implicit builder: Collector[Scope, A]): builder.Product = {
    var i = 0
    for (x <- this) {
      if (i >= lower) builder += x
      i += 1
    }
    builder.result
  }
  
  def take(upper: Int)(implicit builder: Collector[Scope, A]): builder.Product = {
    var i = 0
    try for (x <- this) {
      if (i >= upper) throw Break
      builder += x
      i += 1
    }
    catch { case e: Break => () }
    builder.result
  }
  
  def slice(lower: Int, upper: Int)(implicit builder: Collector[Scope, A]): builder.Product = {
    var i = 0
    try for (x <- this) {
      if (i >= lower) {
        if (i >= upper) throw Break
        builder += x
      }
      i += 1
    }
    catch { case e: Break => () }
    builder.result
  }
  
  def :++ [B >: A](elements: Incremental[B])(implicit builder: Collector[Scope, B]): builder.Product = {
    val append = (x: B) => builder += x
    this foreach append
    elements foreach append
    builder.result
  }
  
  def ++: [B >: A](elements: Incremental[B])(implicit builder: Collector[Scope, B]): builder.Product = {
    val append = (x: B) => builder += x
    elements foreach append
    this foreach append
    builder.result
  }
  
  override def eagerly: Traversing[Scope, A] = this
}

object Traversing {
  abstract class Abstractly[+Scope, +A] extends Traversable.Abstractly[A] with Traversing[Scope, A]
  
  final class Projecting[+Scope, +A](self: Traversable[A]) extends Abstractly[Scope, A] {
    override def foreach[U](f: A => U): Unit = self.foreach[U](f)
  }
}
