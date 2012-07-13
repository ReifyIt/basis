/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Traversed[+A] extends Any with Traversable[A] {
  type Scope
  
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
  
  def withFilter(p: A => Boolean): Traversing[A] = new Traversing.Filtering[A](this, p)
  
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
  
  override def eagerly: Traversed[A] = this
}

private[basis] object Traversed {
  private[basis] abstract class Abstractly[+A] extends Traversable.Abstractly[A] with Traversed[A]
  
  private[basis] final class Projected[+A](self: Traversable[A]) extends Abstractly[A] {
    override def foreach[U](f: A => U): Unit = self.foreach[U](f)
  }
}
