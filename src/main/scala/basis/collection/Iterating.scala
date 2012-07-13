/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Iterating[+A] extends Any with Traversing[A] with Iterable[A] {
  import Iterating._
  
  override def iterator: Iterator[A]
  
  override def map[B](f: A => B): Iterating[B] = new Mapping[A, B](this, f)
  
  override def filter(p: A => Boolean): Iterating[A] = new Filtering[A](this, p)
  
  override def collect[B](q: PartialFunction[A, B]): Iterating[B] = new Collecting[A, B](this, q)
  
  override def drop(lower: Int): Iterating[A] = new Dropping[A](this, lower)
  
  override def take(upper: Int): Iterating[A] = new Taking[A](this, upper)
  
  override def slice(lower: Int, upper: Int): Iterating[A] = new Slicing[A](this, lower, upper)
  
  def zip[B](that: Iterable[B]): Iterating[(A, B)] = new Zipping[A, B](this, that)
  
  override def lazily: Iterating[A] = this
}

private[basis] object Iterating {
  private[basis] abstract class Abstractly[+A] extends Iterable.Abstractly[A] with Iterating[A]
  
  private[basis] final class Projecting[+A](self: Iterable[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator
    override def map[B](f: A => B): Iterating[B] = new Mapping[A, B](self, f)
    override def filter(p: A => Boolean): Iterating[A] = new Filtering[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): Iterating[B] = new Collecting[A, B](self, q)
    override def drop(lower: Int): Iterating[A] = new Dropping[A](self, lower)
    override def take(upper: Int): Iterating[A] = new Taking[A](self, upper)
    override def slice(lower: Int, upper: Int): Iterating[A] = new Slicing[A](self, lower, upper)
    override def zip[B](that: Iterable[B]): Iterating[(A, B)] = new Zipping[A, B](self, that)
  }
  
  private[basis] final class Mapping[-A, +B](self: Iterable[A], f: A => B) extends Abstractly[B] {
    override def iterator: Iterator[B] = self.iterator map f
  }
  
  private[basis] final class Filtering[+A](self: Iterable[A], p: A => Boolean) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator filter p
  }
  
  private[basis] final class Collecting[-A, +B](self: Iterable[A], q: PartialFunction[A, B]) extends Abstractly[B] {
    override def iterator: Iterator[B] = self.iterator collect q
  }
  
  private[basis] final class Dropping[+A](self: Iterable[A], lower: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator drop lower
  }
  
  private[basis] final class Taking[+A](self: Iterable[A], upper: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator take upper
  }
  
  private[basis] final class Slicing[+A](self: Iterable[A], lower: Int, upper: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator slice (lower, upper)
  }
  
  private[basis] final class Zipping[+A, +B](self: Iterable[A], that: Iterable[B]) extends Abstractly[(A, B)] {
    override def iterator: Iterator[(A, B)] = self.iterator zip that.iterator
  }
}
