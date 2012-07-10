/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Iterates[+A] extends Any with Traverses[A] with Iterable[A] {
  import Iterates._
  
  override def iterator: Iterator[A]
  
  override def map[B](f: A => B): Iterates[B] = new Maps[A, B](this, f)
  
  override def filter(p: A => Boolean): Iterates[A] = new Filters[A](this, p)
  
  override def collect[B](q: PartialFunction[A, B]): Iterates[B] = new Collects[A, B](this, q)
  
  override def drop(lower: Int): Iterates[A] = new Drops[A](this, lower)
  
  override def take(upper: Int): Iterates[A] = new Takes[A](this, upper)
  
  override def slice(lower: Int, upper: Int): Iterates[A] = new Slices[A](this, lower, upper)
  
  def zip[B](that: Iterable[B]): Iterates[(A, B)] = new Zips[A, B](this, that)
  
  override def lazily: Iterates[A] = this
}

object Iterates {
  abstract class Abstractly[+A] extends Iterable.Abstractly[A] with Iterates[A]
  
  final class Projects[+A](self: Iterable[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator
    override def map[B](f: A => B): Iterates[B] = new Maps[A, B](self, f)
    override def filter(p: A => Boolean): Iterates[A] = new Filters[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): Iterates[B] = new Collects[A, B](self, q)
    override def drop(lower: Int): Iterates[A] = new Drops[A](self, lower)
    override def take(upper: Int): Iterates[A] = new Takes[A](self, upper)
    override def slice(lower: Int, upper: Int): Iterates[A] = new Slices[A](self, lower, upper)
    override def zip[B](that: Iterable[B]): Iterates[(A, B)] = new Zips[A, B](self, that)
  }
  
  final class Maps[-A, +B](self: Iterable[A], f: A => B) extends Abstractly[B] {
    override def iterator: Iterator[B] = self.iterator map f
  }
  
  final class Filters[+A](self: Iterable[A], p: A => Boolean) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator filter p
  }
  
  final class Collects[-A, +B](self: Iterable[A], q: PartialFunction[A, B]) extends Abstractly[B] {
    override def iterator: Iterator[B] = self.iterator collect q
  }
  
  final class Drops[+A](self: Iterable[A], lower: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator drop lower
  }
  
  final class Takes[+A](self: Iterable[A], upper: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator take upper
  }
  
  final class Slices[+A](self: Iterable[A], lower: Int, upper: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator slice (lower, upper)
  }
  
  final class Zips[+A, +B](self: Iterable[A], that: Iterable[B]) extends Abstractly[(A, B)] {
    override def iterator: Iterator[(A, B)] = self.iterator zip that.iterator
  }
}
