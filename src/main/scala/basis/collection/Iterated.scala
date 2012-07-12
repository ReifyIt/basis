/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Iterated[+A] extends Any with Traversed[A] with Iterable[A] {
  import Iterated._
  
  override def iterator: Iterator[A]
  
  override def map[B](f: A => B): Iterated[B] = new Mapped[A, B](this, f)
  
  override def filter(p: A => Boolean): Iterated[A] = new Filtered[A](this, p)
  
  override def collect[B](q: PartialFunction[A, B]): Iterated[B] = new Collected[A, B](this, q)
  
  override def drop(lower: Int): Iterated[A] = new Dropped[A](this, lower)
  
  override def take(upper: Int): Iterated[A] = new Taken[A](this, upper)
  
  override def slice(lower: Int, upper: Int): Iterated[A] = new Sliced[A](this, lower, upper)
  
  def zip[B](that: Iterable[B]): Iterated[(A, B)] = new Zipped[A, B](this, that)
  
  def :++ [B >: A](elements: Iterable[B]): Iterated[B] = new Appended[B](this, elements)
  
  def ++: [B >: A](elements: Iterable[B]): Iterated[B] = new Appended[B](elements, this)
  
  override def lazily: Iterated[A] = this
}

object Iterated {
  abstract class Abstractly[+A] extends Iterable.Abstractly[A] with Iterated[A]
  
  final class Projected[+A](self: Iterable[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator
    override def map[B](f: A => B): Iterated[B] = new Mapped[A, B](self, f)
    override def filter(p: A => Boolean): Iterated[A] = new Filtered[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): Iterated[B] = new Collected[A, B](self, q)
    override def drop(lower: Int): Iterated[A] = new Dropped[A](self, lower)
    override def take(upper: Int): Iterated[A] = new Taken[A](self, upper)
    override def slice(lower: Int, upper: Int): Iterated[A] = new Sliced[A](self, lower, upper)
    override def zip[B](that: Iterable[B]): Iterated[(A, B)] = new Zipped[A, B](self, that)
    override def :++ [B >: A](elements: Iterable[B]): Iterated[B] = new Appended[B](self, elements)
    override def ++: [B >: A](elements: Iterable[B]): Iterated[B] = new Appended[B](elements, self)
  }
  
  final class Mapped[-A, +B](self: Iterable[A], f: A => B) extends Abstractly[B] {
    override def iterator: Iterator[B] = self.iterator map f
  }
  
  final class Filtered[+A](self: Iterable[A], p: A => Boolean) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator filter p
  }
  
  final class Collected[-A, +B](self: Iterable[A], q: PartialFunction[A, B]) extends Abstractly[B] {
    override def iterator: Iterator[B] = self.iterator collect q
  }
  
  final class Dropped[+A](self: Iterable[A], lower: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator drop lower
  }
  
  final class Taken[+A](self: Iterable[A], upper: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator take upper
  }
  
  final class Sliced[+A](self: Iterable[A], lower: Int, upper: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator slice (lower, upper)
  }
  
  final class Zipped[+A, +B](self: Iterable[A], that: Iterable[B]) extends Abstractly[(A, B)] {
    override def iterator: Iterator[(A, B)] = self.iterator zip that.iterator
  }
  
  final class Appended[+A](self: Iterable[A], elements: Iterable[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator ++ elements.iterator
  }
}
