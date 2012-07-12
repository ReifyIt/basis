/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Sequenced[+A] extends Any with Iterated[A] with Sequential[A] {
  import Sequenced._
  
  override def iterator: Iterator[A]
  
  override def map[B](f: A => B): Sequenced[B] = new Mapped[A, B](this, f)
  
  override def filter(p: A => Boolean): Sequenced[A] = new Filtered[A](this, p)
  
  override def collect[B](q: PartialFunction[A, B]): Sequenced[B] = new Collected[A, B](this, q)
  
  override def drop(lower: Int): Sequenced[A] = new Dropped[A](this, lower)
  
  override def take(upper: Int): Sequenced[A] = new Taken[A](this, upper)
  
  override def slice(lower: Int, upper: Int): Sequenced[A] = new Sliced[A](this, lower, upper)
  
  override def zip[B](that: Iterable[B]): Sequenced[(A, B)] = new Zipped[A, B](this, that)
  
  override def :++ [B >: A](elements: Iterable[B]): Sequenced[B] = new Appended[B](this, elements)
  
  override def ++: [B >: A](elements: Iterable[B]): Sequenced[B] = new Prepended[B](this, elements)
  
  def :+ [B >: A](element: B): Sequenced[B] = new Suffixed[B](this, element)
  
  def +: [B >: A](element: B): Sequenced[B] = new Prefixed[B](this, element)
  
  override def lazily: Sequenced[A] = this
}

object Sequenced {
  abstract class Abstractly[+A] extends Sequential.Abstractly[A] with Sequenced[A]
  
  final class Projected[+A](self: Sequential[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator
    override def map[B](f: A => B): Sequenced[B] = new Mapped[A, B](self, f)
    override def filter(p: A => Boolean): Sequenced[A] = new Filtered[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): Sequenced[B] = new Collected[A, B](self, q)
    override def drop(lower: Int): Sequenced[A] = new Dropped[A](self, lower)
    override def take(upper: Int): Sequenced[A] = new Taken[A](self, upper)
    override def slice(lower: Int, upper: Int): Sequenced[A] = new Sliced[A](self, lower, upper)
    override def zip[B](that: Iterable[B]): Sequenced[(A, B)] = new Zipped[A, B](self, that)
    override def :++ [B >: A](elements: Iterable[B]): Sequenced[B] = new Appended[B](self, elements)
    override def ++: [B >: A](elements: Iterable[B]): Sequenced[B] = new Prepended[B](self, elements)
    override def :+ [B >: A](element: B): Sequenced[B] = new Suffixed[B](self, element)
    override def +: [B >: A](element: B): Sequenced[B] = new Prefixed[B](self, element)
  }
  
  final class Mapped[-A, +B](self: Sequential[A], f: A => B) extends Abstractly[B] {
    override def iterator: Iterator[B] = self.iterator map f
  }
  
  final class Filtered[+A](self: Sequential[A], p: A => Boolean) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator filter p
  }
  
  final class Collected[-A, +B](self: Sequential[A], q: PartialFunction[A, B]) extends Abstractly[B] {
    override def iterator: Iterator[B] = self.iterator collect q
  }
  
  final class Dropped[+A](self: Sequential[A], lower: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator drop lower
  }
  
  final class Taken[+A](self: Sequential[A], upper: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator take upper
  }
  
  final class Sliced[+A](self: Sequential[A], lower: Int, upper: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator slice (lower, upper)
  }
  
  final class Zipped[+A, +B](self: Sequential[A], that: Iterable[B]) extends Abstractly[(A, B)] {
    override def iterator: Iterator[(A, B)] = self.iterator zip that.iterator
  }
  
  final class Appended[+A](self: Sequential[A], elements: Iterable[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator ++ elements.iterator
  }
  
  final class Prepended[+A](self: Sequential[A], elements: Iterable[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = elements.iterator ++ self.iterator
  }
  
  final class Suffixed[+A](self: Sequential[A], element: A) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator :+ element
  }
  
  final class Prefixed[+A](self: Sequential[A], element: A) extends Abstractly[A] {
    override def iterator: Iterator[A] = element +: self.iterator
  }
}
