/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Sequencing[+A] extends Any with Iterating[A] with Sequential[A] {
  import Sequencing._
  
  override def iterator: Iterator[A]
  
  override def map[B](f: A => B): Sequencing[B] = new Mapping[A, B](this, f)
  
  override def filter(p: A => Boolean): Sequencing[A] = new Filtering[A](this, p)
  
  override def collect[B](q: PartialFunction[A, B]): Sequencing[B] = new Collecting[A, B](this, q)
  
  override def drop(lower: Int): Sequencing[A] = new Dropping[A](this, lower)
  
  override def take(upper: Int): Sequencing[A] = new Taking[A](this, upper)
  
  override def slice(lower: Int, upper: Int): Sequencing[A] = new Slicing[A](this, lower, upper)
  
  override def zip[B](that: Iterable[B]): Sequencing[(A, B)] = new Zipping[A, B](this, that)
  
  override def :++ [B >: A](elements: Iterable[B]): Sequencing[B] = new Appending[B](this, elements)
  
  override def ++: [B >: A](elements: Iterable[B]): Sequencing[B] = new Prepending[B](this, elements)
  
  def :+ [B >: A](element: B): Sequencing[B] = new Suffixing[B](this, element)
  
  def +: [B >: A](element: B): Sequencing[B] = new Prefixing[B](this, element)
  
  override def lazily: Sequencing[A] = this
}

object Sequencing {
  abstract class Abstractly[+A] extends Sequential.Abstractly[A] with Sequencing[A]
  
  final class Projecting[+A](self: Sequential[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator
    override def map[B](f: A => B): Sequencing[B] = new Mapping[A, B](self, f)
    override def filter(p: A => Boolean): Sequencing[A] = new Filtering[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): Sequencing[B] = new Collecting[A, B](self, q)
    override def drop(lower: Int): Sequencing[A] = new Dropping[A](self, lower)
    override def take(upper: Int): Sequencing[A] = new Taking[A](self, upper)
    override def slice(lower: Int, upper: Int): Sequencing[A] = new Slicing[A](self, lower, upper)
    override def zip[B](that: Iterable[B]): Sequencing[(A, B)] = new Zipping[A, B](self, that)
    override def :++ [B >: A](elements: Iterable[B]): Sequencing[B] = new Appending[B](self, elements)
    override def ++: [B >: A](elements: Iterable[B]): Sequencing[B] = new Prepending[B](self, elements)
    override def :+ [B >: A](element: B): Sequencing[B] = new Suffixing[B](self, element)
    override def +: [B >: A](element: B): Sequencing[B] = new Prefixing[B](self, element)
  }
  
  final class Mapping[-A, +B](self: Sequential[A], f: A => B) extends Abstractly[B] {
    override def iterator: Iterator[B] = self.iterator map f
  }
  
  final class Filtering[+A](self: Sequential[A], p: A => Boolean) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator filter p
  }
  
  final class Collecting[-A, +B](self: Sequential[A], q: PartialFunction[A, B]) extends Abstractly[B] {
    override def iterator: Iterator[B] = self.iterator collect q
  }
  
  final class Dropping[+A](self: Sequential[A], lower: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator drop lower
  }
  
  final class Taking[+A](self: Sequential[A], upper: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator take upper
  }
  
  final class Slicing[+A](self: Sequential[A], lower: Int, upper: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator slice (lower, upper)
  }
  
  final class Zipping[+A, +B](self: Sequential[A], that: Iterable[B]) extends Abstractly[(A, B)] {
    override def iterator: Iterator[(A, B)] = self.iterator zip that.iterator
  }
  
  final class Appending[+A](self: Sequential[A], elements: Iterable[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator ++ elements.iterator
  }
  
  final class Prepending[+A](self: Sequential[A], elements: Iterable[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = elements.iterator ++ self.iterator
  }
  
  final class Suffixing[+A](self: Sequential[A], element: A) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator :+ element
  }
  
  final class Prefixing[+A](self: Sequential[A], element: A) extends Abstractly[A] {
    override def iterator: Iterator[A] = element +: self.iterator
  }
}
