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
  
  def :++ [B >: A](elements: Iterable[B]): Iterating[B] = new Appending[B](this, elements)
  
  def ++: [B >: A](elements: Iterable[B]): Iterating[B] = new Appending[B](elements, this)
  
  override def lazily: Iterating[A] = this
}

object Iterating {
  abstract class Abstractly[+A] extends Iterable.Abstractly[A] with Iterating[A]
  
  final class Projecting[+A](self: Iterable[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator
    override def map[B](f: A => B): Iterating[B] = new Mapping[A, B](self, f)
    override def filter(p: A => Boolean): Iterating[A] = new Filtering[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): Iterating[B] = new Collecting[A, B](self, q)
    override def drop(lower: Int): Iterating[A] = new Dropping[A](self, lower)
    override def take(upper: Int): Iterating[A] = new Taking[A](self, upper)
    override def slice(lower: Int, upper: Int): Iterating[A] = new Slicing[A](self, lower, upper)
    override def zip[B](that: Iterable[B]): Iterating[(A, B)] = new Zipping[A, B](self, that)
    override def :++ [B >: A](elements: Iterable[B]): Iterating[B] = new Appending[B](self, elements)
    override def ++: [B >: A](elements: Iterable[B]): Iterating[B] = new Appending[B](elements, self)
  }
  
  final class Mapping[-A, +B](self: Iterable[A], f: A => B) extends Abstractly[B] {
    override def iterator: Iterator[B] = self.iterator map f
  }
  
  final class Filtering[+A](self: Iterable[A], p: A => Boolean) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator filter p
  }
  
  final class Collecting[-A, +B](self: Iterable[A], q: PartialFunction[A, B]) extends Abstractly[B] {
    override def iterator: Iterator[B] = self.iterator collect q
  }
  
  final class Dropping[+A](self: Iterable[A], lower: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator drop lower
  }
  
  final class Taking[+A](self: Iterable[A], upper: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator take upper
  }
  
  final class Slicing[+A](self: Iterable[A], lower: Int, upper: Int) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator slice (lower, upper)
  }
  
  final class Zipping[+A, +B](self: Iterable[A], that: Iterable[B]) extends Abstractly[(A, B)] {
    override def iterator: Iterator[(A, B)] = self.iterator zip that.iterator
  }
  
  final class Appending[+A](self: Iterable[A], elements: Iterable[A]) extends Abstractly[A] {
    override def iterator: Iterator[A] = self.iterator ++ elements.iterator
  }
}
