/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait IterableView[+A] extends Any with TraversableView[A] with Iterable[A] {
  import IterableView._
  
  override def map[B](f: A => B): IterableView[B] = new Mapped[A, B](this, f)
  
  override def filter(p: A => Boolean): IterableView[A] = new Filtered[A](this, p)
  
  override def collect[B](q: PartialFunction[A, B]): IterableView[B] = new Collected[A, B](this, q)
  
  override def drop(lower: Int): IterableView[A] = new Dropped[A](this, lower)
  
  override def take(upper: Int): IterableView[A] = new Taken[A](this, upper)
  
  override def slice(lower: Int, upper: Int): IterableView[A] = new Sliced[A](this, lower, upper)
  
  def zip[B](that: Iterable[B]): IterableView[(A, B)] = new Zipped[A, B](this, that)
  
  override def view: IterableView[A] = this
}

private[basis] object IterableView {
  final class Projection[+A](self: Iterable[A]) extends AbstractIterableView[A] {
    override def iterator: Iterated[A] = self.iterator
    override def map[B](f: A => B): IterableView[B] = new Mapped[A, B](self, f)
    override def filter(p: A => Boolean): IterableView[A] = new Filtered[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): IterableView[B] = new Collected[A, B](self, q)
    override def drop(lower: Int): IterableView[A] = new Dropped[A](self, lower)
    override def take(upper: Int): IterableView[A] = new Taken[A](self, upper)
    override def slice(lower: Int, upper: Int): IterableView[A] = new Sliced[A](self, lower, upper)
    override def zip[B](that: Iterable[B]): IterableView[(A, B)] = new Zipped[A, B](self, that)
  }
  
  final class Mapped[-A, +B](self: Iterable[A], f: A => B) extends AbstractIterableView[B] {
    override def iterator: Iterated[B] = self.iterator map f
  }
  
  final class Filtered[+A](self: Iterable[A], p: A => Boolean) extends AbstractIterableView[A] {
    override def iterator: Iterated[A] = self.iterator filter p
  }
  
  final class Collected[-A, +B](self: Iterable[A], q: PartialFunction[A, B]) extends AbstractIterableView[B] {
    override def iterator: Iterated[B] = self.iterator collect q
  }
  
  final class Dropped[+A](self: Iterable[A], lower: Int) extends AbstractIterableView[A] {
    override def iterator: Iterated[A] = self.iterator drop lower
  }
  
  final class Taken[+A](self: Iterable[A], upper: Int) extends AbstractIterableView[A] {
    override def iterator: Iterated[A] = self.iterator take upper
  }
  
  final class Sliced[+A](self: Iterable[A], lower: Int, upper: Int) extends AbstractIterableView[A] {
    override def iterator: Iterated[A] = self.iterator slice (lower, upper)
  }
  
  final class Zipped[+A, +B](self: Iterable[A], that: Iterable[B]) extends AbstractIterableView[(A, B)] {
    override def iterator: Iterated[(A, B)] = self.iterator zip that.iterator
  }
}

private[basis] abstract class AbstractIterableView[+A] extends AbstractIterable[A] with IterableView[A]
