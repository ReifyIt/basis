/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Traversed[+A] extends Any with Traversable[A] {
  import Traversed._
  
  override def foreach[U](f: A => U): Unit
  
  def map[B](f: A => B): Traversed[B] = new Mapped[A, B](this, f)
  
  def flatMap[B](f: A => Incremental[B]): Traversed[B] = new FlatMapped[A, B](this, f)
  
  def filter(p: A => Boolean): Traversed[A] = new Filtered[A](this, p)
  
  def withFilter(p: A => Boolean): Traversed[A] = filter(p)
  
  def collect[B](q: PartialFunction[A, B]): Traversed[B] = new Collected[A, B](this, q)
  
  def drop(lower: Int): Traversed[A] = new Dropped[A](this, lower)
  
  def take(upper: Int): Traversed[A] = new Taken[A](this, upper)
  
  def slice(lower: Int, upper: Int): Traversed[A] = new Sliced[A](this, lower, upper)
  
  override def lazily: Traversed[A] = this
}

object Traversed {
  abstract class Abstractly[+A] extends Traversable.Abstractly[A] with Traversed[A]
  
  final class Projected[+A](self: Traversable[A]) extends Abstractly[A] {
    override def foreach[U](f: A => U): Unit = self.foreach(f)
    override def map[B](f: A => B): Traversed[B] = new Mapped[A, B](self, f)
    override def flatMap[B](f: A => Incremental[B]): Traversed[B] = new FlatMapped[A, B](self, f)
    override def filter(p: A => Boolean): Traversed[A] = new Filtered[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): Traversed[B] = new Collected[A, B](self, q)
    override def drop(lower: Int): Traversed[A] = new Dropped[A](self, lower)
    override def take(upper: Int): Traversed[A] = new Taken[A](self, upper)
    override def slice(lower: Int, upper: Int): Traversed[A] = new Sliced[A](self, lower, upper)
  }
  
  final class Mapped[-A, +B](self: Traversable[A], g: A => B) extends Abstractly[B] {
    override def foreach[U](f: B => U): Unit = for (x <- self) f(g(x))
  }
  
  final class FlatMapped[-A, +B](self: Traversable[A], g: A => Incremental[B]) extends Abstractly[B] {
    override def foreach[U](f: B => U): Unit = for (x <- self) for (y <- g(x)) f(y)
  }
  
  final class Filtered[+A](self: Traversable[A], p: A => Boolean) extends Abstractly[A] {
    override def foreach[U](f: A => U): Unit = for (x <- self) if (p(x)) f(x)
  }
  
  final class Collected[-A, +B](self: Traversable[A], q: PartialFunction[A, B]) extends Abstractly[B] {
    override def foreach[U](f: B => U): Unit = for (x <- self) if (q.isDefinedAt(x)) f(q(x))
  }
  
  final class Dropped[+A](self: Traversable[A], lower: Int) extends Abstractly[A] {
    override def foreach[U](f: A => U) {
      var i = 0
      for (x <- self) {
        if (i >= lower) f(x)
        i += 1
      }
    }
  }
  
  final class Taken[+A](self: Traversable[A], upper: Int) extends Abstractly[A] {
    override def foreach[U](f: A => U) {
      var i = 0
      try for (x <- self) {
        if (i >= upper) throw Break
        f(x)
        i += 1
      }
      catch { case e: Break => () }
    }
  }
  
  final class Sliced[+A](self: Traversable[A], lower: Int, upper: Int) extends Abstractly[A] {
    override def foreach[U](f: A => U) {
      var i = 0
      try for (x <- self) {
        if (i >= lower) {
          if (i >= upper) throw Break
          f(x)
        }
        i += 1
      }
      catch { case e: Break => () }
    }
  }
}
