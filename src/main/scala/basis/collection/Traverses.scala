/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Traverses[+A] extends Any with Traversable[A] {
  import Traverses._
  
  override def foreach[U](f: A => U): Unit
  
  def map[B](f: A => B): Traverses[B] = new Maps[A, B](this, f)
  
  def flatMap[B](f: A => Incremental[B]): Traverses[B] = new FlatMaps[A, B](this, f)
  
  def filter(p: A => Boolean): Traverses[A] = new Filters[A](this, p)
  
  def withFilter(p: A => Boolean): Traverses[A] = filter(p)
  
  def collect[B](q: PartialFunction[A, B]): Traverses[B] = new Collects[A, B](this, q)
  
  def drop(lower: Int): Traverses[A] = new Drops[A](this, lower)
  
  def take(upper: Int): Traverses[A] = new Takes[A](this, upper)
  
  def slice(lower: Int, upper: Int): Traverses[A] = new Slices[A](this, lower, upper)
  
  override def lazily: Traverses[A] = this
}

object Traverses {
  abstract class Abstractly[+A] extends Traversable.Abstractly[A] with Traverses[A]
  
  final class Projects[+A](self: Traversable[A]) extends Abstractly[A] {
    override def foreach[U](f: A => U): Unit = self.foreach(f)
    override def map[B](f: A => B): Traverses[B] = new Maps[A, B](self, f)
    override def flatMap[B](f: A => Incremental[B]): Traverses[B] = new FlatMaps[A, B](self, f)
    override def filter(p: A => Boolean): Traverses[A] = new Filters[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): Traverses[B] = new Collects[A, B](self, q)
    override def drop(lower: Int): Traverses[A] = new Drops[A](self, lower)
    override def take(upper: Int): Traverses[A] = new Takes[A](self, upper)
    override def slice(lower: Int, upper: Int): Traverses[A] = new Slices[A](self, lower, upper)
  }
  
  final class Maps[-A, +B](self: Traversable[A], g: A => B) extends Abstractly[B] {
    override def foreach[U](f: B => U): Unit = for (x <- self) f(g(x))
  }
  
  final class FlatMaps[-A, +B](self: Traversable[A], g: A => Incremental[B]) extends Abstractly[B] {
    override def foreach[U](f: B => U): Unit = for (x <- self) for (y <- g(x)) f(y)
  }
  
  final class Filters[+A](self: Traversable[A], p: A => Boolean) extends Abstractly[A] {
    override def foreach[U](f: A => U): Unit = for (x <- self) if (p(x)) f(x)
  }
  
  final class Collects[-A, +B](self: Traversable[A], q: PartialFunction[A, B]) extends Abstractly[B] {
    override def foreach[U](f: B => U): Unit = for (x <- self) if (q.isDefinedAt(x)) f(q(x))
  }
  
  final class Drops[+A](self: Traversable[A], lower: Int) extends Abstractly[A] {
    override def foreach[U](f: A => U) {
      var i = 0
      for (x <- self) {
        if (i >= lower) f(x)
        i += 1
      }
    }
  }
  
  final class Takes[+A](self: Traversable[A], upper: Int) extends Abstractly[A] {
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
  
  final class Slices[+A](self: Traversable[A], lower: Int, upper: Int) extends Abstractly[A] {
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
