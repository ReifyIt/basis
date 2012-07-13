/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Traversing[+A] extends Any with Traversable[A] {
  import Traversing._
  
  override def foreach[U](f: A => U): Unit
  
  def map[B](f: A => B): Traversing[B] = new Mapping[A, B](this, f)
  
  def flatMap[B](f: A => Incremental[B]): Traversing[B] = new FlatMapping[A, B](this, f)
  
  def filter(p: A => Boolean): Traversing[A] = new Filtering[A](this, p)
  
  def withFilter(p: A => Boolean): Traversing[A] = filter(p)
  
  def collect[B](q: PartialFunction[A, B]): Traversing[B] = new Collecting[A, B](this, q)
  
  def drop(lower: Int): Traversing[A] = new Dropping[A](this, lower)
  
  def take(upper: Int): Traversing[A] = new Taking[A](this, upper)
  
  def slice(lower: Int, upper: Int): Traversing[A] = new Slicing[A](this, lower, upper)
  
  override def lazily: Traversing[A] = this
}

private[basis] object Traversing {
  private[basis] abstract class Abstractly[+A] extends Traversable.Abstractly[A] with Traversing[A]
  
  private[basis] final class Projecting[+A](self: Traversable[A]) extends Abstractly[A] {
    override def foreach[U](f: A => U): Unit = self.foreach(f)
    override def map[B](f: A => B): Traversing[B] = new Mapping[A, B](self, f)
    override def flatMap[B](f: A => Incremental[B]): Traversing[B] = new FlatMapping[A, B](self, f)
    override def filter(p: A => Boolean): Traversing[A] = new Filtering[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): Traversing[B] = new Collecting[A, B](self, q)
    override def drop(lower: Int): Traversing[A] = new Dropping[A](self, lower)
    override def take(upper: Int): Traversing[A] = new Taking[A](self, upper)
    override def slice(lower: Int, upper: Int): Traversing[A] = new Slicing[A](self, lower, upper)
  }
  
  private[basis] final class Mapping[-A, +B](self: Traversable[A], g: A => B) extends Abstractly[B] {
    override def foreach[U](f: B => U): Unit = for (x <- self) f(g(x))
  }
  
  private[basis] final class FlatMapping[-A, +B](self: Traversable[A], g: A => Incremental[B]) extends Abstractly[B] {
    override def foreach[U](f: B => U): Unit = for (x <- self) for (y <- g(x)) f(y)
  }
  
  private[basis] final class Filtering[+A](self: Traversable[A], p: A => Boolean) extends Abstractly[A] {
    override def foreach[U](f: A => U): Unit = for (x <- self) if (p(x)) f(x)
  }
  
  private[basis] final class Collecting[-A, +B](self: Traversable[A], q: PartialFunction[A, B]) extends Abstractly[B] {
    override def foreach[U](f: B => U): Unit = for (x <- self) if (q.isDefinedAt(x)) f(q(x))
  }
  
  private[basis] final class Dropping[+A](self: Traversable[A], lower: Int) extends Abstractly[A] {
    override def foreach[U](f: A => U) {
      var i = 0
      for (x <- self) {
        if (i >= lower) f(x)
        i += 1
      }
    }
  }
  
  private[basis] final class Taking[+A](self: Traversable[A], upper: Int) extends Abstractly[A] {
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
  
  private[basis] final class Slicing[+A](self: Traversable[A], lower: Int, upper: Int) extends Abstractly[A] {
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
