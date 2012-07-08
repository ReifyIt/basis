/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait TraversableView[+A] extends Any with Traversable[A] {
  import TraversableView._
  
  def map[B](f: A => B): TraversableView[B] = new Mapped[A, B](this, f)
  
  def flatMap[B](f: A => Incremental[B]): TraversableView[B] = new FlatMapped[A, B](this, f)
  
  def filter(p: A => Boolean): TraversableView[A] = new Filtered[A](this, p)
  
  def withFilter(p: A => Boolean): TraversableView[A] = filter(p)
  
  def collect[B](q: PartialFunction[A, B]): TraversableView[B] = new Collected[A, B](this, q)
  
  def drop(lower: Int): TraversableView[A] = new Dropped[A](this, lower)
  
  def take(upper: Int): TraversableView[A] = new Taken[A](this, upper)
  
  def slice(lower: Int, upper: Int): TraversableView[A] = new Sliced[A](this, lower, upper)
  
  override def view: TraversableView[A] = this
}

private[basis] object TraversableView {
  final class Projection[+A](self: Traversable[A]) extends AbstractTraversableView[A] {
    override def foreach[U](f: A => U): Unit = self.foreach(f)
    override def map[B](f: A => B): TraversableView[B] = new Mapped[A, B](self, f)
    override def flatMap[B](f: A => Incremental[B]): TraversableView[B] = new FlatMapped[A, B](self, f)
    override def filter(p: A => Boolean): TraversableView[A] = new Filtered[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): TraversableView[B] = new Collected[A, B](self, q)
    override def drop(lower: Int): TraversableView[A] = new Dropped[A](self, lower)
    override def take(upper: Int): TraversableView[A] = new Taken[A](self, upper)
    override def slice(lower: Int, upper: Int): TraversableView[A] = new Sliced[A](self, lower, upper)
  }
  
  final class Mapped[-A, +B](self: Traversable[A], g: A => B) extends AbstractTraversableView[B] {
    override def foreach[U](f: B => U): Unit = for (x <- self) f(g(x))
  }
  
  final class FlatMapped[-A, +B](self: Traversable[A], g: A => Incremental[B]) extends AbstractTraversableView[B] {
    override def foreach[U](f: B => U): Unit = for (x <- self) for (y <- g(x)) f(y)
  }
  
  final class Filtered[+A](self: Traversable[A], p: A => Boolean) extends AbstractTraversableView[A] {
    override def foreach[U](f: A => U): Unit = for (x <- self) if (p(x)) f(x)
  }
  
  final class Collected[-A, +B](self: Traversable[A], q: PartialFunction[A, B]) extends AbstractTraversableView[B] {
    override def foreach[U](f: B => U): Unit = for (x <- self) if (q.isDefinedAt(x)) f(q(x))
  }
  
  final class Dropped[+A](self: Traversable[A], lower: Int) extends AbstractTraversableView[A] {
    override def foreach[U](f: A => U) {
      var i = 0
      for (x <- self) {
        if (i >= lower) f(x)
        i += 1
      }
    }
  }
  
  final class Taken[+A](self: Traversable[A], upper: Int) extends AbstractTraversableView[A] {
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
  
  final class Sliced[+A](self: Traversable[A], from: Int, until: Int) extends AbstractTraversableView[A] {
    private[this] val lower: Int = math.max(0, from)
    private[this] val upper: Int = math.max(lower, until)
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

private[basis] abstract class AbstractTraversableView[+A] extends AbstractTraversable[A] with TraversableView[A]
