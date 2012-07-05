/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait TraversableView[+A] extends Any with Traversable[A] { self =>
  def map[B](f: A => B): TraversableView[B] = new Mapped[B](f)
  
  def flatMap[B](f: A => Incremental[B]): TraversableView[B] = new FlatMapped[B](f)
  
  def filter(p: A => Boolean): TraversableView[A] = new Filtered(p)
  
  def withFilter(p: A => Boolean): TraversableView[A] = filter(p)
  
  def collect[B](q: PartialFunction[A, B]): TraversableView[B] = new Collected[B](q)
  
  def drop(lower: Int): TraversableView[A] = new Dropped(lower)
  
  def take(upper: Int): TraversableView[A] = new Taken(upper)
  
  def slice(lower: Int, upper: Int): TraversableView[A] = new Sliced(lower, upper)
  
  private final class Mapped[+B](g: A => B) extends AbstractTraversableView[B] {
    override def foreach[U](f: B => U): Unit = for (x <- self) f(g(x))
  }
  
  private final class FlatMapped[+B](g: A => Incremental[B]) extends AbstractTraversableView[B] {
    override def foreach[U](f: B => U): Unit = for (x <- self) for (y <- g(x)) f(y)
  }
  
  private final class Filtered(p: A => Boolean) extends AbstractTraversableView[A] {
    override def foreach[U](f: A => U): Unit = for (x <- self) if (p(x)) f(x)
  }
  
  private final class Collected[+B](q: PartialFunction[A, B]) extends AbstractTraversableView[B] {
    override def foreach[U](f: B => U): Unit = for (x <- self) if (q.isDefinedAt(x)) f(q(x))
  }
  
  private final class Dropped(lower: Int) extends AbstractTraversableView[A] {
    override def foreach[U](f: A => U) {
      var i = 0
      for (x <- self) {
        if (i >= lower) f(x)
        i += 1
      }
    }
  }
  
  private final class Taken(upper: Int) extends AbstractTraversableView[A] {
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
  
  private final class Sliced(from: Int, until: Int) extends AbstractTraversableView[A] {
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
