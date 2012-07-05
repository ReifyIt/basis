/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait IterableView[+A] extends Any with TraversableView[A] with Iterable[A] { self =>
  override def map[B](f: A => B): IterableView[B] = new Mapped[B](f)
  
  override def filter(p: A => Boolean): IterableView[A] = new Filtered(p)
  
  override def collect[B](q: PartialFunction[A, B]): IterableView[B] = new Collected[B](q)
  
  override def drop(lower: Int): IterableView[A] = new Dropped(lower)
  
  override def take(upper: Int): IterableView[A] = new Taken(upper)
  
  override def slice(lower: Int, upper: Int): IterableView[A] = new Sliced(lower, upper)
  
  private final class Mapped[+B](f: A => B) extends AbstractIterableView[B] {
    override def iterator: Iterated[B] = self.iterator map f
  }
  
  private final class Filtered(p: A => Boolean) extends AbstractIterableView[A] {
    override def iterator: Iterated[A] = self.iterator filter p
  }
  
  private final class Collected[+B](q: PartialFunction[A, B]) extends AbstractIterableView[B] {
    override def iterator: Iterated[B] = self.iterator collect q
  }
  
  private final class Dropped(lower: Int) extends AbstractIterableView[A] {
    override def iterator: Iterated[A] = self.iterator drop lower
  }
  
  private final class Taken(upper: Int) extends AbstractIterableView[A] {
    override def iterator: Iterated[A] = self.iterator take upper
  }
  
  private final class Sliced(lower: Int, upper: Int) extends AbstractIterableView[A] {
    override def iterator: Iterated[A] = self.iterator slice (lower, upper)
  }
}

private[basis] abstract class AbstractIterableView[+A] extends AbstractIterable[A] with IterableView[A]
