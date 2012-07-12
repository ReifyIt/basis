/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Indexed[+A] extends Any with Sequenced[A] with Indexable[A] {
  import Indexed._
  
  override def length: Int
  
  override def apply(index: Int): A
  
  def reverse: Indexed[A] = new Reversed[A](this)
  
  override def map[B](f: A => B): Indexed[B] = new Mapped[A, B](this, f)
  
  override def filter(p: A => Boolean): Indexed[A] = new Filtered[A](this, p)
  
  override def collect[B](q: PartialFunction[A, B]): Indexed[B] = new Collected[A, B](this, q)
  
  override def drop(lower: Int): Indexed[A] = new Dropped[A](this, lower)
  
  override def take(upper: Int): Indexed[A] = new Taken[A](this, upper)
  
  override def slice(lower: Int, upper: Int): Indexed[A] = new Sliced[A](this, lower, upper)
  
  override def lazily: Indexed[A] = this
}

object Indexed {
  abstract class Abstractly[+A] extends Indexable.Abstractly[A] with Indexed[A]
  
  final class Projected[+A](self: Indexable[A]) extends Abstractly[A] {
    override def length: Int = self.length
    override def apply(index: Int): A = self.apply(index)
    override def reverse: Indexed[A] = new Reversed[A](self)
    override def map[B](f: A => B): Indexed[B] = new Mapped[A, B](self, f)
    override def filter(p: A => Boolean): Indexed[A] = new Filtered[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): Indexed[B] = new Collected[A, B](self, q)
    override def drop(lower: Int): Indexed[A] = new Dropped[A](self, lower)
    override def take(upper: Int): Indexed[A] = new Taken[A](self, upper)
    override def slice(lower: Int, upper: Int): Indexed[A] = new Sliced[A](self, lower, upper)
  }
  
  final class Reversed[+A](self: Indexable[A]) extends Abstractly[A] {
    override def length: Int = self.length
    override def apply(index: Int): A = self.apply(length - index - 1)
    override def reverse: Indexed[A] = self.lazily
  }
  
  final class Mapped[-A, +B](self: Indexable[A], f: A => B) extends Abstractly[B] {
    override def length: Int = self.length
    override def apply(index: Int): B = f(self.apply(index))
  }
  
  final class Filtered[+A](self: Indexable[A], p: A => Boolean) extends Abstractly[A] {
    private[this] var filteredIndexed: Array[Int] = _
    private[this] def lookup: Array[Int] = synchronized {
      if (filteredIndexed == null) {
        val size = self.length
        val indexes = new Array[Int](size)
        var i = 0
        var k = 0
        while (i < size) {
          if (p(self.apply(i))) {
            indexes(k) = i
            k += 1
          }
          i += 1
        }
        if (k == size) filteredIndexed = indexes
        else {
          filteredIndexed = new Array[Int](k)
          Array.copy(indexes, 0, filteredIndexed, 0, k)
        }
      }
      filteredIndexed
    }
    override def length: Int = lookup.length
    override def apply(index: Int): A = self.apply(lookup(index))
  }
  
  final class Collected[-A, +B](self: Indexable[A], q: PartialFunction[A, B]) extends Abstractly[B] {
    private[this] var collectedIndexed: Array[Int] = _
    private[this] def lookup: Array[Int] = synchronized {
      if (collectedIndexed == null) {
        val size = self.length
        val indexes = new Array[Int](size)
        var i = 0
        var k = 0
        while (i < size) {
          if (q.isDefinedAt(self.apply(i))) {
            indexes(k) = i
            k += 1
          }
          i += 1
        }
        if (k == size) collectedIndexed = indexes
        else {
          collectedIndexed = new Array[Int](k)
          Array.copy(indexes, 0, collectedIndexed, 0, k)
        }
      }
      collectedIndexed
    }
    override def length: Int = lookup.length
    override def apply(index: Int): B = q(self.apply(lookup(index)))
  }
  
  final class Dropped[+A](self: Indexable[A], lower: Int) extends Abstractly[A] {
    private[this] val from: Int = math.max(0, math.min(lower, self.length))
    override def length: Int = self.length - from
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      self.apply(from + index)
    }
  }
  
  final class Taken[+A](self: Indexable[A], upper: Int) extends Abstractly[A] {
    private[this] val limit: Int = math.max(0, math.min(upper, self.length))
    override def length: Int = limit
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      self.apply(index)
    }
  }
  
  final class Sliced[+A](self: Indexable[A], lower: Int, upper: Int) extends Abstractly[A] {
    private[this] val from: Int = math.max(0, math.min(lower, self.length))
    private[this] val until: Int = math.max(from, math.min(upper, self.length))
    override def length: Int = until - from
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      self.apply(from + index)
    }
  }
}
