/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Indexes[+A] extends Any with Iterates[A] with Indexable[A] {
  import Indexes._
  
  override def length: Int
  
  override def apply(index: Int): A
  
  def reversed: Indexes[A] = new Reverses[A](this)
  
  override def map[B](f: A => B): Indexes[B] = new Maps[A, B](this, f)
  
  override def filter(p: A => Boolean): Indexes[A] = new Filters[A](this, p)
  
  override def collect[B](q: PartialFunction[A, B]): Indexes[B] = new Collects[A, B](this, q)
  
  override def drop(lower: Int): Indexes[A] = new Drops[A](this, lower)
  
  override def take(upper: Int): Indexes[A] = new Takes[A](this, upper)
  
  override def slice(lower: Int, upper: Int): Indexes[A] = new Slices[A](this, lower, upper)
  
  override def lazily: Indexes[A] = this
}

object Indexes {
  abstract class Abstractly[+A] extends Indexable.Abstractly[A] with Indexes[A]
  
  final class Projects[+A](self: Indexable[A]) extends Abstractly[A] {
    override def length: Int = self.length
    override def apply(index: Int): A = self.apply(index)
    override def reversed: Indexes[A] = new Reverses[A](self)
    override def map[B](f: A => B): Indexes[B] = new Maps[A, B](self, f)
    override def filter(p: A => Boolean): Indexes[A] = new Filters[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): Indexes[B] = new Collects[A, B](self, q)
    override def drop(lower: Int): Indexes[A] = new Drops[A](self, lower)
    override def take(upper: Int): Indexes[A] = new Takes[A](self, upper)
    override def slice(lower: Int, upper: Int): Indexes[A] = new Slices[A](self, lower, upper)
  }
  
  final class Reverses[+A](self: Indexable[A]) extends Abstractly[A] {
    override def length: Int = self.length
    override def apply(index: Int): A = self.apply(length - index - 1)
    override def reversed: Indexes[A] = self.lazily
  }
  
  final class Maps[-A, +B](self: Indexable[A], f: A => B) extends Abstractly[B] {
    override def length: Int = self.length
    override def apply(index: Int): B = f(self.apply(index))
  }
  
  final class Filters[+A](self: Indexable[A], p: A => Boolean) extends Abstractly[A] {
    private[this] var filteredIndexes: Array[Int] = _
    private[this] def lookup: Array[Int] = synchronized {
      if (filteredIndexes == null) {
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
        if (k == size) filteredIndexes = indexes
        else {
          filteredIndexes = new Array[Int](k)
          Array.copy(indexes, 0, filteredIndexes, 0, k)
        }
      }
      filteredIndexes
    }
    override def length: Int = lookup.length
    override def apply(index: Int): A = self.apply(lookup(index))
  }
  
  final class Collects[-A, +B](self: Indexable[A], q: PartialFunction[A, B]) extends Abstractly[B] {
    private[this] var collectedIndexes: Array[Int] = _
    private[this] def lookup: Array[Int] = synchronized {
      if (collectedIndexes == null) {
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
        if (k == size) collectedIndexes = indexes
        else {
          collectedIndexes = new Array[Int](k)
          Array.copy(indexes, 0, collectedIndexes, 0, k)
        }
      }
      collectedIndexes
    }
    override def length: Int = lookup.length
    override def apply(index: Int): B = q(self.apply(lookup(index)))
  }
  
  final class Drops[+A](self: Indexable[A], lower: Int) extends Abstractly[A] {
    private[this] val from: Int = math.max(0, math.min(lower, self.length))
    override def length: Int = self.length - from
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      self.apply(from + index)
    }
  }
  
  final class Takes[+A](self: Indexable[A], upper: Int) extends Abstractly[A] {
    private[this] val limit: Int = math.max(0, math.min(upper, self.length))
    override def length: Int = limit
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      self.apply(index)
    }
  }
  
  final class Slices[+A](self: Indexable[A], lower: Int, upper: Int) extends Abstractly[A] {
    private[this] val from: Int = math.max(0, math.min(lower, self.length))
    private[this] val until: Int = math.max(from, math.min(upper, self.length))
    override def length: Int = until - from
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      self.apply(from + index)
    }
  }
}
