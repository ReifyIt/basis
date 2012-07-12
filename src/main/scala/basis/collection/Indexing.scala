/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Indexing[+A] extends Any with Sequencing[A] with Indexable[A] {
  import Indexing._
  
  override def length: Int
  
  override def apply(index: Int): A
  
  def reverse: Indexing[A] = new Reversing[A](this)
  
  override def map[B](f: A => B): Indexing[B] = new Mapping[A, B](this, f)
  
  override def filter(p: A => Boolean): Indexing[A] = new Filtering[A](this, p)
  
  override def collect[B](q: PartialFunction[A, B]): Indexing[B] = new Collecting[A, B](this, q)
  
  override def drop(lower: Int): Indexing[A] = new Dropping[A](this, lower)
  
  override def take(upper: Int): Indexing[A] = new Taking[A](this, upper)
  
  override def slice(lower: Int, upper: Int): Indexing[A] = new Slicing[A](this, lower, upper)
  
  override def lazily: Indexing[A] = this
}

object Indexing {
  abstract class Abstractly[+A] extends Indexable.Abstractly[A] with Indexing[A]
  
  final class Projecting[+A](self: Indexable[A]) extends Abstractly[A] {
    override def length: Int = self.length
    override def apply(index: Int): A = self.apply(index)
    override def reverse: Indexing[A] = new Reversing[A](self)
    override def map[B](f: A => B): Indexing[B] = new Mapping[A, B](self, f)
    override def filter(p: A => Boolean): Indexing[A] = new Filtering[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): Indexing[B] = new Collecting[A, B](self, q)
    override def drop(lower: Int): Indexing[A] = new Dropping[A](self, lower)
    override def take(upper: Int): Indexing[A] = new Taking[A](self, upper)
    override def slice(lower: Int, upper: Int): Indexing[A] = new Slicing[A](self, lower, upper)
  }
  
  final class Reversing[+A](self: Indexable[A]) extends Abstractly[A] {
    override def length: Int = self.length
    override def apply(index: Int): A = self.apply(length - index - 1)
    override def reverse: Indexing[A] = self.lazily
  }
  
  final class Mapping[-A, +B](self: Indexable[A], f: A => B) extends Abstractly[B] {
    override def length: Int = self.length
    override def apply(index: Int): B = f(self.apply(index))
  }
  
  final class Filtering[+A](self: Indexable[A], p: A => Boolean) extends Abstractly[A] {
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
  
  final class Collecting[-A, +B](self: Indexable[A], q: PartialFunction[A, B]) extends Abstractly[B] {
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
  
  final class Dropping[+A](self: Indexable[A], lower: Int) extends Abstractly[A] {
    private[this] val from: Int = math.max(0, math.min(lower, self.length))
    override def length: Int = self.length - from
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      self.apply(from + index)
    }
  }
  
  final class Taking[+A](self: Indexable[A], upper: Int) extends Abstractly[A] {
    private[this] val limit: Int = math.max(0, math.min(upper, self.length))
    override def length: Int = limit
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      self.apply(index)
    }
  }
  
  final class Slicing[+A](self: Indexable[A], lower: Int, upper: Int) extends Abstractly[A] {
    private[this] val from: Int = math.max(0, math.min(lower, self.length))
    private[this] val until: Int = math.max(from, math.min(upper, self.length))
    override def length: Int = until - from
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      self.apply(from + index)
    }
  }
}
