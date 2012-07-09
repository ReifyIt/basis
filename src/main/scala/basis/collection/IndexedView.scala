/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait IndexedView[+A] extends Any with IterableView[A] with Indexed[A] {
  import IndexedView._
  
  def reversed: IndexedView[A] = new Reversed[A](this)
  
  override def map[B](f: A => B): IndexedView[B] = new Mapped[A, B](this, f)
  
  override def filter(p: A => Boolean): IndexedView[A] = new Filtered[A](this, p)
  
  override def collect[B](q: PartialFunction[A, B]): IndexedView[B] = new Collected[A, B](this, q)
  
  override def drop(lower: Int): IndexedView[A] = new Dropped[A](this, lower)
  
  override def take(upper: Int): IndexedView[A] = new Taken[A](this, upper)
  
  override def slice(lower: Int, upper: Int): IndexedView[A] = new Sliced[A](this, lower, upper)
  
  override def view: IndexedView[A] = this
}

private[basis] object IndexedView {
  final class Projected[+A](self: Indexed[A]) extends AbstractIndexedView[A] {
    override def length: Int = self.length
    override def apply(index: Int): A = self.apply(index)
    override def reversed: IndexedView[A] = new Reversed[A](self)
    override def map[B](f: A => B): IndexedView[B] = new Mapped[A, B](self, f)
    override def filter(p: A => Boolean): IndexedView[A] = new Filtered[A](self, p)
    override def collect[B](q: PartialFunction[A, B]): IndexedView[B] = new Collected[A, B](self, q)
    override def drop(lower: Int): IndexedView[A] = new Dropped[A](self, lower)
    override def take(upper: Int): IndexedView[A] = new Taken[A](self, upper)
    override def slice(lower: Int, upper: Int): IndexedView[A] = new Sliced[A](self, lower, upper)
  }
  
  final class Reversed[+A](self: Indexed[A]) extends AbstractIndexedView[A] {
    override def length: Int = self.length
    override def apply(index: Int): A = self.apply(length - index - 1)
    override def reversed: IndexedView[A] = self.view
  }
  
  final class Mapped[-A, +B](self: Indexed[A], f: A => B) extends AbstractIndexedView[B] {
    override def length: Int = self.length
    override def apply(index: Int): B = f(self.apply(index))
  }
  
  final class Filtered[+A](self: Indexed[A], p: A => Boolean) extends AbstractIndexedView[A] {
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
  
  final class Collected[-A, +B](self: Indexed[A], q: PartialFunction[A, B]) extends AbstractIndexedView[B] {
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
  
  final class Dropped[+A](self: Indexed[A], count: Int) extends AbstractIndexedView[A] {
    private[this] val lower: Int = math.max(0, math.min(self.length, count))
    override def length: Int = self.length - lower
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      self.apply(lower + index)
    }
  }
  
  final class Taken[+A](self: Indexed[A], count: Int) extends AbstractIndexedView[A] {
    private[this] val upper: Int = math.max(0, math.min(self.length, count))
    override def length: Int = upper
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      self.apply(index)
    }
  }
  
  final class Sliced[+A](self: Indexed[A], from: Int, until: Int) extends AbstractIndexedView[A] {
    private[this] val lower: Int = math.max(0, math.min(self.length, from))
    private[this] val upper: Int = math.max(lower, math.min(self.length, until))
    override def length: Int = upper - lower
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      self.apply(lower + index)
    }
  }
}

private[basis] abstract class AbstractIndexedView[+A] extends AbstractIndexed[A] with IndexedView[A]
