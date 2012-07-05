/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait IndexedView[+A] extends Any with IterableView[A] with Indexed[A] { self =>
  override def map[B](f: A => B): IndexedView[B] = new Mapped[B](f)
  
  override def filter(p: A => Boolean): IndexedView[A] = new Filtered(p)
  
  override def collect[B](q: PartialFunction[A, B]): IndexedView[B] = new Collected[B](q)
  
  override def drop(lower: Int): IndexedView[A] = new Dropped(lower)
  
  override def take(upper: Int): IndexedView[A] = new Taken(upper)
  
  override def slice(lower: Int, upper: Int): IndexedView[A] = new Sliced(lower, upper)
  
  private final class Mapped[+B](f: A => B) extends AbstractIndexedView[B] {
    override def length: Int = self.length
    override def apply(index: Int): B = f(self.apply(index))
  }
  
  private final class Filtered(p: A => Boolean) extends AbstractIndexedView[A] {
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
  
  private final class Collected[+B](q: PartialFunction[A, B]) extends AbstractIndexedView[B] {
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
  
  private final class Dropped(count: Int) extends AbstractIndexedView[A] {
    private[this] val lower: Int = math.max(0, math.min(self.length, count))
    override def length: Int = self.length - lower
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      self.apply(lower + index)
    }
  }
  
  private final class Taken(count: Int) extends AbstractIndexedView[A] {
    private[this] val upper: Int = math.max(0, math.min(self.length, count))
    override def length: Int = upper
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      self.apply(index)
    }
  }
  
  private final class Sliced(from: Int, until: Int) extends AbstractIndexedView[A] {
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
