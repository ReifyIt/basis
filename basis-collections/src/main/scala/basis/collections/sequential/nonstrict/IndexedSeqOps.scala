/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collections
package sequential
package nonstrict

import traversable._

/** Non-strictly evaluated indexed sequence operations.
  * 
  * @groupprio  Traversing  -6
  * @groupprio  Reducing    -5
  * @groupprio  Querying    -4
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  * 
  * @define collection  sequence
  */
trait IndexedSeqOps[+A, +From]
  extends Any
    with SeqOps[A, From]
    with general.IndexedSeqOps[A, From] {
  
  protected[this] override def self: IndexedSeq[A]
  
  override def collect[B](q: PartialFunction[A, B]): IndexedSeq[B] =
    new IndexedSeqView.Collect(self, q)
  
  override def map[B](f: A => B): IndexedSeq[B] =
    new IndexedSeqView.Map(self, f)
  
  override def filter(p: A => Boolean): IndexedSeq[A] =
    new IndexedSeqView.Filter(self, p)
  
  override def dropWhile(p: A => Boolean): IndexedSeq[A] =
    new IndexedSeqView.DropWhile(self, p)
  
  override def takeWhile(p: A => Boolean): IndexedSeq[A] =
    new IndexedSeqView.TakeWhile(self, p)
  
  override def span(p: A => Boolean): (IndexedSeq[A], IndexedSeq[A]) =
    (takeWhile(p), dropWhile(p))
  
  override def drop(lower: Int): IndexedSeq[A] =
    new IndexedSeqView.Drop(self, lower)
  
  override def take(upper: Int): IndexedSeq[A] =
    new IndexedSeqView.Take(self, upper)
  
  override def slice(lower: Int, upper: Int): IndexedSeq[A] =
    new IndexedSeqView.Slice(self, lower, upper)
  
  /** Returns a reversed view of this $collection.
    * 
    * @return a non-strict view of the elements in this $collection in reverse order.
    * @group  Combining
    */
  def reverse: IndexedSeq[A] =
    new IndexedSeqView.Reverse(self)
}

class IndexedSeqView[+A, +From](override val self: IndexedSeq[A])
  extends AnyVal with IndexedSeqOps[A, From] {
  
  override def collect[B](q: PartialFunction[A, B]): IndexedSeq[B] =
    new IndexedSeqView.Collect(self, q)
  
  override def map[B](f: A => B): IndexedSeq[B] =
    new IndexedSeqView.Map(self, f)
  
  override def filter(p: A => Boolean): IndexedSeq[A] =
    new IndexedSeqView.Filter(self, p)
  
  override def dropWhile(p: A => Boolean): IndexedSeq[A] =
    new IndexedSeqView.DropWhile(self, p)
  
  override def takeWhile(p: A => Boolean): IndexedSeq[A] =
    new IndexedSeqView.TakeWhile(self, p)
  
  override def span(p: A => Boolean): (IndexedSeq[A], IndexedSeq[A]) =
    (takeWhile(p), dropWhile(p))
  
  override def drop(lower: Int): IndexedSeq[A] =
    new IndexedSeqView.Drop(self, lower)
  
  override def take(upper: Int): IndexedSeq[A] =
    new IndexedSeqView.Take(self, upper)
  
  override def slice(lower: Int, upper: Int): IndexedSeq[A] =
    new IndexedSeqView.Slice(self, lower, upper)
  
  override def reverse: IndexedSeq[A] =
    new IndexedSeqView.Reverse(self)
  
  /** Returns a view of pairs of elemnts from this and another $collection.
    * 
    * @param  those   the $collection whose elements to lazily pair with these elements.
    * @return a non-strict view of the pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B](those: IndexedSeq[B]): IndexedSeq[(A, B)] =
    new IndexedSeqView.Zip(self, those)
  
  /** Returns a view concatenating this and another $collection.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: IndexedSeq[B]): IndexedSeq[B] =
    new IndexedSeqView.++(self, those)
}

private[nonstrict] object IndexedSeqView {
  import scala.annotation.tailrec
  import basis.util.IntOps
  
  final class Collect[-A, +B](base: IndexedSeq[A], q: PartialFunction[A, B]) extends IndexedSeq[B] {
    private[this] var table: Array[Int] = _
    private[this] def lookup: Array[Int] = synchronized {
      if (table == null) {
        val n = base.length
        var i = 0
        var j = 0
        table = new Array[Int](n)
        while (j < n) {
          if (q.isDefinedAt(base(j))) {
            table(i) = j
            i += 1
          }
          j += 1
        }
        if (i != n) {
          val newTable = new Array[Int](i)
          java.lang.System.arraycopy(table, 0, newTable, 0, i)
          table = newTable
        }
      }
      table
    }
    
    override def length: Int = lookup.length
    
    override def apply(index: Int): B = q(base(lookup(index)))
  }
  
  final class Map[-A, +B](base: IndexedSeq[A], f: A => B) extends IndexedSeq[B] {
    override def length: Int = base.length
    
    override def apply(index: Int): B = f(base(index))
  }
  
  final class Filter[+A](base: IndexedSeq[A], p: A => Boolean) extends IndexedSeq[A] {
    private[this] var table: Array[Int] = _
    private[this] def lookup: Array[Int] = synchronized {
      if (table == null) {
        val n = base.length
        var i = 0
        var j = 0
        table = new Array[Int](n)
        while (j < n) {
          if (p(base(j))) {
            table(i) = j
            i += 1
          }
          j += 1
        }
        if (i != n) {
          val newTable = new Array[Int](i)
          java.lang.System.arraycopy(table, 0, newTable, 0, i)
          table = newTable
        }
      }
      table
    }
    
    override def length: Int = table.length
    
    override def apply(index: Int): A = base(lookup(index))
  }
  
  final class DropWhile[+A](base: IndexedSeq[A], p: A => Boolean) extends IndexedSeq[A] {
    private[this] var lower: Int = -1
    private[this] def offset: Int = synchronized {
      if (lower < 0) {
        val n = base.length
        lower = 0
        while (lower < n && p(base(lower))) lower += 1
      }
      lower
    }
    
    override def length: Int = base.length - offset
    
    override def apply(index: Int): A = {
      val i = offset + index
      if (i < 0 || i >= length) throw new IndexOutOfBoundsException(index.toString)
      base(i)
    }
  }
  
  final class TakeWhile[+A](base: IndexedSeq[A], p: A => Boolean) extends IndexedSeq[A] {
    private[this] var upper: Int = -1
    
    override def length: Int = synchronized {
      if (upper < 0) {
        val n = base.length
        upper = 0
        while (upper < n && p(base(upper))) upper += 1
      }
      upper
    }
    
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      base(index)
    }
  }
  
  final class Drop[+A](base: IndexedSeq[A], lower: Int) extends IndexedSeq[A] {
    private[this] val offset: Int = 0 max lower min base.length
    
    override def length: Int = base.length - offset
    
    override def apply(index: Int): A = {
      val i = offset + index
      if (i < 0 || i >= length) throw new IndexOutOfBoundsException(index.toString)
      base(i)
    }
  }
  
  final class Take[+A](base: IndexedSeq[A], upper: Int) extends IndexedSeq[A] {
    override val length: Int = 0 max upper min base.length
    
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      base(index)
    }
  }
  
  final class Slice[+A](base: IndexedSeq[A], lower: Int, upper: Int) extends IndexedSeq[A] {
    private[this] val offset: Int = 0 max lower min base.length
    
    override val length: Int = (offset max upper min base.length) - offset
    
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      base(offset + index)
    }
  }
  
  final class Reverse[+A](base: IndexedSeq[A]) extends IndexedSeq[A] {
    override def length: Int = base.length
    
    override def apply(index: Int): A = {
      val n = base.length
      if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
      base(n - index - 1)
    }
  }
  
  final class Zip[+A, +B](xs: IndexedSeq[A], ys: IndexedSeq[B]) extends IndexedSeq[(A, B)] {
    override val length: Int = xs.length min ys.length
    
    override def apply(index: Int): (A, B) = (xs(index), ys(index))
  }
  
  final class ++[+A](xs: IndexedSeq[A], ys: IndexedSeq[A]) extends IndexedSeq[A] {
    override val length: Int = xs.length + ys.length
    
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      val n = xs.length
      if (index < n) xs(index) else ys(index - n)
    }
  }
}
