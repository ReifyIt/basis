/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.sequential
package nonstrict

import basis.collections._

/** Non-strictly evaluated indexed sequence operations.
  * 
  * @groupprio  Mapping     -3
  * @groupprio  Filtering   -2
  * @groupprio  Combining   -1
  */
final class IndexedSeqOps[+A](val these: IndexedSeq[A]) extends AnyVal {
  /** Returns a view that applies a partial function to each element in this
    * sequence for which the function is defined.
    * 
    * @param  q   the partial function to lazily filter and map elements.
    * @return a non-strict view of the filtered and mapped elements.
    * @group  Mapping
    */
  def collect[B](q: PartialFunction[A, B]): IndexedSeq[B] =
    new IndexedSeqOps.Collect(these, q)
  
  /** Returns a view that applies a function to each element in this sequence.
    * 
    * @param  f   the function to lazily apply to each element.
    * @return a non-strict view of the mapped elements.
    * @group  Mapping
    */
  def map[B](f: A => B): IndexedSeq[B] =
    new IndexedSeqOps.Map(these, f)
  
  /** Returns a view of all elements in this sequence that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def filter(p: A => Boolean): IndexedSeq[A] =
    new IndexedSeqOps.Filter(these, p)
  
  /** Returns a view of all elements following the longest prefix of this
    * sequence for which each element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the suffix of accumulated elements beginning
    *         with the first element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: A => Boolean): IndexedSeq[A] =
    new IndexedSeqOps.DropWhile(these, p)
  
  /** Returns a view of the longest prefix of this sequence for which each
    * element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the longest prefix of elements preceding
    *         the first element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: A => Boolean): IndexedSeq[A] =
    new IndexedSeqOps.TakeWhile(these, p)
  
  /** Returns a (prefix, suffix) pair of views with the prefix being the
    * longest one for which each element satisfies a predicate, and the suffix
    * beginning with the first element to not satisfy the predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the (predix, suffix) pair of non-strict views.
    * @group  Filtering
    */
  def span(p: A => Boolean): (IndexedSeq[A], IndexedSeq[A]) =
    (takeWhile(p), dropWhile(p))
  
  /** Returns a view of all elements in this sequence following a prefix
    * up to some length.
    * 
    * @param  lower   the length of the prefix to drop; also the inclusive
    *                 lower bound for indexes of included elements.
    * @return a non-strict view of all but the first `lower` elements.
    * @group  Filtering
    */
  def drop(lower: Int): IndexedSeq[A] =
    new IndexedSeqOps.Drop(these, lower)
  
  /** Returns a view of a prefix of this sequence up to some length.
    * 
    * @param  upper   the length of the prefix to take; also the exclusive
    *                 upper bound for indexes of included elements.
    * @return a non-strict view of up to the first `upper` elements.
    * @group  Filtering
    */
  def take(upper: Int): IndexedSeq[A] =
    new IndexedSeqOps.Take(these, upper)
  
  /** Returns a view of an interval of elements in this sequence.
    * 
    * @param  lower   the inclusive lower bound for indexes of included elements.
    * @param  upper   the exclusive upper bound for indexes of included elements.
    * @return a non-strict view of the elements with indexes greater than or
    *         equal to `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int): IndexedSeq[A] =
    new IndexedSeqOps.Slice(these, lower, upper)
  
  /** Returns a view of the reverse of this sequence.
    * 
    * @return a non-strict view of the elements in this sequence in reverse order.
    * @group  Combining
    */
  def reverse: IndexedSeq[A] =
    new IndexedSeqOps.Reverse(these)
  
  /** Returns a view of pairs of elemnts from this and another sequence.
    * 
    * @param  those   the sequence whose elements to lazily pair with these elements.
    * @return a non-strict view of the pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B](those: IndexedSeq[B]): IndexedSeq[(A, B)] =
    new IndexedSeqOps.Zip(these, those)
  
  /** Returns a view concatenating this and another sequence.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: IndexedSeq[B]): IndexedSeq[B] =
    new IndexedSeqOps.++(these, those)
}

private[nonstrict] object IndexedSeqOps {
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
