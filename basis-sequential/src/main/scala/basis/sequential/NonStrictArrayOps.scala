/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.sequential

import basis.collections._

/** Non-strictly evaluated array operations.
  * 
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  * @group    NonStrict
  * 
  * @groupprio  Mapping     1
  * @groupprio  Filtering   2
  * @groupprio  Combining   3
  * 
  * @define collection  array
  */
final class NonStrictArrayOps[A](val these: Array[A]) extends AnyVal {
  /** Returns a view that applies a partial function to each element in this
    * $collection for which the function is defined.
    * 
    * @param  q   the partial function to lazily filter and map elements.
    * @return a non-strict view of the filtered and mapped elements.
    * @group  Mapping
    */
  def collect[B](q: PartialFunction[A, B]): Index[B] =
    new NonStrictArrayOps.Collect(these, q)
  
  /** Returns a view that applies a function to each element in this $collection.
    * 
    * @param  f   the function to lazily apply to each element.
    * @return a non-strict view of the mapped elements.
    * @group  Mapping
    */
  def map[B](f: A => B): Index[B] =
    new NonStrictArrayOps.Map(these, f)
  
  /** Returns a view of all elements in this $collection that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def filter(p: A => Boolean): Index[A] =
    new NonStrictArrayOps.Filter(these, p)
  
  /** Returns a view of all elements in this $collection that satisfy a predicate.
    * 
    * @param  p   the predicate to lazily test elements against.
    * @return a non-strict view of the filtered elements.
    * @group  Filtering
    */
  def withFilter(p: A => Boolean): Index[A] =
    new NonStrictArrayOps.Filter(these, p)
  
  /** Returns a view of all elements following the longest prefix of this
    * $collection for which each element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the suffix of accumulated elements beginning
    *         with the first element to not satisfy `p`.
    * @group  Filtering
    */
  def dropWhile(p: A => Boolean): Index[A] =
    new NonStrictArrayOps.DropWhile(these, p)
  
  /** Returns a view of the longest prefix of this $collection for which each
    * element satisfies a predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return a non-strict view of the longest prefix of elements preceding
    *         the first element to not satisfy `p`.
    * @group  Filtering
    */
  def takeWhile(p: A => Boolean): Index[A] =
    new NonStrictArrayOps.TakeWhile(these, p)
  
  /** Returns a (prefix, suffix) pair of views with the prefix being the
    * longest one for which each element satisfies a predicate, and the suffix
    * beginning with the first element to not satisfy the predicate.
    * 
    * @param  p   the predicate to test elements against.
    * @return the (predix, suffix) pair of non-strict views.
    * @group  Filtering
    */
  def span(p: A => Boolean): (Index[A], Index[A]) =
    (takeWhile(p), dropWhile(p))
  
  /** Returns a view of all elements in this $collection following a prefix
    * up to some length.
    * 
    * @param  lower   the length of the prefix to drop; also the inclusive
    *                 lower bound for indexes of included elements.
    * @return a non-strict view of all but the first `lower` elements.
    * @group  Filtering
    */
  def drop(lower: Int): Index[A] =
    new NonStrictArrayOps.Drop(these, lower)
  
  /** Returns a view of a prefix of this $collection up to some length.
    * 
    * @param  upper   the length of the prefix to take; also the exclusive
    *                 upper bound for indexes of included elements.
    * @return a non-strict view of up to the first `upper` elements.
    * @group  Filtering
    */
  def take(upper: Int): Index[A] =
    new NonStrictArrayOps.Take(these, upper)
  
  /** Returns a view of an interval of elements in this $collection.
    * 
    * @param  lower   the inclusive lower bound for indexes of included elements.
    * @param  upper   the exclusive upper bound for indexes of included elements.
    * @return a non-strict view of the elements with indexes greater than or
    *         equal to `lower` and less than `upper`.
    * @group  Filtering
    */
  def slice(lower: Int, upper: Int): Index[A] =
    new NonStrictArrayOps.Slice(these, lower, upper)
  
  /** Returns a view of the reverse of this $collection.
    * 
    * @return a non-strict view of the elements in this $collection in reverse order.
    * @group  Combining
    */
  def reverse: Index[A] =
    new NonStrictArrayOps.Reverse(these)
  
  /** Returns a view of pairs of elemnts from this and another $collection.
    * 
    * @param  those   the $collection whose elements to lazily pair with these elements.
    * @return a non-strict view of the pairs of corresponding elements.
    * @group  Combining
    */
  def zip[B](those: Array[B]): Index[(A, B)] =
    new NonStrictArrayOps.Zip(these, those)
  
  /** Returns a view concatenating this and another $collection.
    * 
    * @param  those   the elements to append to these elements.
    * @return a non-strict view of the concatenated elements.
    * @group Combining
    */
  def ++ [B >: A](those: Array[B]): Index[B] =
    new NonStrictArrayOps.++(these.asInstanceOf[Array[B]], those)
}

private[sequential] object NonStrictArrayOps {
  import scala.annotation.tailrec
  import basis.util.IntOps
  
  final class Collect[-A, +B](these: Array[A], q: PartialFunction[A, B]) extends Index[B] {
    private[this] var table: Array[Int] = _
    private[this] def lookup: Array[Int] = synchronized {
      if (table == null) {
        val n = these.length
        var i = 0
        var j = 0
        table = new Array[Int](n)
        while (j < n) {
          if (q.isDefinedAt(these(j))) {
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
    
    override def apply(index: Int): B = q(these(lookup(index)))
  }
  
  final class Map[-A, +B](these: Array[A], f: A => B) extends Index[B] {
    override def length: Int = these.length
    
    override def apply(index: Int): B = f(these(index))
  }
  
  final class Filter[+A](these: Array[A], p: A => Boolean) extends Index[A] {
    private[this] var table: Array[Int] = _
    private[this] def lookup: Array[Int] = synchronized {
      if (table == null) {
        val n = these.length
        var i = 0
        var j = 0
        table = new Array[Int](n)
        while (j < n) {
          if (p(these(j))) {
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
    
    override def apply(index: Int): A = these(lookup(index))
  }
  
  final class DropWhile[+A](these: Array[A], p: A => Boolean) extends Index[A] {
    private[this] var lower: Int = -1
    private[this] def offset: Int = synchronized {
      if (lower < 0) {
        val n = these.length
        lower = 0
        while (lower < n && p(these(lower))) lower += 1
      }
      lower
    }
    
    override def length: Int = these.length - offset
    
    override def apply(index: Int): A = {
      val i = offset + index
      if (i < 0 || i >= length) throw new IndexOutOfBoundsException(index.toString)
      these(i)
    }
  }
  
  final class TakeWhile[+A](these: Array[A], p: A => Boolean) extends Index[A] {
    private[this] var upper: Int = -1
    
    override def length: Int = synchronized {
      if (upper < 0) {
        val n = these.length
        upper = 0
        while (upper < n && p(these(upper))) upper += 1
      }
      upper
    }
    
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      these(index)
    }
  }
  
  final class Drop[+A](these: Array[A], lower: Int) extends Index[A] {
    private[this] val offset: Int = 0 max lower min these.length
    
    override def length: Int = these.length - offset
    
    override def apply(index: Int): A = {
      val i = offset + index
      if (i < 0 || i >= length) throw new IndexOutOfBoundsException(index.toString)
      these(i)
    }
  }
  
  final class Take[+A](these: Array[A], upper: Int) extends Index[A] {
    override val length: Int = 0 max upper min these.length
    
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      these(index)
    }
  }
  
  final class Slice[+A](these: Array[A], lower: Int, upper: Int) extends Index[A] {
    private[this] val offset: Int = 0 max lower min these.length
    
    override val length: Int = (offset max upper min these.length) - offset
    
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      these(offset + index)
    }
  }
  
  final class Reverse[+A](these: Array[A]) extends Index[A] {
    override def length: Int = these.length
    
    override def apply(index: Int): A = {
      val n = these.length
      if (index < 0 || index >= n) throw new IndexOutOfBoundsException(index.toString)
      these(n - index - 1)
    }
  }
  
  final class Zip[+A, +B](these: Array[A], those: Array[B]) extends Index[(A, B)] {
    override val length: Int = these.length min those.length
    
    override def apply(index: Int): (A, B) = (these(index), those(index))
  }
  
  final class ++[+A](these: Array[A], those: Array[A]) extends Index[A] {
    override val length: Int = these.length + those.length
    
    override def apply(index: Int): A = {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      val n = these.length
      if (index < n) these(index) else those(index - n)
    }
  }
}
