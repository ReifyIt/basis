/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import scala.annotation.tailrec

trait Indexable[+A] extends Any with Sequential[A] {
  def length: Int
  
  def apply(index: Int): A
  
  override def iterator: Iterator[A] = new Indexable.Elements[A](this, 0, length)
  
  override def foreach[U](f: A => U) {
    val until = length
    var i = 0
    while (i < until) {
      f(apply(i))
      i += 1
    }
  }
  
  @tailrec private def collectFirstBetween[B](from: Int, until: Int, q: PartialFunction[A, B]): Option[B] = {
    if (from >= until) None
    else {
      val head = apply(from)
      if (q.isDefinedAt(head)) Some(q(head)) else collectFirstBetween(from + 1, until, q)
    }
  }
  
  override def collectFirst[B](q: PartialFunction[A, B]): Option[B] = collectFirstBetween(0, length, q)
  
  @tailrec private def foldl[B](first: Int, last: Int, z: B, op: (B, A) => B): B =
    if (first > last) z else foldl(first + 1, last, op(z, apply(first)), op)
  
  override def foldLeft[B](z: B)(op: (B, A) => B): B = foldl(0, length - 1, z, op)
  
  override def reduceLeft[B >: A](op: (B, A) => B): B =
    if (length > 0) foldl(1, length - 1, apply(0), op)
    else throw new UnsupportedOperationException("empty reduce")
  
  override def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    if (length > 0) Some(foldl(1, length - 1, apply(0), op)) else None
  
  @tailrec private def foldr[B](first: Int, last: Int, z: B, op: (A, B) => B): B =
    if (first < last) z else foldr(first - 1, last, op(apply(first), z), op)
  
  def foldRight[B](z: B)(op: (A, B) => B): B = foldr(length - 1, 0, z, op)
  
  def reduceRight[B >: A](op: (A, B) => B): B =
    if (length > 0) foldr(length - 2, 0, apply(length - 1), op)
    else throw new UnsupportedOperationException("empty reduce")
  
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B] =
    if (length > 0) Some(foldr(length - 2, 0, apply(length - 1), op)) else None
  
  @tailrec private def findBetween(from: Int, until: Int, p: A => Boolean): Option[A] = {
    if (from >= until) None
    else {
      val head = apply(from)
      if (p(head)) Some(head) else findBetween(from + 1, until, p)
    }
  }
  
  override def find(p: A => Boolean): Option[A] = findBetween(0, length, p)
  
  @tailrec private def forallBetween(from: Int, until: Int, p: A => Boolean): Boolean =
    from >= until || (p(apply(from)) && forallBetween(from + 1, until, p))
  
  override def forall(p: A => Boolean): Boolean = forallBetween(0, length, p)
  
  @tailrec private def existsBetween(from: Int, until: Int, p: A => Boolean): Boolean =
    from < until && (p(apply(from)) || existsBetween(from + 1, until, p))
  
  override def exists(p: A => Boolean): Boolean = existsBetween(0, length, p)
  
  @tailrec private def countBetween(from: Int, until: Int, total: Int, p: A => Boolean): Int =
    if (from >= until) total else countBetween(from + 1, until, if (p(apply(from))) total + 1 else total, p)
  
  override def count(p: A => Boolean): Int = countBetween(0, length, 0, p)
  
  override def corresponds[B](that: Sequential[B])(p: (A, B) => Boolean): Boolean = that match {
    case that: Indexable[B] =>
      val limit = length
      limit == that.length && {
        var i = 0
        while (i < limit && p(apply(i), that.apply(i))) i += 1
        i == limit
      }
    case _ => super.corresponds[B](that)(p)
  }
  
  override def sameAs[B >: A](that: Iterable[B]): Boolean = that match {
    case that: Indexable[B] =>
      val limit = length
      limit == that.length && {
        var i = 0
        while (i < limit && apply(i) == that.apply(i)) i += 1
        i == limit
      }
    case _ => super.sameAs[B](that)
  }
  
  override def eagerly: Indexed[Any, A] = new Indexed.Projected[Any, A](this)
  
  override def lazily: Indexing[A] = new Indexing.Projecting[A](this)
  
  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3._
    var i = 0
    var h = 1829453087
    val limit = length
    while (i < limit) {
      h = mix(h, apply(i).##)
      i += 1
    }
    finalizeHash(h, i)
  }
}

object Indexable {
  abstract class Abstractly[+A] extends Sequential.Abstractly[A] with Indexable[A]
  
  final class Elements[+A](self: Indexable[A], lower: Int, upper: Int) extends Iterator.Abstract[A] {
    private[this] var index: Int = math.max(0, math.min(lower, self.length))
    private[this] var limit: Int = math.max(index, math.min(upper, self.length))
    override def hasNext: Boolean = index < limit
    override def next(): A = {
      if (index >= limit) Iterator.Empty.next()
      val result = self.apply(index)
      index += 1
      result
    }
    override def drop(lower: Int): Iterator[A] = new Elements[A](self, index + lower, limit)
    override def take(upper: Int): Iterator[A] = new Elements[A](self, index, index + upper)
    override def slice(lower: Int, upper: Int): Iterator[A] = new Elements[A](self, index + lower, index + upper)
  }
}
