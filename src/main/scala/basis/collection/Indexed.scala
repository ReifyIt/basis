/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

import scala.annotation.tailrec

trait Indexed[+A] extends Any with Iterable[A] {
  def length: Int
  
  def apply(index: Int): A
  
  override def iterator: Iterated[A] = new Iterator(0, length)
  
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
      val x = apply(from)
      if (q.isDefinedAt(x)) Some(q(x)) else collectFirstBetween(from + 1, until, q)
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
      val x = apply(from)
      if (p(x)) Some(x) else findBetween(from + 1, until, p)
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
  
  override def view: IndexedView[A] = new View
  
  private final class Iterator(from: Int, until: Int) extends AbstractIterated[A] {
    private[this] var lower: Int = math.max(0, math.min(Indexed.this.length, from))
    private[this] var upper: Int = math.max(lower, math.min(Indexed.this.length, until))
    private[this] var index: Int = lower
    
    override def hasNext: Boolean = index < upper
    
    override def next(): A = {
      if (index >= upper) throw new NoSuchElementException("empty iterator")
      val result = Indexed.this.apply(index)
      index += 1
      result
    }
    
    override def drop(count: Int): Iterated[A] = new Iterator(index + count, upper)
    override def take(count: Int): Iterated[A] = new Iterator(index, index + count)
    override def slice(lower: Int, upper: Int): Iterated[A] = new Iterator(index + lower, index + upper)
  }
  
  private final class View extends AbstractIndexedView[A] {
    override def length: Int = Indexed.this.length
    override def apply(index: Int): A = Indexed.this.apply(index)
  }
}

private[basis] abstract class AbstractIndexed[+A] extends AbstractIterable[A] with Indexed[A]
