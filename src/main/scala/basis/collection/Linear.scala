/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Linear[+A] extends Any with Iterable[A] {
  def isEmpty: Boolean
  
  def head: A
  
  def tail: Linear[A]
  
  override def iterator: Iterated[A] = new Iterator
  
  override def foreach[U](f: A => U) {
    var rest = this
    while (!rest.isEmpty) {
      f(rest.head)
      rest = rest.tail
    }
  }
  
  override def collectFirst[B](q: PartialFunction[A, B]): Option[B] = {
    var rest = this
    while (!rest.isEmpty) {
      val x = rest.head
      if (q.isDefinedAt(x)) return Some(q(x))
      rest = rest.tail
    }
    None
  }
  
  override def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    var rest = this
    while (!rest.isEmpty) {
      result = op(result, rest.head)
      rest = rest.tail
    }
    result
  }
  
  override def reduceLeft[B >: A](op: (B, A) => B): B = {
    if (isEmpty) throw new UnsupportedOperationException("empty reduce")
    tail.foldLeft[B](head)(op)
  }
  
  override def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    if (!isEmpty) Some(tail.foldLeft[B](head)(op)) else None
  
  override def find(p: A => Boolean): Option[A] = {
    var rest = this
    while (!rest.isEmpty) {
      if (p(rest.head)) return Some(rest.head)
      rest = rest.tail
    }
    None
  }
  
  override def forall(p: A => Boolean): Boolean = {
    var rest = this
    while (!rest.isEmpty) {
      if (!p(rest.head)) return false
      rest = rest.tail
    }
    true
  }
  
  override def exists(p: A => Boolean): Boolean = {
    var rest = this
    while (!rest.isEmpty) {
      if (p(rest.head)) return true
      rest = rest.tail
    }
    false
  }
  
  override def count(p: A => Boolean): Int = {
    var total = 0
    var rest = this
    while (!rest.isEmpty) {
      if (p(rest.head)) total += 1
      rest = rest.tail
    }
    total
  }
  
  override def view: LinearView[A] = new View
  
  private final class Iterator extends AbstractIterated[A] {
    private[this] var left: Linear[A] = Linear.this
    
    override def hasNext: Boolean = !left.isEmpty
    
    override def next(): A = {
      if (left.isEmpty) throw new NoSuchElementException("empty iterator")
      val result = left.head
      left = left.tail
      result
    }
  }
  
  private final class View extends AbstractLinearView[A] {
    override def isEmpty: Boolean = Linear.this.isEmpty
    override def head: A = Linear.this.head
    override def tail: Linear[A] = Linear.this.tail
  }
}

private[basis] abstract class AbstractLinear[+A] extends AbstractIterable[A] with Linear[A]
