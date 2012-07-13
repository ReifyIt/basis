/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.collection

trait Listable[+A] extends Any with Sequential[A] {
  def isEmpty: Boolean
  
  def head: A
  
  def tail: Listable[A]
  
  override def iterator: Iterator[A] = new Listable.Elements[A](this)
  
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
      if (q.isDefinedAt(rest.head)) return Some(q(rest.head))
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
  
  override def corresponds[B](that: Sequential[B])(p: (A, B) => Boolean): Boolean = that match {
    case that: Listable[B] =>
      var these = this
      var those = that
      while (!these.isEmpty && !those.isEmpty && these.head == those.head) {
        these = these.tail
        those = those.tail
      }
      these.isEmpty && those.isEmpty
    case _ => super.sameAs(that)
  }
  
  override def sameAs[B >: A](that: Iterable[B]): Boolean = that match {
    case that: Listable[B] =>
      var these = this
      var those = that
      while (!these.isEmpty && !those.isEmpty && these.head == those.head) {
        these = these.tail
        those = those.tail
      }
      these.isEmpty && those.isEmpty
    case _ => super.sameAs(that)
  }
  
  override def eagerly: Listed[A] = new Listed.Projected[A](this)
  
  override def lazily: Listing[A] = new Listing.Projecting[A](this)
  
  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3._
    var rest = this
    var h = 1829453087
    var i = 0
    while (!rest.isEmpty) {
      h = mix(h, rest.head.##)
      rest = rest.tail
      i += 1
    }
    finalizeHash(h, i)
  }
}

private[basis] object Listable {
  private[basis] abstract class Abstractly[+A] extends Sequential.Abstractly[A] with Listable[A]
  
  private[basis] final class Elements[+A](self: Listable[A]) extends Iterator.Abstract[A] {
    private[this] var rest: Listable[A] = self
    override def hasNext: Boolean = !rest.isEmpty
    override def next(): A = {
      if (rest.isEmpty) Iterator.Empty.next()
      val result = rest.head
      rest = rest.tail
      result
    }
  }
}
