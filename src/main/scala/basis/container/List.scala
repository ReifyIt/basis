/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis.collection._

sealed abstract class List[+A] private[container] extends More[A] {
  import scala.annotation.tailrec
  
  override type Self <: List[A]
  
  override def isEmpty: Boolean
  
  override def head: A
  
  override def tail: List[A]
  
  @inline @tailrec final override def foreach[U](f: A => U) =
    if (!isEmpty) { f(head); tail.foreach[U](f) }
  
  @tailrec final def select[B](q: PartialFunction[A, B]): Option[B] =
    if (isEmpty) None else if (q.isDefinedAt(head)) Some(q(head)) else tail.select[B](q)
  
  @inline @tailrec final def fold[B >: A](z: B)(op: (B, B) => B): B =
    if (isEmpty) z else tail.fold[B](op(z, head))(op)
  
  @inline final def reduce[B >: A](op: (B, B) => B): B =
    if (!isEmpty) tail.fold[B](head)(op) else throw new UnsupportedOperationException
  
  @inline final def reduceOption[B >: A](op: (B, B) => B): Option[B] =
    if (isEmpty) None else Some(tail.fold[B](head)(op))
  
  @inline @tailrec final def foldLeft[B](z: B)(op: (B, A) => B): B =
    if (isEmpty) z else tail.foldLeft[B](op(z, head))(op)
  
  @inline final def reduceLeft[B >: A](op: (B, A) => B): B =
    if (!isEmpty) tail.foldLeft[B](head)(op) else throw new UnsupportedOperationException
  
  @inline final def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    if (isEmpty) None else Some(tail.foldLeft[B](head)(op))
  
  @inline @tailrec final def find(p: A => Boolean): Option[A] =
    if (isEmpty) None else if (p(head)) Some(head) else tail.find(p)
  
  @inline @tailrec final def forall(p: A => Boolean): Boolean =
    isEmpty || (p(head) && tail.forall(p))
  
  @inline @tailrec final def exists(p: A => Boolean): Boolean =
    !isEmpty && (p(head) || tail.exists(p))
  
  @inline final def count(p: A => Boolean): Int = {
    var these = this
    var total = 0
    while (!these.isEmpty) {
      if (p(these.head)) total += 1
      these = these.tail
    }
    total
  }
  
  //@inline @tailrec final def map[B](f: A => B)(implicit make: Make[List[A], B]): make.What =
  //  if (isEmpty) make.result else { make += f(head); tail.map[B](f)(make) }
  
  @inline final def map[B](f: A => B)(implicit make: Make[List[A], B]): make.What = {
    var these = this
    while (!these.isEmpty) {
      make += f(these.head)
      these = these.tail
    }
    make.result
  }
  
  @inline final def flatMap[B](f: A => List[B])(implicit make: Make[List[A], B]): make.What = {
    var these = this
    while (!these.isEmpty) {
      var those = f(these.head)
      while (!those.isEmpty) {
        make += those.head
        those = those.tail
      }
      these = these.tail
    }
    make.result
  }
  
  @inline @tailrec final def filter(p: A => Boolean)(implicit make: Make[List[A], A]): make.What =
    if (isEmpty) make.result else { if (p(head)) make += head; tail.filter(p) }
  
  //@tailrec final def collect[B](q: PartialFunction[A, B])(implicit make: Make[List[A], B]): make.What =
  //  if (isEmpty) make.result else { if (q.isDefinedAt(head)) make += q(head); tail.collect[B](q)(make) }
  
  final def collect[B](q: PartialFunction[A, B])(implicit make: Make[List[A], B]): make.What = {
    var these = this
    while (!these.isEmpty) {
      val x = these.head
      if (q.isDefinedAt(x)) make += q(x)
      these = these.tail
    }
    make.result
  }
  
  @inline @tailrec final def dropWhile(p: A => Boolean): List[A] =
    if (isEmpty || !p(head)) this else tail.dropWhile(p)
  
  @inline @tailrec final def takeWhile(p: A => Boolean)(implicit make: Make[List[A], A]): make.What =
    if (isEmpty || !p(head)) make.result else { make += head; tail.takeWhile(p) }
  
  @inline @tailrec final def span(p: A => Boolean)(implicit make: Make[List[A], A]): (make.What, List[A]) =
    if (isEmpty || !p(head)) (make.result, this) else { make += head; tail.span(p) }
  
  @tailrec final def drop(lower: Int): List[A] = if (isEmpty || lower <= 0) this else tail.drop(lower - 1)
  
  @tailrec final def take(upper: Int)(implicit make: Make[List[A], A]): make.What =
    if (isEmpty || upper <= 0) make.result else { make += head; tail.take(upper - 1) }
  
  final def slice(lower: Int, upper: Int): List[A] = drop(lower).take(upper)
  
  protected override def stringPrefix: String = "List"
}

final class ::[A](override val head: A, private[this] var next: List[A]) extends List[A] {
  override def isEmpty: Boolean = false
  
  override def tail: List[A] = next
  
  private[container] def tail_=(tail: List[A]): Unit = next = tail
}

object :: {
  def unapply[A](cons: ::[A]): Some[(A, List[A])] = Some(cons.head, cons.tail)
}

object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true
  
  override def head: Nothing = throw new NoSuchElementException("head of empty list")
  
  override def tail: List[Nothing] = throw new UnsupportedOperationException("tail of empty list")
  
  override def toString: String = "Nil"
}

object List {
  implicit def Maker[A]: Maker[A] = new Maker[A]
  
  final class Maker[A] extends Make[Any, A] {
    override type What = List[A]
    
    private[this] var last: ::[A] = _
    private[this] var first: List[A] = Nil
    private[this] var aliased: Boolean = false
    
    private[this] def prepare(): Unit = if (aliased) {
      var these = first
      last = new ::(these.head, Nil)
      first = last
      these = these.tail
      while (!these.isEmpty) {
        val link = last
        last = new ::(these.head, Nil)
        link.tail = last
        these = these.tail
      }
      aliased = false
    }
    
    override def expect(count: Int): Unit = ()
    
    override def += (that: A) {
      if (first.isEmpty) {
        last = new ::(that, Nil)
        first = last
      }
      else {
        prepare()
        val link = last
        last = new ::(that, Nil)
        link.tail = last
      }
    }
    
    override def ++= (those: Each[A]): Unit = those match {
      case those: ::[A] =>
        prepare()
        last.tail = those
        while (!last.tail.isEmpty) last = last.tail.asInstanceOf[::[A]]
        aliased = true
      case _ => super.++=(those)
    }
    
    override def result(): List[A] = {
      if (!first.isEmpty) aliased = true
      first
    }
    
    override def clear() {
      first = Nil
      last = null
      aliased = false
    }
  }
}
