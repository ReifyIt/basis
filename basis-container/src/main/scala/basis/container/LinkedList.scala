/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

sealed abstract class LinkedList[+A] extends List[A] {
  import scala.annotation.tailrec
  
  override type Self <: LinkedList[A]
  
  override def isEmpty: Boolean
  
  override def head: A
  
  override def tail: LinkedList[A]
  
  @tailrec protected final override def foreach[U](f: A => U) =
    if (!isEmpty) { f(head); tail.foreach[U](f) }
  
  @inline @tailrec final def dropWhile(p: A => Boolean): LinkedList[A] =
    if (isEmpty || !p(head)) this else tail.dropWhile(p)
  
  @tailrec final def drop(lower: Int): LinkedList[A] =
    if (isEmpty || lower <= 0) this else tail.drop(lower - 1)
}

final class ::[A](override val head: A, private[this] var next: LinkedList[A]) extends LinkedList[A] {
  override def isEmpty: Boolean = false
  
  override def tail: LinkedList[A] = next
  
  private[container] def tail_=(tail: LinkedList[A]): Unit = (next = tail)
}

object :: {
  def unapply[A](cons: ::[A]): Some[(A, LinkedList[A])] = Some((cons.head, cons.tail))
}

object Nil extends LinkedList[Nothing] {
  override def isEmpty: Boolean = true
  
  override def head: Nothing =
    throw new scala.NoSuchElementException("head of empty list")
  
  override def tail: LinkedList[Nothing] =
    throw new java.lang.UnsupportedOperationException("tail of empty list")
  
  override def toString: java.lang.String = "Nil"
}
