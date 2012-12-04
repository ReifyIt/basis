/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers
package immutable

import basis.collections._
import basis.collections.generic._
import basis.collections.traversable._
import basis.util._

import scala.annotation.tailrec

/** An immutable singly linked list.
  * 
  * @define collection  list
  */
sealed abstract class List[+A]
  extends Equals
    with Immutable
    with Family[List[A]]
    with immutable.LinearSeq[A] {
  
  override def tail: List[A]
  
  @tailrec final def drop(lower: Int): List[A] =
    if (lower <= 0 || isEmpty) this else tail.drop(lower - 1)
  
  final def take(upper: Int): List[A] = {
    var xs = this
    val b = new ListBuilder[A]
    var i = 0
    while (i < upper && !xs.isEmpty) {
      i += 1
      b += xs.head
      xs = xs.tail
    }
    b.state
  }
  
  final def slice(lower: Int, upper: Int): List[A] =
    if (lower >= upper) Nil else drop(lower).take(upper)
  
  final override def length: Int = length(0)
  @tailrec private final def length(count: Int): Int =
    if (isEmpty) count else tail.length(count + 1)
  
  final def reverse: List[A] = reverse(Nil, this)
  @tailrec private[this] def reverse(sx: List[A], xs: List[A]): List[A] =
    if (xs.isEmpty) sx else reverse(xs.head :: sx, xs.tail)
  
  final def :: [B >: A](elem: B): List[B] = new ::[B](elem, this)
  
  final override def :+ [B >: A](elem: B): List[B] =
    (new ListBuilder[B] ++= this += elem).state
  
  final override def +: [B >: A](elem: B): List[B] = new ::[B](elem, this)
  
  @tailrec protected final override def foreach[U](f: A => U) =
    if (!isEmpty) { f(head); tail.foreach[U](f) }
  
  protected override def stringPrefix: String = "List"
}

final class ::[A](override val head: A, private[this] var next: List[A]) extends List[A] {
  override def isEmpty: Boolean = false
  
  override def tail: List[A] = next
  
  private[containers] def tail_=(tail: List[A]): Unit = next = tail
}

object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true
  
  override def head: Nothing =
    throw new NoSuchElementException("Head of empty list,")
  
  override def tail: List[Nothing] =
    throw new UnsupportedOperationException("Tail of empty list.")
}

object :: {
  def unapply[A](list: ::[A]): Some[(A, List[A])] = Some((list.head, list.tail))
}

object List extends SeqFactory[List] {
  // FIXME: don't hurt the compiler!
  //override def apply[A](xs: A*): List[A] = macro ListMacros.apply[A]
  
  implicit override def Builder[A]: Builder[Any, A] { type State = List[A] } = new ListBuilder
  
  override def toString: String = "List"
}

private[containers] final class ListBuilder[A] extends Builder[Any, A] {
  override type State = List[A]
  
  private[this] var last: ::[A] = _
  
  private[this] var first: List[A] = Nil
  
  private[this] var length: Int = 0
  
  private[this] var aliased: Int = -1 // index of the first aliased cons cell
  
  private[this] def prepare() {
    var xs = first
    if (aliased == 0) {
      last = new ::(xs.head, Nil)
      first = last
      xs = xs.tail
    }
    else if (aliased > 0) {
      var i = 0
      while (i < aliased) {
        last = xs.asInstanceOf[::[A]]
        xs = xs.tail
        i += 1
      }
    }
    if (aliased >= 0) {
      while (!xs.isEmpty) {
        val next = new ::(xs.head, Nil)
        last.tail = next
        last = next
        xs = xs.tail
      }
    }
    aliased = -1
  }
  
  override def += (element: A): this.type = {
    if (first.isEmpty) {
      last = new ::(element, Nil)
      first = last
    }
    else {
      prepare()
      val next = new ::(element, Nil)
      last.tail = next
      last = next
    }
    length += 1
    this
  }
  
  override def ++= (xs: Enumerator[A]): this.type = xs match {
    case Nil => this
    case xs: ::[A] =>
      if (first.isEmpty) {
        last = xs
        first = last
        aliased = 0
      }
      else {
        prepare()
        last.tail = xs
        aliased = length + 1
      }
      while (!last.tail.isEmpty) {
        last = last.tail.asInstanceOf[::[A]]
        length += 1
      }
      this
    case _ => super.++=(xs)
  }
  
  override def expect(count: Int): this.type = this
  
  override def state: List[A] = {
    if (length > 0) aliased = 0
    first
  }
  
  override def clear() {
    last = null
    first = Nil
    length = 0
    aliased = -1
  }
}
