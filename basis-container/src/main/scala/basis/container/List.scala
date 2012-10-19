/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._
import basis.collection._
import basis.util._

import scala.annotation.tailrec

/** A singly linked list of elements.
  * 
  * @author Chris Sachs
  * 
  * @define collection  list
  */
sealed abstract class List[+A] extends Seq[A] {
  override type Self <: List[A]
  
  /** Returns the first element of this $collection. */
  def head: A
  
  /** Returns all but the first element of this $collection. */
  def tail: List[A]
  
  final def ::[B >: A](x: B): List[B] = new ::[B](x, this)
  
  @tailrec final def drop(lower: Int): List[A] =
    if (lower <= 0 || isEmpty) this else tail.drop(lower - 1)
  
  final def take(upper: Int): List[A] = {
    var xs = this
    val b = new List.Buffer[A]
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
  
  @tailrec protected final override def foreach[U](f: A => U) =
    if (!isEmpty) { f(head); tail.foreach[U](f) }
  
  final override def iterator: Iterator[A] =
    new List.Iterator[A](this)
  
  override def equals(other: Any): Boolean = other match {
    case that: List[A] =>
      var xs = this
      var ys = that
      var e = xs.isEmpty == ys.isEmpty
      while (e && !xs.isEmpty && !ys.isEmpty) {
        e = xs.head == ys.head
        xs = xs.tail
        ys = ys.tail
      }
      e
    case _ => false
  }
  
  override def hashCode: Int = {
    import MurmurHash3._
    var h = 2368702
    var xs = this
    while (!xs.isEmpty) {
      h = mix(h, xs.head.##)
      xs = xs.tail
    }
    mash(h)
  }
}

final class ::[A](override val head: A, private[this] var next: List[A]) extends List[A] {
  override def isEmpty: Boolean = false
  
  override def tail: List[A] = next
  
  private[basis] def tail_=(tail: List[A]): Unit = next = tail
  
  override def toString: String = {
    val s = new java.lang.StringBuilder("List")
    s.append('(')
    s.append(head)
    var xs = tail
    while (!xs.isEmpty) {
      s.append(", ").append(xs.head)
      xs = xs.tail
    }
    s.append(')')
    s.toString
  }
}

object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true
  
  override def head: Nothing =
    throw new scala.NoSuchElementException("head of empty list")
  
  override def tail: List[Nothing] =
    throw new java.lang.UnsupportedOperationException("tail of empty list")
  
  override def toString: String = "Nil"
}

object :: {
  def unapply[A](list: ::[A]): Some[(A, List[A])] = Some((list.head, list.tail))
}

object List extends SeqFactory[List] {
  def apply[A](xs: A*): List[A] = macro ListMacros.apply[A]
  
  implicit def Buffer[A]: List.Buffer[A] = new List.Buffer[A]
  
  final class Buffer[A] extends basis.Buffer[Any, A] {
    override type State = List[A]
    
    private[this] var last: ::[A] = _
    
    private[this] var first: List[A] = Nil
    
    private[this] var aliased: Boolean = true
    
    private[this] def prepare() {
      if (!first.isEmpty && aliased) {
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
    }
    
    override def += (element: A): this.type = {
      if (first.isEmpty) {
        last = new ::(element, Nil)
        first = last
      }
      else {
        prepare()
        val link = last
        last = new ::(element, Nil)
        link.tail = last
      }
      this
    }
    
    override def expect(count: Int): this.type = this
    /*
    override def ++= (those: Enumerator[A]): Unit = those match {
      case those: ::[A] =>
        prepare()
        last.tail = those
        while (!last.tail.isEmpty) last = last.tail.asInstanceOf[::[A]]
        aliased = true
      case _ => super.++=(those)
    }
    */
    override def state: List[A] = {
      aliased = true
      first
    }
    
    override def clear() {
      last = null
      first = Nil
      aliased = true
    }
  }
  
  private[basis] final class Iterator[+A]
      (private[this] var xs: List[A])
    extends basis.Iterator[A] {
    
    override def isEmpty: Boolean = xs.isEmpty
    
    override def head: A = {
      if (isEmpty) Iterator.empty.head
      else xs.head
    }
    
    override def step() {
      if (isEmpty) Iterator.empty.step()
      else xs = xs.tail
    }
    
    override def dup: List.Iterator[A] =
      new List.Iterator[A](xs)
  }
}
