/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.util._

import scala.annotation.tailrec
import scala.reflect.ClassTag

/** A singly linked list.
  * 
  * @groupprio  Examining     -8
  * @groupprio  Mutating      -7
  * @groupprio  Inserting     -6
  * @groupprio  Removing      -5
  * @groupprio  Iterating     -4
  * @groupprio  Traversing    -3
  * @groupprio  Converting    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  list
  */
sealed abstract class List[+A]
  extends Equals
    with Family[List[A]]
    with Stack[A]
    with ListLike[A] {
  
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
  
  final def :: [B >: A](elem: B): List[B] = elem match {
    case elem: Int => new IntCons(elem, this).asInstanceOf[List[B]]
    case elem: Long => new LongCons(elem, this).asInstanceOf[List[B]]
    case elem: Float => new FloatCons(elem, this).asInstanceOf[List[B]]
    case elem: Double => new DoubleCons(elem, this).asInstanceOf[List[B]]
    case _ => new RefCons(elem, this)
  }
  
  final override def toList: this.type = this
  
  final override def iterator: Iterator[A] = new ListIterator(this)
  
  @tailrec protected final override def foreach[U](f: A => U) =
    if (!isEmpty) { f(head); tail.foreach[U](f) }
  
  protected override def stringPrefix: String = "List"
}

object List extends SeqFactory[List] {
  implicit override def Builder[A : ClassTag]
    : Builder[Any, A] { type State = List[A] } =
    new ListBuilder
  
  override def toString: String = "List"
}

sealed abstract class ::[A] extends List[A] {
  final override def isEmpty: Boolean = false
  
  private[containers] def tail_=(tail: List[A]): Unit
}

object :: {
  def apply[A](x: A, xs: List[A]): ::[A] = x match {
    case x: Int => new IntCons(x, xs).asInstanceOf[::[A]]
    case x: Long => new LongCons(x, xs).asInstanceOf[::[A]]
    case x: Float => new FloatCons(x, xs).asInstanceOf[::[A]]
    case x: Double => new DoubleCons(x, xs).asInstanceOf[::[A]]
    case _ => new RefCons(x, xs)
  }
  
  def unapply[A](list: ::[A]): Some[(A, List[A])] = Some((list.head, list.tail))
}

object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException("Head of empty list,")
  override def tail: List[Nothing] = throw new UnsupportedOperationException("Tail of empty list.")
}

private[containers] final class IntCons(
    override val head: Int,
    override var tail: List[Any])
  extends ::[Any]

private[containers] final class LongCons(
    override val head: Long,
    override var tail: List[Any])
  extends ::[Any]

private[containers] final class FloatCons(
    override val head: Float,
    override var tail: List[Any])
  extends ::[Any]

private[containers] final class DoubleCons(
    override val head: Double,
    override var tail: List[Any])
  extends ::[Any]

private[containers] final class RefCons[A](
    override val head: A,
    override var tail: List[A])
  extends ::[A]

private[containers] final class ListIterator[+A](private[this] var xs: List[A]) extends Iterator[A] {
  override def isEmpty: Boolean = xs.isEmpty
  
  override def head: A = {
    if (xs.isEmpty) throw new NoSuchElementException("Head of empty iterator.")
    xs.head
  }
  
  override def step() {
    if (xs.isEmpty) throw new UnsupportedOperationException("Empty iterator step.")
    xs = xs.tail
  }
  
  override def dup: Iterator[A] = new ListIterator(xs)
}

private[containers] final class ListBuilder[A] extends ListBuffer[A] with Builder[Any, A] {
  override type State = List[A]
  
  override def expect(count: Int): this.type = this
  
  override def state: List[A] = toList
  
  protected override def stringPrefix: String = "List.Builder"
}
