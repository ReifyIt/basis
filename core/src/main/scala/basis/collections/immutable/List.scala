//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import basis._
import basis.collections.generic._
import basis.collections.mutable._

sealed abstract class List[+A]
  extends Equals
  with Immutable
  with Family[List[_]]
  with ListLike[A]
  with LinearSeq[A]
  with Stack[A] {

  override def length: Int = {
    var n = 0
    var xs = this
    while (!xs.isEmpty) {
      n += 1
      xs = xs.tail
    }
    n
  }

  override def tail: List[A]

  def drop(lower: Int): List[A] = {
    var i = 0
    var xs = this
    while (i < lower && !xs.isEmpty) {
      i += 1
      xs = xs.tail
    }
    xs
  }

  def take(upper: Int): List[A] = {
    var i = 0
    val b = new ListBuilder[A]
    var xs = this
    while (i < upper && !xs.isEmpty) {
      i += 1
      b += xs.head
      xs = xs.tail
    }
    b.state
  }

  def slice(lower: Int, upper: Int): List[A] =
    if (lower < upper) drop(lower).take(upper) else Nil

  override def :: [B >: A](elem: B): List[B]

  def reverse: List[A]

  final override def toList: List[A] = this

  protected override def stringPrefix: String = "List"
}

object List extends SeqFactory[List] {
  override def empty[A]: List[A] = Nil

  override def from[A](elems: Traverser[A]): List[A] = {
    if (elems.isInstanceOf[List[_]]) elems.asInstanceOf[List[A]]
    else if (elems.isInstanceOf[ListLike[_]]) elems.asInstanceOf[ListLike[A]].toList
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[List[A]] =
    new ListBuilder[A]

  override def toString: String = "List"
}

sealed abstract class ::[A] extends List[A] {
  final override def isEmpty: Boolean = false

  private[collections] def tail_=(tail: List[A]): Unit
}

object :: {
  def apply[A](x: A, xs: List[A]): ::[A] = {
    if (x.isInstanceOf[Int] && (xs.isInstanceOf[IntList] || xs.isInstanceOf[Nil.type]))
      new IntList(x.asInstanceOf[Int], xs.asInstanceOf[List[Int]]).asInstanceOf[::[A]]
    else if (x.isInstanceOf[Long] && (xs.isInstanceOf[LongList] || xs.isInstanceOf[Nil.type]))
      new LongList(x.asInstanceOf[Long], xs.asInstanceOf[List[Long]]).asInstanceOf[::[A]]
    else if (x.isInstanceOf[Float] && (xs.isInstanceOf[FloatList] || xs.isInstanceOf[Nil.type]))
      new FloatList(x.asInstanceOf[Float], xs.asInstanceOf[List[Float]]).asInstanceOf[::[A]]
    else if (x.isInstanceOf[Double] && (xs.isInstanceOf[DoubleList] || xs.isInstanceOf[Nil.type]))
      new DoubleList(x.asInstanceOf[Double], xs.asInstanceOf[List[Double]]).asInstanceOf[::[A]]
    else new RefList(x, xs)
  }

  def unapply[A](list: ::[A]): Some[(A, List[A])] = Some((list.head, list.tail))
}

private[collections] final class IntList(car: Int, cdr: List[Int]) extends ::[Int] {
  override val head: Int = car

  override var tail: List[Int] = cdr

  override def :: [B >: Int](elem: B): List[B] = {
    if (elem.isInstanceOf[Int]) new IntList(elem.asInstanceOf[Int], this)
    else new RefList(elem, this)
  }

  override def reverse: List[Int] = {
    var sx = Nil: List[Int]
    var xs = this: List[Int]
    while (!xs.isEmpty) {
      sx = new IntList(xs.asInstanceOf[IntList].head, sx)
      xs = xs.tail
    }
    sx
  }

  override def traverse(f: Int => Unit): Unit = {
    var xs = this: List[Int]
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }

  override def iterator: Iterator[Int] = new IntListIterator(this)
}

private[immutable] final class LongList(car: Long, cdr: List[Long]) extends ::[Long] {
  override val head: Long = car

  override var tail: List[Long] = cdr

  override def :: [B >: Long](elem: B): List[B] = {
    if (elem.isInstanceOf[Long]) new LongList(elem.asInstanceOf[Long], this)
    else new RefList(elem, this)
  }

  override def reverse: List[Long] = {
    var sx = Nil: List[Long]
    var xs = this: List[Long]
    while (!xs.isEmpty) {
      sx = new LongList(xs.asInstanceOf[LongList].head, sx)
      xs = xs.tail
    }
    sx
  }

  override def traverse(f: Long => Unit): Unit = {
    var xs = this: List[Long]
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }

  override def iterator: Iterator[Long] = new LongListIterator(this)
}

private[immutable] final class FloatList(car: Float, cdr: List[Float]) extends ::[Float] {
  override val head: Float = car

  override var tail: List[Float] = cdr

  override def :: [B >: Float](elem: B): List[B] = {
    if (elem.isInstanceOf[Float]) new FloatList(elem.asInstanceOf[Float], this)
    else new RefList(elem, this)
  }

  override def reverse: List[Float] = {
    var sx = Nil: List[Float]
    var xs = this: List[Float]
    while (!xs.isEmpty) {
      sx = new FloatList(xs.asInstanceOf[FloatList].head, sx)
      xs = xs.tail
    }
    sx
  }

  override def traverse(f: Float => Unit): Unit = {
    var xs = this: List[Float]
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }

  override def iterator: Iterator[Float] = new FloatListIterator(this)
}

private[immutable] final class DoubleList(car: Double, cdr: List[Double]) extends ::[Double] {
  override val head: Double = car

  override var tail: List[Double] = cdr

  override def :: [B >: Double](elem: B): List[B] = {
    if (elem.isInstanceOf[Double]) new DoubleList(elem.asInstanceOf[Double], this)
    else new RefList(elem, this)
  }

  override def reverse: List[Double] = {
    var sx = Nil: List[Double]
    var xs = this: List[Double]
    while (!xs.isEmpty) {
      sx = new DoubleList(xs.asInstanceOf[DoubleList].head, sx)
      xs = xs.tail
    }
    sx
  }

  override def traverse(f: Double => Unit): Unit = {
    var xs = this: List[Double]
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }

  override def iterator: Iterator[Double] = new DoubleListIterator(this)
}

private[immutable] final class RefList[A](car: A, cdr: List[A]) extends ::[A] {
  override val head: A = car

  override var tail: List[A] = cdr

  override def :: [B >: A](elem: B): List[B] = new RefList(elem, this)

  override def reverse: List[A] = {
    var sx = Nil: List[A]
    var xs = this: List[A]
    while (!xs.isEmpty) {
      sx = new RefList(xs.head, sx)
      xs = xs.tail
    }
    sx
  }

  override def traverse(f: A => Unit): Unit = {
    var xs = this: List[A]
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }

  override def iterator: Iterator[A] = new RefListIterator(this)
}

object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true

  override def length: Int = 0

  override def head: Nothing = throw new NoSuchElementException("Head of empty list")

  override def tail: List[Nothing] = throw new UnsupportedOperationException("Tail of empty list")

  override def :: [B](elem: B): List[B] = {
    if (elem.isInstanceOf[Int]) new IntList(elem.asInstanceOf[Int], this).asInstanceOf[List[B]]
    else if (elem.isInstanceOf[Long]) new LongList(elem.asInstanceOf[Long], this).asInstanceOf[List[B]]
    else if (elem.isInstanceOf[Float]) new FloatList(elem.asInstanceOf[Float], this).asInstanceOf[List[B]]
    else if (elem.isInstanceOf[Double]) new DoubleList(elem.asInstanceOf[Double], this).asInstanceOf[List[B]]
    else new RefList(elem, this)
  }

  override def reverse: List[Nothing] = Nil

  override def iterator: Iterator[Nothing] = Iterator.empty

  override def traverse(f: Nothing => Unit): Unit = ()

  override def toString: String = "Nil"
}

private[immutable] final class IntListIterator(private[this] var xs: List[Int]) extends Iterator[Int] {
  override def isEmpty: Boolean = xs.isEmpty
  override def head: Int = if (!xs.isEmpty) xs.asInstanceOf[IntList].head else Iterator.empty.head
  override def step(): Unit = if (!xs.isEmpty) xs = xs.tail else Iterator.empty.step()
  override def dup: Iterator[Int] = new IntListIterator(xs)
}

private[immutable] final class LongListIterator(private[this] var xs: List[Long]) extends Iterator[Long] {
  override def isEmpty: Boolean = xs.isEmpty
  override def head: Long = if (!xs.isEmpty) xs.asInstanceOf[LongList].head else Iterator.empty.head
  override def step(): Unit = if (!xs.isEmpty) xs = xs.tail else Iterator.empty.step()
  override def dup: Iterator[Long] = new LongListIterator(xs)
}

private[immutable] final class FloatListIterator(private[this] var xs: List[Float]) extends Iterator[Float] {
  override def isEmpty: Boolean = xs.isEmpty
  override def head: Float = if (!xs.isEmpty) xs.asInstanceOf[FloatList].head else Iterator.empty.head
  override def step(): Unit = if (!xs.isEmpty) xs = xs.tail else Iterator.empty.step()
  override def dup: Iterator[Float] = new FloatListIterator(xs)
}

private[immutable] final class DoubleListIterator(private[this] var xs: List[Double]) extends Iterator[Double] {
  override def isEmpty: Boolean = xs.isEmpty
  override def head: Double = if (!xs.isEmpty) xs.asInstanceOf[DoubleList].head else Iterator.empty.head
  override def step(): Unit = if (!xs.isEmpty) xs = xs.tail else Iterator.empty.step()
  override def dup: Iterator[Double] = new DoubleListIterator(xs)
}

private[immutable] final class RefListIterator[+A](private[this] var xs: List[A]) extends Iterator[A] {
  override def isEmpty: Boolean = xs.isEmpty
  override def head: A = if (!xs.isEmpty) xs.head else Iterator.empty.head
  override def step(): Unit = if (!xs.isEmpty) xs = xs.tail else Iterator.empty.step()
  override def dup: Iterator[A] = new RefListIterator(xs)
}

private[collections] final class ListBuilder[A] extends ListBuffer[A] with State[List[A]] {
  override def state: List[A] = toList
  override def toString: String = "List"+"."+"Builder"
}
