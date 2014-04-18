//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package mutable

import basis.collections.generic._
import basis.collections.immutable._

class ListBuffer[A] private (
    private[this] var first: List[A],
    private[this] var last: ::[A],
    private[this] var size: Int,
    private[this] var aliased: Int)
  extends Equals
  with Mutable
  with Family[ListBuffer[_]]
  with ListLike[A]
  with Buffer[A] {

  def this() = this(Nil, null, 0, 0)

  override def isEmpty: Boolean = size == 0

  override def length: Int = size

  override def apply(index: Int): A = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException(index.toString)
    var i = 0
    var xs = first
    while (i < index) {
      xs = xs.tail
      i += 1
    }
    xs.head
  }

  override def update(index: Int, elem: A): Unit = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException(index.toString)
    if (index == 0) first = ::(elem, first.tail)
    else {
      val xi = dealias(index - 1)
      val xn = ::(elem, xi.tail.tail)
      xi.tail = xn
      if (xn.tail.isEmpty) last = xn
    }
  }

  override def append(elem: A): Unit = {
    val xn = ::(elem, Nil)
    if (size == 0) first = xn
    else dealias(size - 1).tail = xn
    last = xn
    size += 1
    aliased += 1
  }

  override def appendAll(elems: Traverser[A]): Unit = {
    if (elems.isInstanceOf[Nil.type]) ()
    else if (elems.isInstanceOf[::[_]]) {
      var xs = elems.asInstanceOf[::[A]]
      if (size == 0) first = xs
      else dealias(size - 1).tail = xs
      size += 1
      while (!xs.tail.isEmpty) {
        xs = xs.tail.asInstanceOf[::[A]]
        size += 1
      }
      last = xs
    }
    else if (elems.isInstanceOf[ListLike[_]]) appendAll(elems.asInstanceOf[ListLike[A]].toList)
    else super.appendAll(elems)
  }

  override def prepend(elem: A): Unit = {
    val x0 = ::(elem, first)
    first = x0
    if (size == 0) last = x0
    size += 1
    aliased += 1
  }

  override def prependAll(elems: Traverser[A]): Unit = {
    if (size == 0) appendAll(elems)
    else {
      val f = new Prepend
      elems traverse f
      f.splice()
    }
  }

  private final class Prepend extends scala.runtime.AbstractFunction1[A, Unit] {
    private[this] var x0: List[A] = _
    private[this] var xi: ::[A] = _
    override def apply(elem: A): Unit = {
      val xn = ::(elem, Nil)
      if (x0 == null) x0 = xn
      if (xi != null) xi.tail = xn
      xi = xn
      size += 1
      aliased += 1
    }
    def splice(): Unit = if (x0 != null) {
      xi.tail = first
      first = x0
      if (xi.tail.isEmpty) last = xi
    }
  }

  override def insert(index: Int, elem: A): Unit = {
    if (index < 0 || index > size) throw new IndexOutOfBoundsException(index.toString)
    if (index == size) append(elem)
    else if (index == 0) prepend(elem)
    else {
      val xi = dealias(index - 1)
      xi.tail = ::(elem, xi.tail)
      size += 1
      aliased += 1
    }
  }

  override def insertAll(index: Int, elems: Traverser[A]): Unit = {
    if (index < 0 || index > size) throw new IndexOutOfBoundsException(index.toString)
    if (index == size) appendAll(elems)
    else if (index == 0) prependAll(elems)
    else elems traverse new Insert(index)
  }

  private final class Insert(index: Int) extends scala.runtime.AbstractFunction1[A, Unit] {
    private[this] var xi = dealias(index - 1)
    override def apply(elem: A): Unit = {
      val xn = ::(elem, xi.tail)
      xi.tail = xn
      xi = xn
      size += 1
      aliased += 1
    }
  }

  override def remove(index: Int): A = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException(index.toString)
    if (index == 0) {
      val x0 = first
      val x = x0.head
      first = x0.tail
      if (x0.tail.isEmpty) last = null
      size -= 1
      if (aliased > 0) aliased -= 1
      x
    }
    else if (index == size - 1) {
      val xi = dealias(index - 1)
      val x = xi.tail.head
      xi.tail = Nil
      last = xi
      size -= 1
      aliased -= 1
      x
    }
    else {
      val xi = dealias(index - 1)
      val x = xi.tail.head
      xi.tail = xi.tail.tail
      size -= 1
      aliased -= 1
      x
    }
  }

  override def remove(index: Int, count: Int): Unit = {
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    if (index + count > size) throw new IndexOutOfBoundsException((index + count).toString)
    if (size == count) clear()
    else if (index == 0) {
      var xs = first
      var i = 0
      while (i < count) {
        xs = xs.tail
        i += 1
      }
      first = xs
    }
    else {
      val xi = dealias(index - 1)
      var xn = xi: List[A]
      var i = 0
      while (i < count) {
        xn = xn.tail
        i += 1
      }
      xi.tail = xn.tail
      if (xi.tail.isEmpty) last = xi
    }
    size -= count
    if (aliased > count) aliased -= count else aliased = 0
  }

  override def clear(): Unit = {
    first = Nil
    last = null
    size = 0
    aliased = 0
  }

  override def copy: ListBuffer[A] = {
    aliased = 0
    new ListBuffer(first, last, size, aliased)
  }

  override def toList: List[A] = {
    aliased = 0
    first
  }

  override def expect(count: Int): this.type = this

  override def traverse(f: A => Unit): Unit = {
    var xs = first
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }

  override def iterator: Iterator[A] = new ListBufferIterator(first)

  protected override def stringPrefix: String = "ListBuffer"

  /** Returns the list cell at index `n` after privately copying all aliased
    * list cells with indexes less than or equal to `n`. */
  private[this] def dealias(n: Int): ::[A] = {
    var i = 0
    var xi = null: ::[A]
    var xs = first
    if (aliased <= n) {
      while (i < aliased) {
        xi = xs.asInstanceOf[::[A]]
        xs = xs.tail
        i += 1
      }
      while (i <= n) {
        val xn = ::(xs.head, xs.tail)
        if (i == 0) first = xn
        else xi.tail = xn
        xi = xn
        xs = xs.tail
        i += 1
      }
      if (i == size) last = xi
      aliased = i
    }
    else if (n == 0) xi = first.asInstanceOf[::[A]]
    else if (n == size - 1) xi = last
    else while (i <= n) {
      xi = xs.asInstanceOf[::[A]]
      xs = xs.tail
      i += 1
    }
    xi
  }
}

/** A factory for [[ListBuffer list buffers]].
  * @group Containers */
object ListBuffer extends SeqFactory[ListBuffer] {
  override def empty[A]: ListBuffer[A] = new ListBuffer

  override def from[A](elems: Traverser[A]): ListBuffer[A] = {
    if (elems.isInstanceOf[ListBuffer[_]]) elems.asInstanceOf[ListBuffer[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[ListBuffer[A]] =
    new ListBufferBuilder[A]

  override def toString: String = "ListBuffer"
}

private[collections] final class ListBufferIterator[+A] private (
    private[this] var empty: Boolean,
    private[this] var elem: A,
    private[this] var next: List[A])
  extends Iterator[A] {

  def this(list: List[A]) =
    this(list.isEmpty,
         if (!list.isEmpty) list.head else null.asInstanceOf[A],
         if (!list.isEmpty) list.tail else null)

  override def isEmpty: Boolean = empty

  override def head: A = {
    if (empty) Iterator.empty.head
    elem
  }

  override def step(): Unit = {
    if (empty) Iterator.empty.step()
    if (!next.isEmpty) {
      elem = next.head
      next = next.tail
    }
    else {
      empty = true
      elem = null.asInstanceOf[A]
      next = null
    }
  }

  override def dup: Iterator[A] = new ListBufferIterator(empty, head, next)
}

private[mutable] final class ListBufferBuilder[A] extends ListBuffer[A] with State[ListBuffer[A]] {
  override def state: ListBuffer[A] = copy
  override def toString: String = "ListBuffer"+"."+"Builder"
}
