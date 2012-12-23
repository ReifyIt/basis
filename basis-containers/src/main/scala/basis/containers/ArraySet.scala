/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.runtime._
import basis.util._

import scala.annotation.unchecked.uncheckedVariance

final class ArraySet[+A] private[containers] (slots: Array[AnyRef])
  extends Immutable with Family[ArraySet[A]] with Set[A] {
  
  override def isEmpty: Boolean = slots.length == 0
  
  override def size: Int = slots.length
  
  override def contains(elem: A @uncheckedVariance): Boolean = {
    var i = 0
    val n = slots.length
    while (i < n) {
      if (elem == slots(i)) return true
      i += 1
    }
    false
  }
  
  /** Returns a copy of this $collection that contains the given element.
    * @group Updating */
  def + [B >: A](elem: B): ArraySet[B] = {
    var i = 0
    val n = slots.length
    while (i < n && elem != slots(i)) i += 1
    if (i < n) this
    else {
      val newSlots = new Array[AnyRef](n + 1)
      java.lang.System.arraycopy(slots, 0, newSlots, 0, n)
      newSlots(n) = elem.asInstanceOf[AnyRef]
      new ArraySet(newSlots)
    }
  }
  
  /** Returns a copy of this $collection that excludes the given element.
    * @group Updating */
  def - (elem: A @uncheckedVariance): ArraySet[A] = {
    var i = 0
    val n = slots.length
    while (i < n && elem != slots(i)) i += 1
    if (i == n) this
    else if (n == 1) ArraySet.empty[A]
    else {
      val newSlots = new Array[AnyRef](n - 1)
      java.lang.System.arraycopy(slots, 0, newSlots, 0, i)
      java.lang.System.arraycopy(slots, i + 1, newSlots, i, (n - 1) - i)
      new ArraySet(newSlots)
    }
  }
  
  private[containers] def isUnary: Boolean = slots.length == 1
  
  private[containers] def unaryElement: A = slots(0).asInstanceOf[A]
  
  private[containers] def elementAt(index: Int): A = slots(index).asInstanceOf[A]
  
  override def iterator: Iterator[A] = new ArraySetIterator(slots, 0)
  
  protected[containers] override def foreach[U](f: A => U) {
    var i = 0
    val n = slots.length
    while (i < n) {
      f(slots(i).asInstanceOf[A])
      i += 1
    }
  }
}

object ArraySet extends SetFactory[ArraySet] {
  implicit override def Builder[A : TypeHint]
    : Builder[Any, A] { type State = ArraySet[A] } =
    new ArraySetBuilder
  
  private[this] val empty = new ArraySet[Nothing](new Array[AnyRef](0))
  override def empty[A : TypeHint]: ArraySet[A] = empty
  
  private[containers] def apply[A](elem: A): ArraySet[A] = {
    val slots = new Array[AnyRef](1)
    slots(0) = elem.asInstanceOf[AnyRef]
    new ArraySet(slots)
  }
  
  private[containers] def apply[A](elem0: A, elem1: A): ArraySet[A] = {
    if (elem0 == elem1) apply(elem0)
    else {
      val slots = new Array[AnyRef](2)
      slots(0) = elem0.asInstanceOf[AnyRef]
      slots(1) = elem1.asInstanceOf[AnyRef]
      new ArraySet(slots)
    }
  }
  
  override def toString: String = "ArraySet"
}

private[containers] final class ArraySetIterator[+A]
    (slots: Array[AnyRef], private[this] var index: Int)
  extends Iterator[A] {
  
  override def isEmpty: Boolean = index >= slots.length
  
  override def head: A = {
    if (!isEmpty) slots(index).asInstanceOf[A]
    else throw new NoSuchElementException("Head of empty iterator.")
  }
  
  override def step() {
    if (!isEmpty) index += 1
    else throw new UnsupportedOperationException("Empty iterator step.")
  }
  
  override def dup: Iterator[A] = new ArraySetIterator(slots, index)
}

private[containers] final class ArraySetBuilder[A] extends Builder[Any, A] {
  override type State = ArraySet[A]
  
  private[this] var seen: HashSet[A] = HashSet.empty[A]
  
  private[this] var slots: Array[AnyRef] = _
  
  private[this] var aliased: Boolean = true
  
  private[this] var size: Int = 0
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def resize(size: Int) {
    val newSlots = new Array[AnyRef](size)
    if (slots != null) java.lang.System.arraycopy(slots, 0, newSlots, 0, slots.length min size)
    slots = newSlots
  }
  
  private[this] def prepare(size: Int) {
    if (aliased || size > slots.length) {
      resize(expand(16, size))
      aliased = false
    }
  }
  
  override def append(elem: A) {
    if (!seen.contains(elem)) {
      seen += elem
      prepare(size + 1)
      slots(size) = elem.asInstanceOf[AnyRef]
      size += 1
    }
  }
  
  override def expect(count: Int): this.type = {
    if (slots == null || size + count > slots.length) {
      resize(size + count)
      aliased = false
    }
    this
  }
  
  override def state: ArraySet[A] = {
    if (slots == null || size != slots.length) resize(size)
    aliased = true
    new ArraySet(slots)
  }
  
  override def clear() {
    seen = HashSet.empty[A]
    slots = null
    aliased = true
    size = 0
  }
}
