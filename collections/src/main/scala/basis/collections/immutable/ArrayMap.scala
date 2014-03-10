//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import basis.collections.generic._
import basis.util._
import scala.annotation.unchecked._

private[collections] final class ArrayMap[+A, +T] private[collections] (slots: Array[AnyRef])
  extends Equals with Immutable with Family[ArrayMap[_, _]] with Map[A, T] {

  override def isEmpty: Boolean = slots.length == 0

  override def size: Int = slots.length >> 1

  override def contains(key: A @uncheckedVariance): Boolean = {
    var i = 0
    val n = slots.length
    while (i < n) {
      if (key == slots(i)) return true
      i += 2
    }
    false
  }

  override def apply(key: A @uncheckedVariance): T = {
    var i = 0
    val n = slots.length
    while (i < n) {
      if (key == slots(i)) return slots(i + 1).asInstanceOf[T]
      i += 2
    }
    throw new NoSuchElementException(key.toString)
  }

  override def get(key: A @uncheckedVariance): Maybe[T] = {
    var i = 0
    val n = slots.length
    while (i < n) {
      if (key == slots(i)) return Bind(slots(i + 1).asInstanceOf[T])
      i += 2
    }
    Trap
  }

  /** Returns a copy of this $collection that associates the given value with the given key.
    * @group Updating */
  def + [B >: A, U >: T](key: B, value: U): ArrayMap[B, U] = {
    var i = 0
    val n = slots.length
    while (i < n && key != slots(i)) i += 2
    if (i < n) this
    else {
      val newSlots = new Array[AnyRef](n + 2)
      java.lang.System.arraycopy(slots, 0, newSlots, 0, n)
      newSlots(n) = key.asInstanceOf[AnyRef]
      newSlots(n + 1) = value.asInstanceOf[AnyRef]
      new ArrayMap(newSlots)
    }
  }

  /** Returns a copy of this $collection that associates nothing with the given key.
    * @group Updating */
  def - (key: A @uncheckedVariance): ArrayMap[A, T] = {
    var i = 0
    val n = slots.length
    while (i < n && key != slots(i)) i += 2
    if (i == n) this
    else if (n == 2) ArrayMap.empty[A, T]
    else {
      val newSlots = new Array[AnyRef](n - 2)
      java.lang.System.arraycopy(slots, 0, newSlots, 0, i)
      java.lang.System.arraycopy(slots, i + 2, newSlots, i, (n - 2) - i)
      new ArrayMap(newSlots)
    }
  }

  private[collections] def isUnary: Boolean = slots.length == 2

  private[collections] def unaryKey: A = slots(0).asInstanceOf[A]

  private[collections] def unaryValue: T = slots(1).asInstanceOf[T]

  private[collections] def keyAt(index: Int): A = slots(index >> 1).asInstanceOf[A]

  private[collections] def valueAt(index: Int): T = slots((index >> 1) + 1).asInstanceOf[T]

  private[collections] def traverseKeys(f: A => Unit): Unit = {
    var i = 0
    val n = slots.length
    while (i < n) {
      f(slots(i).asInstanceOf[A])
      i += 2
    }
  }

  private[collections] def traverseValues(f: T => Unit): Unit = {
    var i = 1
    val n = slots.length
    while (i < n) {
      f(slots(i).asInstanceOf[T])
      i += 2
    }
  }

  override def traverse(f: (A, T) => Unit): Unit = {
    var i = 0
    val n = slots.length
    while (i < n) {
      f(slots(i).asInstanceOf[A], slots(i + 1).asInstanceOf[T])
      i += 2
    }
  }

  override def traverse(f: ((A, T)) => Unit): Unit = {
    var i = 0
    val n = slots.length
    while (i < n) {
      f((slots(i).asInstanceOf[A], slots(i + 1).asInstanceOf[T]))
      i += 2
    }
  }

  override def iterator: Iterator[(A, T)] = new ArrayMapIterator(slots, 0)

  protected override def stringPrefix: String = "ArrayMap"
}

private[collections] object ArrayMap extends MapFactory[ArrayMap] {
  private[this] val Empty = new ArrayMap[Nothing, Nothing](new Array[AnyRef](0))
  override def empty[A, T]: ArrayMap[A, T] = Empty

  override def from[A, T](elems: Traverser[(A, T)]): ArrayMap[A, T] = {
    if (elems.isInstanceOf[ArrayMap[_, _]]) elems.asInstanceOf[ArrayMap[A, T]]
    else super.from(elems)
  }

  private[collections] def apply[A, T](key: A, value: T): ArrayMap[A, T] = {
    val slots = new Array[AnyRef](2)
    slots(0) = key.asInstanceOf[AnyRef]
    slots(1) = value.asInstanceOf[AnyRef]
    new ArrayMap(slots)
  }

  private[collections] def apply[A, T](key0: A, value0: T, key1: A, value1: T): ArrayMap[A, T] = {
    val slots = new Array[AnyRef](4)
    slots(0) = key0.asInstanceOf[AnyRef]
    slots(1) = value0.asInstanceOf[AnyRef]
    slots(2) = key1.asInstanceOf[AnyRef]
    slots(3) = value1.asInstanceOf[AnyRef]
    new ArrayMap(slots)
  }

  implicit override def Builder[A, T]: Builder[(A, T)] with State[ArrayMap[A, T]] =
    new ArrayMapBuilder[A, T]

  override def toString: String = "ArrayMap"
}

private[collections] final class ArrayMapIterator[+A, +T]
    (slots: Array[AnyRef], private[this] var index: Int)
  extends Iterator[(A, T)] {

  override def isEmpty: Boolean = index >= slots.length

  override def head: (A, T) = {
    if (!isEmpty) (slots(index).asInstanceOf[A], slots(index + 1).asInstanceOf[T])
    else Iterator.empty.head
  }

  override def step(): Unit = {
    if (!isEmpty) index += 2
    else Iterator.empty.step()
  }

  override def dup: Iterator[(A, T)] = new ArrayMapIterator(slots, index)
}

private[collections] final class ArrayMapBuilder[A, T] extends Builder[(A, T)] with State[ArrayMap[A, T]] {
  private[this] var seen: HashMap[A, Int] = HashMap.empty[A, Int]

  private[this] var slots: Array[AnyRef] = _

  private[this] var aliased: Boolean = true

  private[this] var size: Int = 0

  private[this] def capacity: Int = slots.length >> 1

  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }

  private[this] def resize(size: Int): Unit = {
    val newSlots = new Array[AnyRef](size << 1)
    if (slots != null) java.lang.System.arraycopy(slots, 0, newSlots, 0, slots.length min newSlots.length)
    slots = newSlots
  }

  private[this] def prepare(size: Int): Unit = {
    if (aliased || size > capacity) {
      resize(expand(16, size))
      aliased = false
    }
  }

  private[this] def setKey(index: Int, key: A): Unit =
    slots(index << 1) = key.asInstanceOf[AnyRef]

  private[this] def setValue(index: Int, value: T): Unit =
    slots((index << 1) + 1) = value.asInstanceOf[AnyRef]

  override def append(entry: (A, T)): Unit = {
    val (key, value) = entry
    if (seen.contains(key)) setValue(seen(key), value)
    else {
      seen += (key, size)
      prepare(size + 1)
      setKey(size, key)
      setValue(size, value)
      size += 1
    }
  }

  override def expect(count: Int): this.type = {
    if (slots == null || size + count > capacity) {
      resize(size + count)
      aliased = false
    }
    this
  }

  override def state: ArrayMap[A, T] = {
    if (slots == null || size != capacity) resize(size)
    aliased = true
    new ArrayMap(slots)
  }

  override def clear(): Unit = {
    seen = HashMap.empty[A, Int]
    slots = null
    aliased = true
    size = 0
  }

  override def toString: String = "ArrayMap"+"."+"Builder"+"()"
}
