/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.util._

final class ArrayMap[+A, +T] private[containers] (slots: Array[AnyRef])
  extends Family[ArrayMap[A, T]] with Map[A, T] {
  
  import scala.annotation.unchecked.uncheckedVariance
  
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
  
  override def get(key: A @uncheckedVariance): Option[T] = {
    var i = 0
    val n = slots.length
    while (i < n) {
      if (key == slots(i)) return Some(slots(i + 1).asInstanceOf[T])
      i += 2
    }
    None
  }
  
  /** Returns a copy of this $collection with the given value associated with the given key.
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
  
  /** Returns a copy of this $collection without any value associated with the given key.
    * @group Updating */
  def - (key: A @uncheckedVariance): ArrayMap[A, T] = {
    var i = 0
    val n = slots.length
    while (i < n && key != slots(i)) i += 2
    if (i == n) this
    else if (n == 2) ArrayMap.empty
    else {
      val newSlots = new Array[AnyRef](n - 2)
      java.lang.System.arraycopy(slots, 0, newSlots, 0, i)
      java.lang.System.arraycopy(slots, i + 2, newSlots, i, (n - 2) - i)
      new ArrayMap(newSlots)
    }
  }
  
  private[containers] def isUnary: Boolean = slots.length == 2
  
  private[containers] def unaryKey: A = slots(0).asInstanceOf[A]
  
  private[containers] def unaryValue: T = slots(1).asInstanceOf[T]
  
  private[containers] def keyAt(index: Int): A =
    slots(index >> 1).asInstanceOf[A]
  
  private[containers] def valueAt(index: Int): T =
    slots((index >> 1) + 1).asInstanceOf[T]
  
  override def iterator: Iterator[(A, T)] = new ArrayMapIterator(slots, 0)
  
  protected[containers] override def foreach[U](f: ((A, T)) => U) {
    var i = 0
    val n = slots.length
    while (i < n) {
      f((slots(i).asInstanceOf[A], slots(i + 1).asInstanceOf[T]))
      i += 2
    }
  }
}

object ArrayMap extends MapFactory[ArrayMap] {
  val empty: ArrayMap[Nothing, Nothing] = new ArrayMap(new Array[AnyRef](0))
  
  def apply[A, T](key: A, value: T): ArrayMap[A, T] = {
    val slots = new Array[AnyRef](2)
    slots(0) = key.asInstanceOf[AnyRef]
    slots(1) = value.asInstanceOf[AnyRef]
    new ArrayMap(slots)
  }
  
  def apply[A, T](key0: A, value0: T, key1: A, value1: T): ArrayMap[A, T] = {
    val slots = new Array[AnyRef](4)
    slots(0) = key0.asInstanceOf[AnyRef]
    slots(1) = value0.asInstanceOf[AnyRef]
    slots(2) = key1.asInstanceOf[AnyRef]
    slots(3) = value1.asInstanceOf[AnyRef]
    new ArrayMap(slots)
  }
  
  implicit override def Builder[A, T]: Builder[Any, (A, T)] { type State = ArrayMap[A, T] } = new ArrayMapBuilder
  
  override def toString: String = "ArrayMap"
}

private[containers] final class ArrayMapIterator[+A, +T]
    (slots: Array[AnyRef], private[this] var index: Int)
  extends Iterator[(A, T)] {
  
  override def isEmpty: Boolean = index >= slots.length
  
  override def head: (A, T) = {
    if (!isEmpty) (slots(index).asInstanceOf[A], slots(index + 1).asInstanceOf[T])
    else throw new NoSuchElementException("Head of empty iterator.")
  }
  
  override def step() {
    if (!isEmpty) index += 2
    else throw new UnsupportedOperationException("Empty iterator step.")
  }
  
  override def dup: Iterator[(A, T)] = new ArrayMapIterator(slots, index)
}

private[containers] final class ArrayMapBuilder[A, T] extends Builder[Any, (A, T)] {
  override type State = ArrayMap[A, T]
  
  private[this] var seen: HashMap[A, Int] = HashMap.empty
  
  private[this] var slots: Array[AnyRef] = _
  
  private[this] var aliased: Boolean = true
  
  private[this] var size: Int = 0
  
  private[this] def capacity: Int = slots.length >> 1
  
  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }
  
  private[this] def resize(size: Int) {
    val newSlots = new Array[AnyRef](size << 1)
    if (slots != null) java.lang.System.arraycopy(slots, 0, newSlots, 0, slots.length min newSlots.length)
    slots = newSlots
  }
  
  private[this] def prepare(size: Int) {
    if (aliased || size > capacity) {
      resize(expand(16, size))
      aliased = false
    }
  }
  
  private[this] def setKey(index: Int, key: A): Unit =
    slots(index << 1) = key.asInstanceOf[AnyRef]
  
  private[this] def setValue(index: Int, value: T): Unit =
    slots((index << 1) + 1) = value.asInstanceOf[AnyRef]
  
  override def += (entry: (A, T)): this.type = {
    val (key, value) = entry
    if (seen.contains(key)) setValue(seen(key), value)
    else {
      seen += (key, size)
      prepare(size + 1)
      setKey(size, key)
      setValue(size, value)
      size += 1
    }
    this
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
  
  override def clear() {
    seen = HashMap.empty
    slots = null
    aliased = true
    size = 0
  }
}
