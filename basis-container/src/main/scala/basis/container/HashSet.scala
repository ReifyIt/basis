/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

final class HashSet[A] private
    (nodeMap: Int, itemMap: Int, slots: scala.Array[AnyRef])
    (implicit hasher: Hashable[A])
  extends Set[A] {
  
  import hasher._
  
  override type Self = HashSet[A]
  
  override def iterator: Iterator[A] =
    new HashSetIterator[A](this)
  
  override def contains(element: A): Boolean =
    contains(element, hash(element))
  
  override def + (element: A): HashSet[A] =
    update(element, hash(element), 0)
  
  override def - (element: A): HashSet[A] =
    remove(element, hash(element))
  
  /** Returns `true` if this set contains no elements. */
  def isEmpty: Boolean = slots.length == 0
  
  /** Returns `true` if this node represents a trie (not a collision bucket). */
  private[container] def isTrie: Boolean = nodeMap != 0
  
  /** Returns `true` if the nth trie slot contains an item or a subset. */
  private[container] def hasNodeAt(n: Int): Boolean =
    (nodeMap & n) != 0
  
  /** Returns `true` if the nth trie slot contains an item. */
  private[container] def hasItemAt(n: Int): Boolean =
    (itemMap & n) != 0
  
  /** Returns 2 to the power of the low 5 bits of the right-shifted hash.
    * Yields an `Int` with a single set bit. */
  private[container] def trieSlot(h: Int, k: Int): Int =
    1 << ((h >>> k) & 0x1F)
  
  /** Returns 2 to the power of the low 5 bits of the hash.
    * Yields an `Int` with a single set bit. */
  private[container] def trieSlot(h: Int): Int =
    1 << (h & 0x1F)
  
  /** Returns the hamming weight of the low n bits of the node map. This counts
    * the number of set bits in the node map lower than the nth bit. */
  private[container] def slotIndex(n: Int): Int =
    java.lang.Integer.bitCount(nodeMap & (n - 1))
  
  private[container] def slot(i: Int): AnyRef = slots(i)
  
  private[container] def arity: Int = slots.length
  
  private def contains(item: A, h: Int): Boolean = {
    if (isEmpty) false
    else if (isTrie) trieContains(item, h)
    else bucketContains(item, h)
  }
  
  private[this] def trieContains(item: A, h: Int): Boolean = {
    val n = trieSlot(h)
    val i = slotIndex(n)
    val node = slots(i)
    if (hasItemAt(n)) equal(node.asInstanceOf[A], item)
    else if (hasNodeAt(n)) node.asInstanceOf[HashSet[A]].contains(item, h >>> 5)
    else false
  }
  
  private[this] def bucketContains(item: A, h: Int): Boolean = {
    // assume(!isEmpty)
    if (h == hash(slots(0).asInstanceOf[A])) {
      var i = 0
      val n = slots.length
      while (i < n) { if (equal(slots(i).asInstanceOf[A], item)) return true; i += 1 }
    }
    false
  }
  
  private def update(item: A, h: Int, k: Int): HashSet[A] = {
    val n = trieSlot(h, k)
    if (isEmpty) updateEmptySet(item, n)
    else if (isTrie) updateTrie(item, h, k, n)
    else updateBucket(item, h, k)
  }
  
  private[this] def updateEmptySet(item: A, n: Int): HashSet[A] = {
    val newSlots = new scala.Array[AnyRef](1)
    newSlots(0) = item.asInstanceOf[AnyRef]
    new HashSet[A](n, n, newSlots)
  }
  
  private[this] def updateTrie(item: A, h: Int, k: Int, n: Int): HashSet[A] = {
    val i = slotIndex(n)
    if (hasItemAt(n)) updateItem(item, h, k, n, i)
    else if (hasNodeAt(n)) updateNode(item, h, k, i)
    else insertItem(item, n, i)
  }
  
  private[this] def updateItem(item: A, h: Int, k: Int, n: Int, i: Int): HashSet[A] = {
    val node = slots(i).asInstanceOf[A]
    if (equal(node, item)) this
    else {
      val newNode = merge(item, h >>> (k + 5), node, hash(node) >>> (k + 5))
      val newSlots = slots.clone
      newSlots(i) = newNode
      new HashSet(nodeMap, itemMap ^ n, newSlots)
    }
  }
  
  private[this] def updateNode(item: A, h: Int, k: Int, i: Int): HashSet[A] = {
    val node = slots(i).asInstanceOf[HashSet[A]]
    val newNode = node.update(item, h, k + 5)
    if (node eq newNode) this
    else {
      val newSlots = slots.clone
      newSlots(i) = newNode
      new HashSet[A](nodeMap, itemMap, newSlots)
    }
  }
  
  private[this] def insertItem(item: A, n: Int, i: Int): HashSet[A] = {
    val newSlots = new scala.Array[AnyRef](slots.length + 1)
    java.lang.System.arraycopy(slots, 0, newSlots, 0, i)
    newSlots(i) = item.asInstanceOf[AnyRef]
    java.lang.System.arraycopy(slots, i, newSlots, i + 1, slots.length - i)
    new HashSet[A](nodeMap | n, itemMap | n, newSlots)
  }
  
  private[this] def updateBucket(item: A, h: Int, k: Int): HashSet[A] = {
    val bucketHash = hash(slots(0).asInstanceOf[A])
    if (h == bucketHash) {
      val newSlots = new scala.Array[AnyRef](slots.length + 1)
      java.lang.System.arraycopy(slots, 0, newSlots, 0, slots.length)
      newSlots(slots.length) = item.asInstanceOf[AnyRef]
      new HashSet(0, 0, newSlots)
    }
    else resolve(this, bucketHash >>> k, item, h >>> k)
  }
  
  private[this] def merge(itemA: A, hashA: Int, itemB: A, hashB: Int): HashSet[A] = {
    if (hashA == hashB) {
      val newSlots = new scala.Array[AnyRef](2)
      newSlots(0) = itemA.asInstanceOf[AnyRef]
      newSlots(1) = itemB.asInstanceOf[AnyRef]
      new HashSet[A](0, 0, newSlots)
    }
    else {
      val slotA = trieSlot(hashA)
      val slotB = trieSlot(hashB)
      val newMap = slotA | slotB
      if (slotA == slotB) {
        val newNode = merge(itemA, hashA >>> 5, itemB, hashB >>> 5)
        val newSlots = new scala.Array[AnyRef](1)
        newSlots(0) = newNode
        new HashSet[A](newMap, 0, newSlots)
      }
      else {
        val newSlots = new scala.Array[AnyRef](2)
        if (((slotA - 1) & slotB) == 0) {
          newSlots(0) = itemA.asInstanceOf[AnyRef]
          newSlots(1) = itemB.asInstanceOf[AnyRef]
        }
        else {
          newSlots(0) = itemB.asInstanceOf[AnyRef]
          newSlots(1) = itemA.asInstanceOf[AnyRef]
        }
        new HashSet[A](newMap, newMap, newSlots)
      }
    }
  }
  
  private[this] def resolve(bucket: HashSet[A], bucketHash: Int, item: A, itemHash: Int): HashSet[A] = {
    // assume(bucketHash != itemHash)
    val bucketSlot = trieSlot(bucketHash)
    val itemSlot = trieSlot(itemHash)
    if (bucketSlot == itemSlot) {
      val newNode = resolve(bucket, bucketHash >>> 5, item, itemHash >>> 5)
      val newSlots = new scala.Array[AnyRef](1)
      newSlots(0) = newNode
      new HashSet[A](bucketSlot, 0, newSlots)
    }
    else {
      val newSlots = new scala.Array[AnyRef](2)
      if (((bucketSlot - 1) & itemSlot) == 0) {
        newSlots(0) = bucket
        newSlots(1) = item.asInstanceOf[AnyRef]
      }
      else {
        newSlots(0) = item.asInstanceOf[AnyRef]
        newSlots(1) = bucket
      }
      new HashSet[A](bucketSlot | itemSlot, itemSlot, newSlots)
    }
  }
  
  private def remove(item: A, h: Int): HashSet[A] = {
    if (isEmpty) this
    else if (isTrie) removeFromTrie(item, h)
    else removeFromBucket(item, h)
  }
  
  private[this] def removeFromTrie(item: A, h: Int): HashSet[A] = {
    val n = trieSlot(h)
    val i = slotIndex(n)
    if (hasItemAt(n)) removeItem(item, n, i)
    else if (hasNodeAt(n)) removeNode(item, h, n, i)
    else this
  }
  
  private[this] def removeItem(item: A, n: Int, i: Int): HashSet[A] = {
    val node = slots(i)
    if (equal(node.asInstanceOf[A], item)) {
      val newSlots = new scala.Array[AnyRef](slots.length - 1)
      java.lang.System.arraycopy(slots, 0, newSlots, 0, i)
      java.lang.System.arraycopy(slots, i + 1, newSlots, i, newSlots.length - i)
      new HashSet[A](nodeMap ^ n, itemMap ^ n, newSlots)
    }
    else this
  }
  
  private[this] def removeNode(item: A, h: Int, n: Int, i: Int): HashSet[A] = {
    val node = slots(i).asInstanceOf[HashSet[A]]
    val newNode = node.remove(item, h >>> 5)
    if (node eq newNode) this
    else newNode.arity match {
      case 0 =>
        val newSlots = new scala.Array[AnyRef](slots.length - 1)
        java.lang.System.arraycopy(slots, 0, newSlots, 0, i)
        java.lang.System.arraycopy(slots, i + 1, newSlots, i, newSlots.length - i)
        new HashSet[A](nodeMap ^ n, itemMap, newSlots)
      case 1 =>
        val newSlots = slots.clone
        newSlots(i) = newNode.slot(0)
        new HashSet[A](nodeMap, itemMap | n, newSlots)
      case _ =>
        val newSlots = slots.clone
        newSlots(i) = newNode
        new HashSet[A](nodeMap, itemMap, newSlots)
    }
  }
  
  private[this] def removeFromBucket(item: A, h: Int): HashSet[A] = {
    // assume(!isEmpty)
    if (h == hash(slots(0).asInstanceOf[A])) {
      var i = 0
      val n = slots.length
      while (i < n) if (equal(slots(i).asInstanceOf[A], item)) {
        val newSlots = new scala.Array[AnyRef](slots.length - 1)
        java.lang.System.arraycopy(slots, 0, newSlots, 0, i)
        java.lang.System.arraycopy(slots, i + 1, newSlots, i, newSlots.length - i)
        return new HashSet[A](0, 0, newSlots)
      }
    }
    this
  }
}

private[container] final class HashSetIterator[A](self: HashSet[A]) extends Iterator[A] {
  private[this] var child: HashSetIterator[A] = _
  private[this] var index: Int = 0
  
  @tailrec override def hasNext: Boolean = {
    if (child != null) child.asInstanceOf[Iterator[A]].hasNext || { child = null; hasNext }
    else if (self.isTrie && index < 32) self.hasNodeAt(1 << index) || { index += 1; hasNext }
    else !self.isTrie && index < self.arity
  }
  
  @tailrec override def next(): A = {
    if (child != null) {
      if (child.hasNext) child.next()
      else { child = null; next() }
    }
    else if (self.isTrie && index < 32) {
      val n = 1 << index
      index += 1
      if (!self.hasNodeAt(n)) next()
      else {
        val i = self.slotIndex(n)
        if (self.hasItemAt(n)) self.slot(i).asInstanceOf[A]
        else {
          child = new HashSetIterator[A](self.slot(i).asInstanceOf[HashSet[A]])
          child.next()
        }
      }
    }
    else if (!self.isTrie && index < self.arity) {
      val item = self.slot(index).asInstanceOf[A]
      index += 1
      item
    }
    else throw new scala.NoSuchElementException("empty iterator")
  }
}
