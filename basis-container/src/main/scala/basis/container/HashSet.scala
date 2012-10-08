/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._

final class HashSet[A] private (nodeMap: Int, itemMap: Int, slots: scala.Array[AnyRef]) extends Set[A] {
  override type Self = HashSet[A]
  
  override def iterator: Iterator[A] =
    new HashSetIterator[A](this)
  
  override def contains(element: A): Boolean =
    contains(element, element.hashCode)
  
  override def + (element: A): HashSet[A] =
    update(element, element.hashCode, 0)
  
  override def - (element: A): HashSet[A] =
    remove(element, element.hashCode)
  
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
  private[container] def trieSlot(hash: Int, shift: Int): Int =
    1 << ((hash >>> shift) & 0x1F)
  
  /** Returns 2 to the power of the low 5 bits of the hash.
    * Yields an `Int` with a single set bit. */
  private[container] def trieSlot(hash: Int): Int =
    1 << (hash & 0x1F)
  
  /** Returns the hamming weight of the low n bits of the node map. This counts
    * the number of set bits in the node map lower than the nth bit. */
  private[container] def slotIndex(n: Int): Int =
    java.lang.Integer.bitCount(nodeMap & (n - 1))
  
  private[container] def slot(i: Int): AnyRef = slots(i)
  
  private[container] def arity: Int = slots.length
  
  private def contains(item: A, hash: Int): Boolean = {
    if (isTrie) trieContains(item, hash)
    else bucketContains(item)
  }
  
  private[this] def trieContains(item: A, hash: Int): Boolean = {
    val n = trieSlot(hash)
    val i = slotIndex(n)
    if (hasItemAt(n)) slots(i).equals(item)
    else if (hasNodeAt(n)) slots(i).asInstanceOf[HashSet[A]].contains(item, hash >>> 5)
    else false
  }
  
  private[this] def bucketContains(item: A): Boolean = {
    var i = 0
    val n = slots.length
    while (i < n) { if (slots(i).equals(item)) return true; i += 1 }
    false
  }
  
  private def update(item: A, hash: Int, shift: Int): HashSet[A] = {
    val n = trieSlot(hash, shift)
    if (isEmpty) updateEmptySet(item, n)
    else if (isTrie) updateTrie(item, hash, shift, n)
    else updateBucket(item, hash, shift)
  }
  
  private[this] def updateEmptySet(item: A, n: Int): HashSet[A] = {
    val newSlots = new scala.Array[AnyRef](1)
    newSlots(0) = item.asInstanceOf[AnyRef]
    new HashSet[A](n, n, newSlots)
  }
  
  private[this] def updateTrie(item: A, hash: Int, shift: Int, n: Int): HashSet[A] = {
    val i = slotIndex(n)
    if (hasItemAt(n)) updateItem(item, hash, shift, n, i)
    else if (hasNodeAt(n)) updateNode(item, hash, shift, i)
    else insertItem(item, n, i)
  }
  
  private[this] def updateItem(item: A, hash: Int, shift: Int, n: Int, i: Int): HashSet[A] = {
    val node = slots(i)
    if (node.equals(item)) this
    else {
      val newNode = resolve(item.asInstanceOf[AnyRef], hash >>> (shift + 5), node, node.hashCode >>> (shift + 5))
      val newSlots = slots.clone
      newSlots(i) = newNode
      new HashSet[A](nodeMap, itemMap ^ n, newSlots)
    }
  }
  
  private[this] def updateNode(item: A, hash: Int, shift: Int, i: Int): HashSet[A] = {
    val node = slots(i).asInstanceOf[HashSet[A]]
    val newNode = node.update(item, hash, shift + 5)
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
  
  private[this] def updateBucket(item: A, hash: Int, shift: Int): HashSet[A] = {
    val node = slots(0)
    val nodeHash = node.hashCode
    if (hash != nodeHash) resolve(item.asInstanceOf[AnyRef], hash >>> shift, this, nodeHash >>> shift)
    else {
      val newSlots = new scala.Array[AnyRef](slots.length + 1)
      java.lang.System.arraycopy(slots, 0, newSlots, 0, slots.length)
      newSlots(slots.length) = item.asInstanceOf[AnyRef]
      new HashSet(0, 0, newSlots)
    }
  }
  
  private[this] def resolve(itemA: AnyRef, hashA: Int, itemB: AnyRef, hashB: Int): HashSet[A] = {
    if (hashA == hashB) {
      val newSlots = new scala.Array[AnyRef](2)
      newSlots(0) = itemA
      newSlots(1) = itemB
      new HashSet[A](0, 0, newSlots)
    }
    else {
      val bitA = 1 << (hashA & 0x1F)
      val bitB = 1 << (hashB & 0x1F)
      val nodeMap = bitA | bitB
      if (bitA == bitB) {
        val newNode = resolve(itemA, hashA >>> 5, itemB, hashB >>> 5)
        val newSlots = new scala.Array[AnyRef](1)
        newSlots(0) = newNode
        new HashSet[A](nodeMap, nodeMap, newSlots)
      }
      else {
        val newSlots = new scala.Array[AnyRef](2)
        if (((bitA - 1) & bitB) == 0) {
          newSlots(0) = itemA
          newSlots(1) = itemB
        }
        else {
          newSlots(0) = itemB
          newSlots(1) = itemA
        }
        new HashSet[A](nodeMap, nodeMap, newSlots)
      }
    }
  }
  
  private def remove(item: A, hash: Int): HashSet[A] = {
    if (isEmpty) this
    else if (isTrie) removeFromTrie(item, hash)
    else removeFromBucket(item, hash)
  }
  
  private[this] def removeFromTrie(item: A, hash: Int): HashSet[A] = {
    val n = trieSlot(hash)
    val i = slotIndex(n)
    if (hasItemAt(n)) removeItem(item, n, i)
    else if (hasNodeAt(n)) removeNode(item, hash, n, i)
    else this
  }
  
  private[this] def removeItem(item: A, n: Int, i: Int): HashSet[A] = {
    val node = slots(i)
    if (node.equals(item)) {
      val newSlots = new scala.Array[AnyRef](slots.length - 1)
      java.lang.System.arraycopy(slots, 0, newSlots, 0, i)
      java.lang.System.arraycopy(slots, i + 1, newSlots, i, newSlots.length - i)
      new HashSet[A](nodeMap ^ n, itemMap ^ n, newSlots)
    }
    else this
  }
  
  private[this] def removeNode(item: A, hash: Int, n: Int, i: Int): HashSet[A] = {
    val node = slots(i).asInstanceOf[HashSet[A]]
    val newNode = node.remove(item, hash >>> 5)
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
  
  private[this] def removeFromBucket(item: A, hash: Int): HashSet[A] = {
    // assume(!isEmpty)
    if (slots(0).hashCode == hash) {
      var i = 0
      val n = slots.length
      while (i < n) if (slots(i).equals(item)) {
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
