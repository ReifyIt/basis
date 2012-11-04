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
import basis.util._

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance

final class HashMap[+A, +T] private
    (slotMap: Int, entryMap: Int, slots: Array[AnyRef], links: Array[AnyRef])
  extends Family[HashMap[A, T]] with Map[A, T] {
  
  override def isEmpty: Boolean = slots.length == 0
  
  override def size: Int = {
    if (!isTrie || slotMap == entryMap) rank
    else {
      var t = 0
      var i = 0
      while (i < 32 && hasSlotAbove(i)) {
        val n = 1 << i
        if (hasEntryAt(n)) t += 1
        else if (hasSlotAt(n)) t += nodeAt(slot(n)).size
        i += 1
      }
      t
    }
  }
  
  override def contains(key: A @uncheckedVariance): Boolean = contains(key, key.##, 0)
  
  override def apply(key: A @uncheckedVariance): T = apply(key, key.##, 0)
  
  override def get(key: A @uncheckedVariance): Option[T] = get(key, key.##, 0)
  
  /** Returns a copy of this $collection with the given value associated with the given key. */
  def + [B >: A, U >: T](key: B, value: U): HashMap[B, U] = update(key, key.##, value, 0)
  
  /** Returns a copy of this $collection without any value associated with the given key. */
  def - (key: A @uncheckedVariance): HashMap[A, T] = remove(key, key.##, 0)
  
  override def iterator: Iterator[(A, T)] = new HashMap.Cursor(this)
  
  protected override def foreach[@specialized(Unit) U](f: ((A, T)) => U) {
    var i = 0
    if (isTrie) {
      while (i < 32 && hasSlotAbove(i)) {
        val n = 1 << i
        if (hasEntryAt(n)) f((keyAt(slot(n)), valAt(link(n))))
        else if (hasSlotAt(n)) nodeAt(slot(n)) foreach f
        i += 1
      }
    }
    else {
      var n = rank
      while (i < n) {
        f((keyAt(i), valAt(i)))
        i += 1
      }
    }
  }
  
  /** Returns `true` for a trie and 'false' for a collision bucket. */
  private[basis] def isTrie: Boolean = slotMap != 0
  
  /** Returns `true` if this trie defines slots for higher branches.
    * 
    * @param  j   the branch index to look above.
    */
  private[basis] def hasSlotAbove(j: Int): Boolean = (slotMap >>> j) != 0
  
  /** Returns `true` if the nth trie branch contains an entry or a submap.
    * 
    * @param  n   the single set bit identifying the branch to test.
    */
  private[basis] def hasSlotAt(n: Int): Boolean = (slotMap & n) != 0
  
  /** Returns `true` if the nth trie branch contains an entry.
    * 
    * @param  n   the single set bit identifying the branch to test.
    */
  private[basis] def hasEntryAt(n: Int): Boolean = (entryMap & n) != 0
  
  /** Returns the trie branch for the low 5 shifted hash bits.
    * 
    * @param  h   the un-shifted hash code to branch on.
    * @param  k   the amount to right shift the hash code before branching.
    * @return a single set bit identifying the next branch.
    */
  private[basis] def branch(h: Int, k: Int): Int = 1 << ((h >>> k) & 0x1F)
  
  /** Returns the slot index of a non-empty trie branch.
    * 
    * @param  n   the single set bit identifying a non-empty trie branch.
    */
  private[basis] def slot(n: Int): Int = (slotMap & (n - 1)).countSetBits
  
  /** Returns the link index of an entry-holding trie branch.
    * 
    * @param  n   the single set bit identifying an entry-holding trie branch.
    */
  private[basis] def link(n: Int): Int = (entryMap & (n - 1)).countSetBits
  
  /** Returns the submap at a slot index.
    * 
    * @param  i   the index of a slot known to hold a submap.
    */
  private[basis] def nodeAt(i: Int): HashMap[A, T] = slots(i).asInstanceOf[HashMap[A, T]]
  
  /** Returns the key at a slot index.
    * 
    * @param  i   the index of a slot known to hold an entry.
    */
  private[basis] def keyAt(i: Int): A = slots(i).asInstanceOf[A]
  
  /** Returns the value at a link index.
    * 
    * @param  i   the index of a link known to hold a value.
    */
  private[basis] def valAt(i: Int): T = links(i).asInstanceOf[T]
  
  /** Returns the number of filled slots in this node. */
  private[basis] def rank: Int = slots.length
  
  /** Returns `true` if this node directly contains exactly one entry. */
  private[basis] def isUnary: Boolean =
    rank == 1 && (slotMap == entryMap || slotMap == 0)
  
  /** Returns the hash code of the keys in this known collision bucket. */
  private[basis] def bucketHash: Int = entryMap // assume(!isTrie)
  
  /** Returns `true` if this node has a value associated with the given key.
    * 
    * @param  key   the key to lookup.
    * @param  h     the key's hash code.
    * @param  k     the shift-level of this node.
    */
  @tailrec private def contains(key: A @uncheckedVariance, h: Int, k: Int): Boolean = {
    if (isTrie) { // search trie
      val n = branch(h, k)
      if (hasEntryAt(n)) key == keyAt(slot(n))
      else if (hasSlotAt(n)) nodeAt(slot(n)).contains(key, h, k + 5)
      else false
    }
    else { // search collision bucket
      if (h == bucketHash) {
        var i = 0
        val n = rank
        while (i < n) {
          if (key == keyAt(i)) return true
          i += 1
        }
      }
      false
    }
  }
  
  /** Returns the value associated with the given key.
    * 
    * @param  key   the key to lookup.
    * @param  h     the key's hash code.
    * @param  k     the shift-level of this node.
    */
  @tailrec private def apply(key: A @uncheckedVariance, h: Int, k: Int): T = {
    if (isTrie) { // search trie
      val n = branch(h, k)
      if (hasEntryAt(n) && key == keyAt(slot(n))) valAt(link(n))
      else if (hasSlotAt(n)) nodeAt(slot(n)).apply(key, h, k + 5)
      else throw new scala.NoSuchElementException(key.toString)
    }
    else { // search collision bucket
      if (h == bucketHash) {
        var i = 0
        val n = rank
        while (i < n) {
          if (key == keyAt(i)) return valAt(i)
          i += 1
        }
      }
      throw new scala.NoSuchElementException(key.toString)
    }
  }
  
  /** Returns some value associated with the given key, or none if no association exists.
    * 
    * @param  key   the key to lookup.
    * @param  h     the key's hash code.
    * @param  k     the shift-level of this node.
    */
  @tailrec private def get(key: A @uncheckedVariance, h: Int, k: Int): Option[T] = {
    if (isTrie) { // search trie
      val n = branch(h, k)
      if (hasEntryAt(n) && key == keyAt(slot(n))) Some(valAt(link(n)))
      else if (hasSlotAt(n)) nodeAt(slot(n)).get(key, h, k + 5)
      else None
    }
    else { // search collision bucket
      if (h == bucketHash) {
        var i = 0
        val n = rank
        while (i < n) {
          if (key == keyAt(i)) return Some(valAt(i))
          i += 1
        }
      }
      None
    }
  }
  
  /** Returns a copy of this map the given value associated with the given
    * key, or this map itself if it already contains the key, value pair.
    * 
    * @param  key     the key to lookup.
    * @param  h       the key's hash code.
    * @param  value   the value to associate the the key.
    * @param  k       the shift-level of this node.
    */
  private def update[B >: A, U >: T](key: B, h: Int, value: U, k: Int): HashMap[B, U] = {
    val n = branch(h, k)
    if (isEmpty) // new unary map
      new HashMap(n, n, newSlots(key), newLinks(value))
    else if (isTrie) { // update trie
      val i = slot(n)
      val j = link(n)
      if (hasEntryAt(n)) { // update entry
        val e = keyAt(i)
        if (key == e) // replace value
          new HashMap(slotMap, entryMap, slots, updateLink(link(n), value))
        else // merge keys in new submap
          new HashMap(slotMap, entryMap ^ n,
            updateSlot(i, merge(key, h, value, e, e.##, valAt(j), k + 5)),
            removeLink(j))
      }
      else if (hasSlotAt(n)) { // update submap
        val node = nodeAt(i)
        val newNode = node.update(key, h, value, k + 5)
        if (node eq newNode) this
        else new HashMap(slotMap, entryMap, updateSlot(i, newNode), links)
      }
      else // insert entry
        new HashMap(slotMap | n, entryMap | n, insertSlot(i, key), insertLink(j, value))
    }
    else { // update collision bucket
      if (h == bucketHash) {
        var i = 0
        var n = rank
        while (i < n) {
          if (key == keyAt(i))
            return new HashMap(0, bucketHash, slots, updateLink(i, value))
          i += 1
        }
        new HashMap(0, bucketHash, appendSlot(key), appendLink(value))
      }
      else // reconcile key with collision bucket
        resolve(key, h, value, k)
    }
  }
  
  /** Returns a map containing the two key, value pairs. */
  private[this] def merge[B >: A, U >: T](
      keyA: B, hashA: Int, valueA: U,
      keyB: B, hashB: Int, valueB: U,
      k: Int)
    : HashMap[B, U] = {
    if (hashA == hashB) // new collision bucket
      new HashMap(0, hashA, newSlots(keyA, keyB), newLinks(valueA, valueB))
    else {
      val branchA = branch(hashA, k)
      val branchB = branch(hashB, k)
      if (branchA == branchB) // new submap
        new HashMap(branchA | branchB, 0,
          newSlots(merge(keyA, hashA, valueA, keyB, hashB, valueB, k + 5)),
          new Array[AnyRef](0))
      else // new branch
        new HashMap(branchA | branchB, branchA | branchB,
          if (((branchA - 1) & branchB) == 0) newSlots(keyA, keyB) else newSlots(keyB, keyA),
          if (((branchA - 1) & branchB) == 0) newLinks(valueA, valueB) else newLinks(valueB, valueA))
    }
  }
  
  /** Returns a set containing this hash bucket and the key, value pair. */
  private[this] def resolve[B >: A, U >: T](key: B, h: Int, value: U, k: Int): HashMap[B, U] = {
    // assume(h != bucketHash)
    val bucketBranch = branch(bucketHash, k)
    val entryBranch = branch(h, k)
    if (bucketBranch == entryBranch) // new submap
      new HashMap(bucketBranch, 0,
        newSlots(resolve(key, h, value, k + 5)),
        new Array[AnyRef](0))
    else // new branch
      new HashMap(bucketBranch | entryBranch, entryBranch,
        if (((bucketBranch - 1) & entryBranch) == 0) newSlots(this, key) else newSlots(key, this),
        newLinks(value))
  }
  
  /** Returns a copy of this map without a value associated with the given key,
    * or this map itself if it doesn't contain the key. */
  private def remove(key: A @uncheckedVariance, h: Int, k: Int): HashMap[A, T] = {
    if (isEmpty) this
    else if (isTrie) { // remove from trie
      val n = branch(h, k)
      val i = slot(n)
      val j = link(n)
      if (hasEntryAt(n)) { // remove entry
        if (key == keyAt(i))
          new HashMap(slotMap ^ n, entryMap ^ n, removeSlot(i), removeLink(j))
        else this
      }
      else if (hasSlotAt(n)) { // remove from submap
        val node = nodeAt(i)
        val newNode = node.remove(key, h, k + 5)
        if (node eq newNode) this
        else if (newNode.isEmpty) // remove empty submaps
          new HashMap(slotMap ^ n, entryMap, removeSlot(i), links)
        else if (newNode.isUnary) // lift unary submaps
          new HashMap(slotMap, entryMap | n, updateSlot(i, newNode.keyAt(0)), updateLink(j, newNode.valAt(0)))
        else new HashMap(slotMap, entryMap, updateSlot(i, newNode), links)
      }
      else this
    }
    else { // remove from collision bucket
      if (h == bucketHash) {
        var i = 0
        val n = rank
        while (i < n) {
          if (key == keyAt(i))
            return new HashMap(0, bucketHash, removeSlot(i), removeLink(i))
          i += 1
        }
      }
      this
    }
  }
  
  private[this] def newSlots(elem: Any): Array[AnyRef] =
    Array(elem.asInstanceOf[AnyRef])
  
  private[this] def newSlots(elemA: Any, elemB: Any): Array[AnyRef] =
    Array(elemA.asInstanceOf[AnyRef], elemB.asInstanceOf[AnyRef])
  
  private[this] def updateSlot(index: Int, elem: Any): Array[AnyRef] = {
    val newSlots = slots.clone
    newSlots(index) = elem.asInstanceOf[AnyRef]
    newSlots
  }
  
  private[this] def appendSlot(elem: Any): Array[AnyRef] = {
    val newSlots = new Array[AnyRef](slots.length + 1)
    java.lang.System.arraycopy(slots, 0, newSlots, 0, slots.length)
    newSlots(newSlots.length) = elem.asInstanceOf[AnyRef]
    newSlots
  }
  
  private[this] def insertSlot(index: Int, elem: Any): Array[AnyRef] = {
    val newSlots = new Array[AnyRef](slots.length + 1)
    java.lang.System.arraycopy(slots, 0, newSlots, 0, index)
    newSlots(index) = elem.asInstanceOf[AnyRef]
    java.lang.System.arraycopy(slots, index, newSlots, index + 1, slots.length - index)
    newSlots
  }
  
  private[this] def removeSlot(index: Int): Array[AnyRef] = {
    val newSlots = new Array[AnyRef](slots.length - 1)
    java.lang.System.arraycopy(slots, 0, newSlots, 0, index)
    java.lang.System.arraycopy(slots, index + 1, newSlots, index, newSlots.length - index)
    newSlots
  }
  
  private[this] def newLinks[U >: T](value: U): Array[AnyRef] =
    Array(value.asInstanceOf[AnyRef])
  
  private[this] def newLinks[U >: T](valueA: U, valueB: U): Array[AnyRef] =
    Array(valueA.asInstanceOf[AnyRef], valueB.asInstanceOf[AnyRef])
  
  private[this] def updateLink[U >: T](index: Int, value: U): Array[AnyRef] = {
    val newLinks = links.clone
    newLinks(index) = value.asInstanceOf[AnyRef]
    newLinks
  }
  
  private[this] def appendLink[U >: T](value: U): Array[AnyRef] = {
    val newLinks = new Array[AnyRef](links.length + 1)
    java.lang.System.arraycopy(links, 0, newLinks, 0, links.length)
    newLinks(newLinks.length) = value.asInstanceOf[AnyRef]
    newLinks
  }
  
  private[this] def insertLink[U >: T](index: Int, value: U): Array[AnyRef] = {
    val newLinks = new Array[AnyRef](links.length + 1)
    java.lang.System.arraycopy(links, 0, newLinks, 0, index)
    newLinks(index) = value.asInstanceOf[AnyRef]
    java.lang.System.arraycopy(links, index, newLinks, index + 1, links.length - index)
    newLinks
  }
  
  private[this] def removeLink(index: Int): Array[AnyRef] = {
    val newLinks = new Array[AnyRef](links.length - 1)
    java.lang.System.arraycopy(links, 0, newLinks, 0, index)
    java.lang.System.arraycopy(links, index + 1, newLinks, index, newLinks.length - index)
    newLinks
  }
  
  override def toString: String = {
    val s = new java.lang.StringBuilder("HashMap")
    s.append('(')
    if (!isEmpty) {
      val iter = iterator
      val entry = iter.head
      s.append(entry._1).append(" -> ").append(entry._2)
      iter.step()
      while (!iter.isEmpty) {
        val entry = iter.head
        s.append(", ").append(entry._1).append(" -> ").append(entry._2)
        iter.step()
      }
    }
    s.append(')')
    s.toString
  }
}

object HashMap extends MapFactory[HashMap] {
  val Empty: HashMap[Nothing, Nothing] =
    new HashMap[Nothing, Nothing](0, 0, new Array[AnyRef](0), new Array[AnyRef](0))
  
  implicit override def Builder[A, T]: Builder[Any, (A, T), HashMap[A, T]] =
    new HashMapBuilder[A, T]
  
  override def toString: String = "HashMap"
  
  private[immutable] final class Cursor[+A, +T](
      self: HashMap[A, T],
      private[this] var child: Cursor[A, T],
      private[this] var index: Int)
    extends Iterator[(A, T)] {
    
    def this(self: HashMap[A, T]) = this(self, null, 0)
    
    @tailrec override def isEmpty: Boolean = {
      if (child != null)
        child.asInstanceOf[Iterator[_]].isEmpty && { child = null; isEmpty }
      else if (self.isTrie)
        index >= 32 || !self.hasSlotAbove(index) ||
          (!self.hasSlotAt(1 << index) && { index += 1; isEmpty })
      else index >= self.rank
    }
    
    @tailrec override def head: (A, T) = {
      if (child != null) {
        if (!child.isEmpty) child.head
        else {
          child = null
          head
        }
      }
      else if (self.isTrie) {
        if (index < 32 && self.hasSlotAbove(index)) {
          val n = 1 << index
          if (self.hasEntryAt(n)) (self.keyAt(self.slot(n)), self.valAt(self.link(n)))
          else if (self.hasSlotAt(n)) {
            child = new Cursor(self.nodeAt(self.slot(n)))
            index += 1
            head
          }
          else {
            index += 1
            head
          }
        }
        else Done.head
      }
      else if (index < self.rank) (self.keyAt(index), self.valAt(index))
      else Done.head
    }
    
    @tailrec override def step() {
      if (child != null) {
        if (!child.isEmpty) child.step()
        else {
          child = null
          step()
        }
      }
      else if (self.isTrie) {
        if (index < 32 && self.hasSlotAbove(index)) {
          val n = 1 << index
          if (self.hasEntryAt(n)) index += 1
          else if (self.hasSlotAt(n)) {
            child = new Cursor(self.nodeAt(self.slot(n)))
            index += 1
            step()
          }
          else {
            index += 1
            step()
          }
        }
        else Done.step()
      }
      else if (index < self.rank) index += 1
      else Done.step()
    }
    
    override def dup: Cursor[A, T] = new Cursor(self, child, index)
  }
}

private[containers] final class HashMapBuilder[A, T] extends Builder[Any, (A, T), HashMap[A, T]] {
  private[this] var map: HashMap[A, T] = HashMap.Empty
  
  def += (key: A, value: T): this.type = {
    map += (key, value)
    this
  }
  
  override def += (entry: (A, T)): this.type = this += (entry._1, entry._2)
  
  override def expect(count: Int): this.type = this
  
  override def state: HashMap[A, T] = map
  
  override def clear(): Unit = map = HashMap.Empty
}
