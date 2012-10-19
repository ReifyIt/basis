/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._
import basis.util._

final class HashMap[A, +T] private
    (slotMap: Int, entryMap: Int, slots: RefArray[Any], links: RefArray[T])
  extends Map[A, T] {
  
  import scala.annotation.tailrec
  
  override type Self <: HashMap[A, T]
  
  override def isEmpty: Boolean = slots.isEmpty
  
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
  
  override def contains(key: A): Boolean = contains(key, key.##, 0)
  
  override def apply(key: A): T = apply(key, key.##, 0)
  
  override def get(key: A): Option[T] = get(key, key.##, 0)
  
  override def + [U >: T](key: A, value: U): HashMap[A, U] = update(key, key.##, value, 0)
  
  override def - (key: A): HashMap[A, T] = remove(key, key.##, 0)
  
  override def iterator: Iterator[(A, T)] = new HashMap.Iterator(this)
  
  protected override def foreach[U](f: ((A, T)) => U) {
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
  private[basis] def valAt(i: Int): T = links(i)
  
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
  @tailrec private def contains(key: A, h: Int, k: Int): Boolean = {
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
  @tailrec private def apply(key: A, h: Int, k: Int): T = {
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
  @tailrec private def get(key: A, h: Int, k: Int): Option[T] = {
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
  private def update[U >: T](key: A, h: Int, value: U, k: Int): HashMap[A, U] = {
    val n = branch(h, k)
    if (isEmpty) // new unary map
      new HashMap(n, n, newSlots(key), newLinks(value))
    else if (isTrie) { // update trie
      val i = slot(n)
      val j = link(n)
      if (hasEntryAt(n)) { // update entry
        val e = keyAt(i)
        if (key == e) // replace value
          new HashMap(slotMap, entryMap, slots, links.update(link(n), value))
        else // merge keys in new submap
          new HashMap(slotMap, entryMap ^ n,
            slots.update(i, merge(key, h, value, e, e.##, valAt(j), k + 5)),
            links.remove(j))
      }
      else if (hasSlotAt(n)) { // update submap
        val node = nodeAt(i)
        val newNode = node.update(key, h, value, k + 5)
        if (node eq newNode) this
        else new HashMap(slotMap, entryMap, slots.update(i, newNode), links)
      }
      else // insert entry
        new HashMap(slotMap | n, entryMap | n, slots.insert(i, key), links.insert(j, value))
    }
    else { // update collision bucket
      if (h == bucketHash) {
        var i = 0
        var n = rank
        while (i < n) {
          if (key == keyAt(i))
            return new HashMap(0, bucketHash, slots, links.update(i, value))
          i += 1
        }
        new HashMap(0, bucketHash, slots :+ key, links :+ value)
      }
      else // reconcile key with collision bucket
        resolve(key, h, value, k)
    }
  }
  
  /** Returns a map containing the two key, value pairs. */
  private[this] def merge[U >: T](
      keyA: A, hashA: Int, valueA: U,
      keyB: A, hashB: Int, valueB: U,
      k: Int)
    : HashMap[A, U] = {
    if (hashA == hashB) // new collision bucket
      new HashMap(0, hashA, newSlots(keyA, keyB), newLinks(valueA, valueB))
    else {
      val branchA = branch(hashA, k)
      val branchB = branch(hashB, k)
      if (branchA == branchB) // new submap
        new HashMap(branchA | branchB, 0,
          newSlots(merge(keyA, hashA, valueA, keyB, hashB, valueB, k + 5)),
          RefArray.empty)
      else // new branch
        new HashMap(branchA | branchB, branchA | branchB,
          if (((branchA - 1) & branchB) == 0) newSlots(keyA, keyB) else newSlots(keyB, keyA),
          if (((branchA - 1) & branchB) == 0) newLinks(valueA, valueB) else newLinks(valueB, valueA))
    }
  }
  
  /** Returns a set containing this hash bucket and the key, value pair. */
  private[this] def resolve[U >: T](key: A, h: Int, value: U, k: Int): HashMap[A, U] = {
    // assume(h != bucketHash)
    val bucketBranch = branch(bucketHash, k)
    val entryBranch = branch(h, k)
    if (bucketBranch == entryBranch) // new submap
      new HashMap(bucketBranch, 0,
        newSlots(resolve(key, h, value, k + 5)),
        RefArray.empty)
    else // new branch
      new HashMap(bucketBranch | entryBranch, entryBranch,
        if (((bucketBranch - 1) & entryBranch) == 0) newSlots(this, key) else newSlots(key, this),
        newLinks(value))
  }
  
  /** Returns a copy of this map without a value associated with the given key,
    * or this map itself if it doesn't contain the key. */
  private def remove(key: A, h: Int, k: Int): HashMap[A, T] = {
    if (isEmpty) this
    else if (isTrie) { // remove from trie
      val n = branch(h, k)
      val i = slot(n)
      val j = link(n)
      if (hasEntryAt(n)) { // remove entry
        if (key == keyAt(i))
          new HashMap(slotMap ^ n, entryMap ^ n, slots.remove(i), links.remove(j))
        else this
      }
      else if (hasSlotAt(n)) { // remove from submap
        val node = nodeAt(i)
        val newNode = node.remove(key, h, k + 5)
        if (node eq newNode) this
        else if (newNode.isEmpty) // remove empty submaps
          new HashMap(slotMap ^ n, entryMap, slots.remove(i), links)
        else if (newNode.isUnary) // lift unary submaps
          new HashMap(slotMap, entryMap | n, slots.update(i, newNode.keyAt(0)), links.update(j, newNode.valAt(0)))
        else new HashMap(slotMap, entryMap, slots.update(i, newNode), links)
      }
      else this
    }
    else { // remove from collision bucket
      if (h == bucketHash) {
        var i = 0
        val n = rank
        while (i < n) {
          if (key == keyAt(i))
            return new HashMap(0, bucketHash, slots.remove(i), links.remove(i))
          i += 1
        }
      }
      this
    }
  }
  
  private[this] def newSlots(elem: Any): RefArray[Any] =
    new RefArray(scala.Array(elem.asInstanceOf[AnyRef]))
  
  private[this] def newSlots(elemA: Any, elemB: Any): RefArray[Any] =
    new RefArray(scala.Array(elemA.asInstanceOf[AnyRef], elemB.asInstanceOf[AnyRef]))
  
  private[this] def newLinks[U >: T](value: U): RefArray[U] =
    new RefArray(scala.Array(value.asInstanceOf[AnyRef]))
  
  private[this] def newLinks[U >: T](valueA: U, valueB: U): RefArray[U] =
    new RefArray(scala.Array(valueA.asInstanceOf[AnyRef], valueB.asInstanceOf[AnyRef]))
}

object HashMap {
  private[this] val Empty = new HashMap[Any, Nothing](0, 0, RefArray.empty, RefArray.empty)
  def empty[A, T]: HashMap[A, T] = Empty.asInstanceOf[HashMap[A, T]]
  
  implicit def Buffer[A, T]: HashMap.Buffer[A, T] = new HashMap.Buffer[A, T]
  
  final class Buffer[A, T] extends basis.Buffer[Any, (A, T)] {
    override type State = HashMap[A, T]
    
    private[this] var map: HashMap[A, T] = HashMap.empty[A, T]
    
    def += (key: A, value: T): this.type = {
      map += (key, value)
      this
    }
    
    override def += (entry: (A, T)): this.type = this += (entry._1, entry._2)
    
    override def expect(count: Int): this.type = this
    
    override def state: HashMap[A, T] = map
    
    override def clear(): Unit = map = HashMap.empty[A, T]
  }
  
  private[basis] final class Iterator[A, +T](
      self: HashMap[A, T],
      private[this] var child: HashMap.Iterator[A, T],
      private[this] var index: Int)
    extends basis.Iterator[(A, T)] {
    
    import scala.annotation.tailrec
    
    def this(self: HashMap[A, T]) = this(self, null, 0)
    
    @tailrec override def isEmpty: Boolean = {
      if (child != null)
        child.asInstanceOf[basis.Iterator[_]].isEmpty && { child = null; isEmpty }
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
            child = new HashMap.Iterator(self.nodeAt(self.slot(n)))
            index += 1
            head
          }
          else {
            index += 1
            head
          }
        }
        else Iterator.empty.head
      }
      else if (index < self.rank) (self.keyAt(index), self.valAt(index))
      else Iterator.empty.head
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
            child = new HashMap.Iterator(self.nodeAt(self.slot(n)))
            index += 1
            step()
          }
          else {
            index += 1
            step()
          }
        }
        else Iterator.empty.step()
      }
      else if (index < self.rank) index += 1
      else Iterator.empty.step()
    }
    
    override def dup: HashMap.Iterator[A, T] =
      new HashMap.Iterator(self, child, index)
  }
}
