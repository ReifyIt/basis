/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.container

import basis._
import basis.collection._

final class HashSet[A] private
    (slotMap: Int, elemMap: Int, slots: RefArray[Any])
    (implicit A: Hash[A])
  extends Set[A] {
  
  import scala.annotation.tailrec
  
  override type Self = HashSet[A]
  
  override def isEmpty: Boolean = slots.isEmpty
  
  override def size: Int = {
    var t: Int = 0
    val iter = iterator
    while (!iter.isEmpty) {
      t += 1
      iter.step()
    }
    t
  }
  
  override def contains(element: A): Boolean = contains(element, A.hash(element), 0)
  
  override def + (element: A): HashSet[A] = update(element, A.hash(element), 0)
  
  override def - (element: A): HashSet[A] = remove(element, A.hash(element), 0)
  
  override def iterator: Iterator[A] = new HashSetIterator[A](this)
  
  protected override def foreach[U](f: A => U) {
    if (isTrie) {
      var j = 0
      while (j < 32 && hasSlotAbove(j)) {
        val n = 1 << j
        if (hasElemAt(n)) f(elemAt(slot(n)))
        else if (hasSlotAt(n)) nodeAt(slot(n)) foreach f
        j += 1
      }
    }
    else {
      var j = 0
      var n = rank
      while (j < n) {
        f(elemAt(j))
        j += 1
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
  
  /** Returns `true` if the nth trie branch contains an element or a subset.
    * 
    * @param  n   the single set bit identifying the branch to test.
    */
  private[basis] def hasSlotAt(n: Int): Boolean = (slotMap & n) != 0
  
  /** Returns `true` if the nth trie branch contains an element.
    * 
    * @param  n   the single set bit identifying the branch to test.
    */
  private[basis] def hasElemAt(n: Int): Boolean = (elemMap & n) != 0
  
  /** Returns the trie branch of the low 5 shifted hash bits.
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
  
  /** Returns the subset at the slot index.
    * 
    * @param  i   the index of a slot known to hold a subset.
    */
  private[basis] def nodeAt(i: Int): HashSet[A] = slots(i).asInstanceOf[HashSet[A]]
  
  /** Returns the element at the slot index.
    * 
    * @param  i   the index of a slot known to hold an element.
    */
  private[basis] def elemAt(i: Int): A = slots(i).asInstanceOf[A]
  
  /** Returns the number of slots filled by this node. */
  private[basis] def rank: Int = slots.length
  
  /** Returns `true` if this node directly contains exactly one element. */
  private[basis] def isUnary: Boolean =
    rank == 1 && (slotMap == elemMap || slotMap == 0)
  
  /** Returns the hash code of the elements in this known collision bucket. */
  private[basis] def bucketHash: Int = elemMap // assume(!isTrie)
  
  /** Returns `true` if this node contains the element.
    * 
    * @param  elem  the element to search for.
    * @param  h     the element's hash code.
    * @param  k     the shift-level of this node.
    */
  @tailrec private def contains(elem: A, h: Int, k: Int): Boolean = {
    if (isTrie) { // search trie
      val n = branch(h, k)
      if (hasElemAt(n)) A.equal(elemAt(slot(n)), elem)
      else if (hasSlotAt(n)) nodeAt(slot(n)).contains(elem, h, k + 5)
      else false
    }
    else { // search collision bucket
      if (h == bucketHash) {
        var i = 0
        val n = rank
        while (i < n) {
          if (A.equal(elem, elemAt(i))) return true
          i += 1
        }
      }
      false
    }
  }
  
  /** Returns a copy of this set including the element, or this set itself if
    * it already contains the element.
    * 
    * @param  elem  the element to include.
    * @param  h     the element's hash code.
    * @param  k     the shift-level of this node.
    */
  private def update(elem: A, h: Int, k: Int): HashSet[A] = {
    val n = branch(h, k)
    if (isEmpty) { // update empty set
      val newSlots = new scala.Array[AnyRef](1)
      newSlots(0) = elem.asInstanceOf[AnyRef]
      new HashSet[A](n, n, new RefArray[A](newSlots))
      // new HashSet[A](n, n, RefArray[Any](elem)) // someday
    }
    else if (isTrie) { // update trie
      val i = slot(n)
      if (hasElemAt(n)) { // update element
        val e = elemAt(i)
        if (A.equal(elem, e)) this
        else {
          val newNode = merge(elem, h, e, A.hash(e), k + 5)
          new HashSet[A](slotMap, elemMap ^ n, slots.update(i, newNode))
        }
      }
      else if (hasSlotAt(n)) { // update subset
        val node = nodeAt(i)
        val newNode = node.update(elem, h, k + 5)
        if (node eq newNode) this
        else new HashSet[A](slotMap, elemMap, slots.update(i, newNode))
      }
      else new HashSet[A](slotMap | n, elemMap | n, slots.insert(i, elem))
    }
    else { // update collision bucket
      if (h == bucketHash) new HashSet[A](0, bucketHash, slots :+ elem)
      else resolve(elem, h, k)
    }
  }
  
  /** Returns a set containing the two elements. */
  private[this] def merge(elemA: A, hashA: Int, elemB: A, hashB: Int, k: Int): HashSet[A] = {
    if (hashA == hashB) { // full hash collision
      val newSlots = new scala.Array[AnyRef](2)
      newSlots(0) = elemA.asInstanceOf[AnyRef]
      newSlots(1) = elemB.asInstanceOf[AnyRef]
      new HashSet[A](0, hashA, new RefArray[Any](newSlots))
      // new HashSet[A](0, hashA, RefArray[Any](elemA, elemB)) // someday
    }
    else {
      val branchA = branch(hashA, k)
      val branchB = branch(hashB, k)
      val newMap = branchA | branchB
      if (branchA == branchB) { // hash prefix collision
        val newNode = merge(elemA, hashA, elemB, hashB, k + 5)
        val newSlots = new scala.Array[AnyRef](1)
        newSlots(0) = newNode
        new HashSet[A](newMap, 0, new RefArray[Any](newSlots))
        // new HashSet[A](newMap, 0, RefArray[Any](newNode)) // someday
      }
      else { // divergent hash prefixes
        val newSlots = new scala.Array[AnyRef](2)
        if (((branchA - 1) & branchB) == 0) {
          newSlots(0) = elemA.asInstanceOf[AnyRef]
          newSlots(1) = elemB.asInstanceOf[AnyRef]
        }
        else {
          newSlots(0) = elemB.asInstanceOf[AnyRef]
          newSlots(1) = elemA.asInstanceOf[AnyRef]
        }
        new HashSet[A](newMap, newMap, new RefArray[Any](newSlots))
      }
    }
  }
  
  /** Returns a set containing this hash bucket and the element. */
  private[this] def resolve(elem: A, h: Int, k: Int): HashSet[A] = {
    // assume(h != bucketHash)
    val bucketBranch = branch(bucketHash, k)
    val elemBranch = branch(h, k)
    if (bucketBranch == elemBranch) { // hash prefix collision
      val newNode = resolve(elem, h, k + 5)
      val newSlots = new scala.Array[AnyRef](1)
      newSlots(0) = newNode
      new HashSet[A](bucketBranch, 0, new RefArray[Any](newSlots))
      // new HashSet[A](bucketBranch, 0, RefArray[Any](newNode)) // someday
    }
    else {
      val newSlots = new scala.Array[AnyRef](2)
      if (((bucketBranch - 1) & elemBranch) == 0) {
        newSlots(0) = this
        newSlots(1) = elem.asInstanceOf[AnyRef]
      }
      else {
        newSlots(0) = elem.asInstanceOf[AnyRef]
        newSlots(1) = this
      }
      new HashSet[A](bucketBranch | elemBranch, elemBranch, new RefArray[Any](newSlots))
    }
  }
  
  /** Returns a copy of this set removing the element, or this set itself if
    * it doesn't contain the element. */
  private def remove(elem: A, h: Int, k: Int): HashSet[A] = {
    if (isEmpty) this
    else if (isTrie) { // remove from trie
      val n = branch(h, k)
      val i = slot(n)
      if (hasElemAt(n)) { // remove element
        if (A.equal(elem, elemAt(i)))
          new HashSet[A](slotMap ^ n, elemMap ^ n, slots.remove(i))
        else this
      }
      else if (hasSlotAt(n)) { // remove from subset
        val node = nodeAt(i)
        val newNode = node.remove(elem, h, k + 5)
        if (node eq newNode) this
        else if (newNode.isEmpty) // remove empty subsets
          new HashSet[A](slotMap ^ n, elemMap, slots.remove(i))
        else if (newNode.isUnary) // lift unary subsets
          new HashSet[A](slotMap, elemMap | n, slots.update(i, newNode.elemAt(0)))
        else new HashSet[A](slotMap, elemMap, slots.update(i, newNode))
      }
      else this
    }
    else { // remove from collision bucket
      if (h == bucketHash) {
        var i = 0
        val n = rank
        while (i < n) {
          if (A.equal(elem, elemAt(i)))
            return new HashSet[A](0, bucketHash, slots.remove(i))
          i += 1
        }
      }
      this
    }
  }
}

object HashSet extends ContainerFactory[HashSet] {
  def empty[A : Hash]: HashSet[A] = new HashSet[A](0, 0, RefArray.empty)
  
  implicit def Buffer[A : Hash]: HashSetBuffer[A] = new HashSetBuffer[A]
  
  protected override def stringPrefix: String = "HashSet"
}

private[basis] final class HashSetIterator[A](
    self: HashSet[A],
    private[this] var child: HashSetIterator[A],
    private[this] var index: Int)
  extends Iterator[A] {
  
  import scala.annotation.tailrec
  
  def this(self: HashSet[A]) = this(self, null, 0)
  
  @tailrec override def isEmpty: Boolean = {
    if (child != null)
      child.asInstanceOf[Iterator[A]].isEmpty && { child = null; isEmpty }
    else if (self.isTrie)
      index >= 32 || !self.hasSlotAbove(index) ||
        (!self.hasSlotAt(1 << index) && { index += 1; isEmpty })
    else index >= self.rank
  }
  
  @tailrec override def head: A = {
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
        if (self.hasElemAt(n)) self.elemAt(self.slot(n))
        else if (self.hasSlotAt(n)) {
          child = new HashSetIterator[A](self.nodeAt(self.slot(n)))
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
    else if (index < self.rank) self.elemAt(index)
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
        if (self.hasElemAt(n)) index += 1
        else if (self.hasSlotAt(n)) {
          child = new HashSetIterator[A](self.nodeAt(self.slot(n)))
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
  
  override def dup: HashSetIterator[A] = new HashSetIterator[A](self, child, index)
}

final class HashSetBuffer[A](implicit A: Hash[A]) extends Buffer[Any, A] {
  override type State = HashSet[A]
  
  private[this] var set: HashSet[A] = HashSet.empty[A]
  
  override def += (element: A): this.type = {
    set += element
    this
  }
  
  override def expect(count: Int): this.type = this
  
  override def state: HashSet[A] = set
  
  override def clear(): Unit = set = HashSet.empty[A]
}
