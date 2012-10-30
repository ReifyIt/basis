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

final class HashSet[+A] private
    (slotMap: Int, elemMap: Int, slots: RefArray[Any])
  extends Set[A] {
  
  override type Self <: HashSet[A]
  
  override def isEmpty: Boolean = slots.isEmpty
  
  override def size: Int = {
    if (!isTrie || slotMap == elemMap) rank
    else  {
      var t = 0
      var i = 0
      while (i < 32 && hasSlotAbove(i)) {
        val n = 1 << i
        if (hasElemAt(n)) t += 1
        else if (hasSlotAt(n)) t += nodeAt(slot(n)).size
        i += 1
      }
      t
    }
  }
  
  override def contains(element: A @uncheckedVariance): Boolean = contains(element, element.##, 0)
  
  /** Returns a copy of this $collection containing the given element. */
  def + [B >: A](element: B): HashSet[B] = update(element, element.##, 0)
  
  /** Returns a copy of this $collection excluding the given element. */
  def - (element: A @uncheckedVariance): HashSet[A] = remove(element, element.##, 0)
  
  override def iterator: Iterator[A] = new HashSet.Cursor(this)
  
  protected override def foreach[U](f: A => U) {
    var i = 0
    if (isTrie) {
      while (i < 32 && hasSlotAbove(i)) {
        val n = 1 << i
        if (hasElemAt(n)) f(elemAt(slot(n)))
        else if (hasSlotAt(n)) nodeAt(slot(n)) foreach f
        i += 1
      }
    }
    else {
      var n = rank
      while (i < n) {
        f(elemAt(i))
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
  
  /** Returns the subset at a slot index.
    * 
    * @param  i   the index of a slot known to hold a subset.
    */
  private[basis] def nodeAt(i: Int): HashSet[A] = slots(i).asInstanceOf[HashSet[A]]
  
  /** Returns the element at a slot index.
    * 
    * @param  i   the index of a slot known to hold an element.
    */
  private[basis] def elemAt(i: Int): A = slots(i).asInstanceOf[A]
  
  /** Returns the number of filled slots in this node. */
  private[basis] def rank: Int = slots.length
  
  /** Returns `true` if this node directly contains exactly one element. */
  private[basis] def isUnary: Boolean =
    rank == 1 && (slotMap == elemMap || slotMap == 0)
  
  /** Returns the hash code of the elements in this known collision bucket. */
  private[basis] def bucketHash: Int = elemMap // assume(!isTrie)
  
  /** Returns `true` if this node contains the given element.
    * 
    * @param  elem  the element to search for.
    * @param  h     the element's hash code.
    * @param  k     the shift-level of this node.
    */
  @tailrec private def contains(elem: A @uncheckedVariance, h: Int, k: Int): Boolean = {
    if (isTrie) { // search trie
      val n = branch(h, k)
      if (hasElemAt(n)) elem == elemAt(slot(n))
      else if (hasSlotAt(n)) nodeAt(slot(n)).contains(elem, h, k + 5)
      else false
    }
    else { // search collision bucket
      if (h == bucketHash) {
        var i = 0
        val n = rank
        while (i < n) {
          if (elem == elemAt(i)) return true
          i += 1
        }
      }
      false
    }
  }
  
  /** Returns a copy of this set including the given element, or this set
    * itself if it already contains the element.
    * 
    * @param  elem  the element to include.
    * @param  h     the element's hash code.
    * @param  k     the shift-level of this node.
    */
  private def update[B >: A](elem: B, h: Int, k: Int): HashSet[B] = {
    val n = branch(h, k)
    if (isEmpty) // new unary set
      new HashSet(n, n, newSlots(elem))
    else if (isTrie) { // update trie
      val i = slot(n)
      if (hasElemAt(n)) { // update element
        val e = elemAt(i)
        if (elem == e) this
        else // merge elements in new subset
          new HashSet(slotMap, elemMap ^ n,
            slots.update(i, merge(elem, h, e, e.##, k + 5)))
      }
      else if (hasSlotAt(n)) { // update subset
        val node = nodeAt(i)
        val newNode = node.update(elem, h, k + 5)
        if (node eq newNode) this
        else new HashSet(slotMap, elemMap, slots.update(i, newNode))
      }
      else // insert element
        new HashSet(slotMap | n, elemMap | n, slots.insert(i, elem))
    }
    else { // update collision bucket
      if (h == bucketHash) {
        var i = 0
        var n = rank
        while (i < n) {
          if (elem == elemAt(i)) return this
          i += 1
        }
        new HashSet(0, bucketHash, slots :+ elem)
      }
      else // reconcile element with collision bucket
        resolve(elem, h, k)
    }
  }
  
  /** Returns a set containing the two elements. */
  private[this] def merge[B >: A](elemA: B, hashA: Int, elemB: B, hashB: Int, k: Int): HashSet[B] = {
    if (hashA == hashB) // new collision bucket
      new HashSet(0, hashA, newSlots(elemA, elemB))
    else {
      val branchA = branch(hashA, k)
      val branchB = branch(hashB, k)
      if (branchA == branchB) // new subset
        new HashSet(branchA | branchB, 0,
          newSlots(merge(elemA, hashA, elemB, hashB, k + 5)))
      else // new branch
        new HashSet(branchA | branchB, branchA | branchB,
          if (((branchA - 1) & branchB) == 0) newSlots(elemA, elemB) else newSlots(elemB, elemA))
    }
  }
  
  /** Returns a set containing this hash bucket and the element. */
  private[this] def resolve[B >: A](elem: B, h: Int, k: Int): HashSet[B] = {
    // assume(h != bucketHash)
    val bucketBranch = branch(bucketHash, k)
    val elemBranch = branch(h, k)
    if (bucketBranch == elemBranch) // new subset
      new HashSet(bucketBranch, 0,
        newSlots(resolve(elem, h, k + 5)))
    else // new branch
      new HashSet(bucketBranch | elemBranch, elemBranch,
        if (((bucketBranch - 1) & elemBranch) == 0) newSlots(this, elem) else newSlots(elem, this))
  }
  
  /** Returns a copy of this set removing the given element, or this set itself
    * if it doesn't contain the element. */
  private def remove(elem: A @uncheckedVariance, h: Int, k: Int): HashSet[A] = {
    if (isEmpty) this
    else if (isTrie) { // remove from trie
      val n = branch(h, k)
      val i = slot(n)
      if (hasElemAt(n)) { // remove element
        if (elem == elemAt(i))
          new HashSet(slotMap ^ n, elemMap ^ n, slots.remove(i))
        else this
      }
      else if (hasSlotAt(n)) { // remove from subset
        val node = nodeAt(i)
        val newNode = node.remove(elem, h, k + 5)
        if (node eq newNode) this
        else if (newNode.isEmpty) // remove empty subsets
          new HashSet(slotMap ^ n, elemMap, slots.remove(i))
        else if (newNode.isUnary) // lift unary subsets
          new HashSet(slotMap, elemMap | n, slots.update(i, newNode.elemAt(0)))
        else new HashSet(slotMap, elemMap, slots.update(i, newNode))
      }
      else this
    }
    else { // remove from collision bucket
      if (h == bucketHash) {
        var i = 0
        val n = rank
        while (i < n) {
          if (elem == elemAt(i))
            return new HashSet(0, bucketHash, slots.remove(i))
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
  
  override def toString: String = {
    val s = new java.lang.StringBuilder("HashSet")
    s.append('(')
    if (!isEmpty) {
      val iter = iterator
      s.append(iter.head)
      iter.step()
      while (!iter.isEmpty) {
        s.append(", ").append(iter.head)
        iter.step()
      }
    }
    s.append(')')
    s.toString
  }
}

object HashSet extends SetFactory[HashSet] {
  val Empty: HashSet[Nothing] = new HashSet[Nothing](0, 0, RefArray.Empty)
  
  implicit override def Builder[A]: Builder[A] = new Builder[A]
  
  final class Builder[A] extends Buffer[Any, A] {
    override type State = HashSet[A]
    
    private[this] var set: HashSet[A] = HashSet.Empty
    
    override def += (element: A): this.type = {
      set += element
      this
    }
    
    override def expect(count: Int): this.type = this
    
    override def state: HashSet[A] = set
    
    override def clear(): Unit = set = HashSet.Empty
  }
  
  private[immutable] final class Cursor[+A](
      self: HashSet[A],
      private[this] var child: Cursor[A],
      private[this] var index: Int)
    extends Iterator[A] {
    
    def this(self: HashSet[A]) = this(self, null, 0)
    
    @tailrec override def isEmpty: Boolean = {
      if (child != null)
        child.asInstanceOf[Iterator[_]].isEmpty && { child = null; isEmpty }
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
      else if (index < self.rank) self.elemAt(index)
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
          if (self.hasElemAt(n)) index += 1
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
    
    override def dup: Cursor[A] = new Cursor(self, child, index)
  }
}
