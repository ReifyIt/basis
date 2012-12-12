/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.util._

import scala.annotation.{switch, tailrec}
import scala.annotation.unchecked.uncheckedVariance

final class HashSet[+A] private[containers] (
    private[containers] val treeMap: Int,
    private[containers] val leafMap: Int,
    slots: Array[AnyRef])
  extends Family[HashSet[A]] with Set[A] {
  
  import HashSet.{VOID, LEAF, TREE, KNOT}
  
  override def isEmpty: Boolean = slotMap == 0
  
  override def size: Int = {
    var t = 0
    var i = 0
    var treeMap = this.treeMap
    var leafMap = this.leafMap
    while ((treeMap | leafMap) != 0) {
      ((leafMap & 1 | (treeMap & 1) << 1): @switch) match {
        case VOID => ()
        case LEAF => t += 1; i += 1
        case TREE => t += treeAt(i).size; i += 1
        case KNOT => t += knotAt(i).size; i += 1
      }
      treeMap >>>= 1
      leafMap >>>= 1
    }
    t
  }
  
  override def contains(elem: A @uncheckedVariance): Boolean = contains(elem, elem.##, 0)
  
  /** Returns a copy of this $collection containing the given element.
    * @group Updating */
  def + [B >: A](elem: B): HashSet[B] = update(elem, elem.##, 0)
  
  /** Returns a copy of this $collection, excluding the given element.
    * @group Updating */
  def - (elem: A @uncheckedVariance): HashSet[A] = remove(elem, elem.##, 0)
  
  private def knotMap: Int = treeMap & leafMap
  
  private def slotMap: Int = treeMap | leafMap
  
  private def choose(hash: Int, shift: Int): Int = 1 << ((hash >>> shift) & 0x1F)
  
  private def select(branch: Int): Int = (slotMap & (branch - 1)).countSetBits
  
  private def follow(branch: Int): Int =
    (if ((leafMap & branch) != 0) 1 else 0) | (if ((treeMap & branch) != 0) 2 else 0)
  
  private[containers] def leafAt(index: Int): A =
    slots(index).asInstanceOf[A]
  
  private def getLeaf(branch: Int): A =
    slots(select(branch)).asInstanceOf[A]
  
  private def setLeaf[B >: A](branch: Int, leaf: B): this.type = {
    slots(select(branch)) = leaf.asInstanceOf[AnyRef]
    this
  }
  
  private[containers] def treeAt(index: Int): HashSet[A] =
    slots(index).asInstanceOf[HashSet[A]]
  
  private def getTree(branch: Int): HashSet[A] =
    slots(select(branch)).asInstanceOf[HashSet[A]]
  
  private def setTree[B >: A](branch: Int, tree: HashSet[B]): this.type = {
    slots(select(branch)) = tree
    this
  }
  
  private[containers] def knotAt(index: Int): ArraySet[A] =
    slots(index).asInstanceOf[ArraySet[A]]
  
  private def getKnot(branch: Int): ArraySet[A] =
    slots(select(branch)).asInstanceOf[ArraySet[A]]
  
  private def setKnot[B >: A](branch: Int, knot: ArraySet[B]): this.type = {
    slots(select(branch)) = knot
    this
  }
  
  private def isUnary: Boolean = treeMap == 0 && leafMap.countSetBits == 1
  
  private def unaryElement: A = slots(0).asInstanceOf[A]
  
  private def remap(treeMap: Int, leafMap: Int): HashSet[A] = {
    var oldSlotMap = this.treeMap | this.leafMap
    var newSlotMap = treeMap | leafMap
    if (oldSlotMap == newSlotMap) new HashSet(treeMap, leafMap, this.slots.clone)
    else {
      var i = 0
      var j = 0
      val slots = new Array[AnyRef](newSlotMap.countSetBits)
      while (newSlotMap != 0) {
        if ((oldSlotMap & newSlotMap & 1) == 1) slots(j) = this.slots(i)
        if ((oldSlotMap & 1) == 1) i += 1
        if ((newSlotMap & 1) == 1) j += 1
        oldSlotMap >>>= 1
        newSlotMap >>>= 1
      }
      new HashSet(treeMap, leafMap, slots)
    }
  }
  
  @tailrec private def contains(elem: A @uncheckedVariance, elemHash: Int, shift: Int): Boolean = {
    val branch = choose(elemHash, shift)
    (follow(branch): @switch) match {
      case VOID => false
      case LEAF => elem == getLeaf(branch)
      case TREE => getTree(branch).contains(elem, elemHash, shift + 5)
      case KNOT => getKnot(branch).contains(elem)
    }
  }
  
  private def update[B >: A](elem: B, elemHash: Int, shift: Int): HashSet[B] = {
    val branch = choose(elemHash, shift)
    (follow(branch): @switch) match {
      case VOID => remap(treeMap, leafMap | branch).setLeaf(branch, elem)
      case LEAF =>
        val leaf = getLeaf(branch)
        val leafHash = leaf.##
        if (elemHash == leafHash && elem == leaf) this
        else if (elemHash != leafHash)
          remap(treeMap | branch, leafMap ^ branch).
            setTree(branch, merge(leaf, leafHash, elem, elemHash, shift + 5))
        else remap(treeMap | branch, leafMap).setKnot(branch, ArraySet(leaf, elem))
      case TREE =>
        val oldTree = getTree(branch)
        val newTree = oldTree.update(elem, elemHash, shift + 5)
        if (oldTree eq newTree) this
        else remap(treeMap, leafMap).setTree(branch, newTree)
      case KNOT =>
        val oldKnot = getKnot(branch)
        val newKnot = oldKnot + elem
        if (oldKnot eq newKnot) this
        else remap(treeMap, leafMap).setKnot(branch, newKnot)
    }
  }
  
  private def remove(elem: A @uncheckedVariance, elemHash: Int, shift: Int): HashSet[A] = {
    val branch = choose(elemHash, shift)
    (follow(branch): @switch) match {
      case VOID => this
      case LEAF =>
        if (elem != getLeaf(branch)) this
        else remap(treeMap, leafMap ^ branch)
      case TREE =>
        val oldTree = getTree(branch)
        val newTree = oldTree.remove(elem, elemHash, shift + 5)
        if (oldTree eq newTree) this
        else if (newTree.isEmpty) remap(treeMap ^ branch, leafMap)
        else if (newTree.isUnary) remap(treeMap ^ branch, leafMap | branch).setLeaf(branch, newTree.unaryElement)
        else remap(treeMap, leafMap).setTree(branch, newTree)
      case KNOT =>
        val oldKnot = getKnot(branch)
        val newKnot = oldKnot - elem
        if (oldKnot eq newKnot) this
        else if (newKnot.isEmpty) remap(treeMap ^ branch, leafMap)
        else if (newKnot.isUnary) remap(treeMap ^ branch, leafMap | branch).setLeaf(branch, newKnot.unaryElement)
        else remap(treeMap, leafMap).setKnot(branch, newKnot)
    }
  }
  
  private def merge[B >: A](elem0: B, hash0: Int, elem1: B, hash1: Int, shift: Int): HashSet[B] = {
    // assume(hash0 != hash1)
    val branch0 = choose(hash0, shift)
    val branch1 = choose(hash1, shift)
    val slotMap = branch0 | branch1
    if (branch0 == branch1) {
      val slots = new Array[AnyRef](1)
      slots(0) = merge(elem0, hash0, elem1, hash1, shift + 5)
      new HashSet(slotMap, 0, slots)
    }
    else {
      val slots = new Array[AnyRef](2)
      if (((branch0 - 1) & branch1) == 0) {
        slots(0) = elem0.asInstanceOf[AnyRef]
        slots(1) = elem1.asInstanceOf[AnyRef]
      }
      else {
        slots(0) = elem1.asInstanceOf[AnyRef]
        slots(1) = elem0.asInstanceOf[AnyRef]
      }
      new HashSet(0, slotMap, slots)
    }
  }
  
  override def iterator: Iterator[A] = new HashSetIterator(this)
  
  protected override def foreach[U](f: A => U) {
    var i = 0
    var treeMap = this.treeMap
    var leafMap = this.leafMap
    while ((treeMap | leafMap) != 0) {
      ((leafMap & 1 | (treeMap & 1) << 1): @switch) match {
        case VOID => ()
        case LEAF => f(leafAt(i)); i += 1
        case TREE => treeAt(i).foreach(f); i += 1
        case KNOT => knotAt(i).foreach(f); i += 1
      }
      treeMap >>>= 1
      leafMap >>>= 1
    }
  }
  
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
  val empty: HashSet[Nothing] = new HashSet(0, 0, new Array[AnyRef](0))
  
  implicit override def Builder[A]: Builder[Any, A] { type State = HashSet[A] } = new HashSetBuilder
  
  override def toString: String = "HashSet"
  
  private[containers] final val VOID = 0
  private[containers] final val LEAF = 1
  private[containers] final val TREE = 2
  private[containers] final val KNOT = 3
}

private[containers] final class HashSetIterator[+A](
    nodes: Array[AnyRef], private[this] var depth: Int,
    stack: Array[Int], private[this] var stackPointer: Int)
  extends Iterator[A] {
  
  import HashSet.{VOID, LEAF, TREE, KNOT}
  
  def this(tree: HashSet[A]) = {
    this(new Array[AnyRef](7), 0, new Array[Int](21), 0)
    node = tree
    i = 0
    treeMap = tree.treeMap
    leafMap = tree.leafMap
  }
  
  private[this] def node: AnyRef = nodes(depth)
  private[this] def node_=(node: AnyRef): Unit = nodes(depth) = node
  
  private[this] def i: Int = stack(stackPointer)
  private[this] def i_=(index: Int): Unit = stack(stackPointer) = index
  
  private[this] def treeMap: Int = stack(stackPointer + 1)
  private[this] def treeMap_=(treeMap: Int): Unit = stack(stackPointer + 1) = treeMap
  
  private[this] def leafMap: Int = stack(stackPointer + 2)
  private[this] def leafMap_=(leafMap: Int): Unit = stack(stackPointer + 2) = leafMap
  
  private[this] def follow: Int = leafMap & 1 | (treeMap & 1) << 1
  
  private[this] def push(tree: HashSet[A]) {
    depth += 1
    node = tree
    
    stackPointer += 3
    i = 0
    treeMap = tree.treeMap
    leafMap = tree.leafMap
  }
  
  private[this] def push(knot: ArraySet[A]) {
    depth += 1
    node = knot
    
    stackPointer += 3
    i = 0
  }
  
  private[this] def pop() {
    node = null
    depth -= 1
    
    i = 0
    treeMap = 0
    leafMap = 0
    stackPointer -= 3
    
    i += 1
    treeMap >>>= 1
    leafMap >>>= 1
  }
  
  @tailrec override def isEmpty: Boolean = node match {
    case node: HashSet[A] =>
      if ((treeMap | leafMap) != 0) (follow: @switch) match {
        case VOID =>
          treeMap >>>= 1
          leafMap >>>= 1
          isEmpty
        case LEAF => false
        case TREE =>
          push(node.treeAt(i))
          isEmpty
        case KNOT =>
          push(node.knotAt(i))
          isEmpty
      }
      else if (depth > 0) { pop(); isEmpty }
      else true
    case node: ArraySet[A] =>
      if (i < node.size) false
      else { pop(); isEmpty }
  }
  
  @tailrec override def head: A = node match {
    case node: HashSet[A] =>
      if ((treeMap | leafMap) != 0) (follow: @switch) match {
        case VOID =>
          treeMap >>>= 1
          leafMap >>>= 1
          head
        case LEAF => node.leafAt(i)
        case TREE =>
          push(node.treeAt(i))
          head
        case KNOT =>
          push(node.knotAt(i))
          head
      }
      else if (depth > 0) { pop(); head }
      else throw new NoSuchElementException("Head of empty iterator.")
    case node: ArraySet[A] =>
      if (i < node.size) node.elementAt(i)
      else { pop(); head }
  }
  
  @tailrec override def step(): Unit = node match {
    case node: HashSet[A] =>
      val slotMap = treeMap | leafMap
      if (slotMap != 0) {
        if ((slotMap & 1) == 1) i += 1
        treeMap >>>= 1
        leafMap >>>= 1
      }
      else if (depth > 0) { pop(); step() }
      else throw new UnsupportedOperationException("Empty iterator step.")
    case node: ArraySet[A] =>
      if (i < node.size) i += 1
      else { pop(); step() }
  }
  
  override def dup: Iterator[A] =
    new HashSetIterator(nodes.clone, depth, stack.clone, stackPointer)
}

private[containers] final class HashSetBuilder[A] extends Builder[Any, A] {
  override type State = HashSet[A]
  
  private[this] var set: HashSet[A] = HashSet.empty
  
  override def += (elem: A): this.type = {
    set += elem
    this
  }
  
  override def expect(count: Int): this.type = this
  
  override def state: HashSet[A] = set
  
  override def clear(): Unit = set = HashSet.empty
}
