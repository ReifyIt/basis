/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.control._
import basis.runtime._
import basis.util._

import scala.annotation.{switch, tailrec}
import scala.annotation.unchecked.uncheckedVariance

/** A hash array-mapped trie map.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * @group    Containers
  * 
  * @groupprio  Quantifying   1
  * @groupprio  Querying      2
  * @groupprio  Updating      3
  * @groupprio  Iterating     4
  * @groupprio  Traversing    5
  * @groupprio  Classifying   6
  * 
  * @define collection  hash map
  */
final class HashMap[+A, +T] private[containers] (
    private[containers] val treeMap: Int,
    private[containers] val leafMap: Int,
    slots: Array[AnyRef])
  extends Equals with Immutable with Family[HashMap[A, T]] with Map[A, T] {
  
  import HashMap.{VOID, LEAF, TREE, KNOT}
  
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
  
  override def contains(key: A @uncheckedVariance): Boolean = contains(key, key.##, 0)
  
  override def apply(key: A @uncheckedVariance): T = apply(key, key.##, 0)
  
  override def get(key: A @uncheckedVariance): Maybe[T] = get(key, key.##, 0)
  
  /** Returns a copy of this $collection that associates the given value with the given key.
    * @group Updating */
  def + [B >: A, U >: T](key: B, value: U): HashMap[B, U] = update(key, key.##, value, 0)
  
  /** Returns a copy of this $collection that associates nothing with the given key.
    * @group Updating */
  def - (key: A @uncheckedVariance): HashMap[A, T] = remove(key, key.##, 0)
  
  private def knotMap: Int = treeMap & leafMap
  
  private def slotMap: Int = treeMap | leafMap
  
  private def choose(hash: Int, shift: Int): Int = 1 << ((hash >>> shift) & 0x1F)
  
  private def select(branch: Int): Int = (slotMap & (branch - 1)).countSetBits
  
  private def lookup(branch: Int): Int = (leafMap & (branch - 1)).countSetBits
  
  private def follow(branch: Int): Int =
    (if ((leafMap & branch) != 0) 1 else 0) | (if ((treeMap & branch) != 0) 2 else 0)
  
  private[containers] def keyAt(index: Int): A =
    slots(index).asInstanceOf[A]
  
  private def getKey(branch: Int): A =
    slots(select(branch)).asInstanceOf[A]
  
  private def setKey[B >: A](branch: Int, key: B): this.type = {
    slots(select(branch)) = key.asInstanceOf[AnyRef]
    this
  }
  
  private[containers] def valueAt(index: Int): T =
    slots(slots.length - index - 1).asInstanceOf[T]
  
  private def getValue(branch: Int): T =
    slots(slots.length - lookup(branch) - 1).asInstanceOf[T]
  
  private def setValue[U >: T](branch: Int, value: U): this.type = {
    slots(slots.length - lookup(branch) - 1) = value.asInstanceOf[AnyRef]
    this
  }
  
  private def setLeaf[B >: A, U >: T](branch: Int, key: B, value: U): this.type = {
    slots(select(branch)) = key.asInstanceOf[AnyRef]
    slots(slots.length - lookup(branch) - 1) = value.asInstanceOf[AnyRef]
    this
  }
  
  private[containers] def treeAt(index: Int): HashMap[A, T] =
    slots(index).asInstanceOf[HashMap[A, T]]
  
  private def getTree(branch: Int): HashMap[A, T] =
    slots(select(branch)).asInstanceOf[HashMap[A, T]]
  
  private def setTree[B >: A, U >: T](branch: Int, tree: HashMap[B, U]): this.type = {
    slots(select(branch)) = tree
    this
  }
  
  private[containers] def knotAt(index: Int): ArrayMap[A, T] =
    slots(index).asInstanceOf[ArrayMap[A, T]]
  
  private def getKnot(branch: Int): ArrayMap[A, T] =
    slots(select(branch)).asInstanceOf[ArrayMap[A, T]]
  
  private def setKnot[B >: A, U >: T](branch: Int, knot: ArrayMap[B, U]): this.type = {
    slots(select(branch)) = knot
    this
  }
  
  private def isUnary: Boolean = treeMap == 0 && leafMap.countSetBits == 1
  
  private def unaryKey: A = slots(0).asInstanceOf[A]
  
  private def unaryValue: T = slots(1).asInstanceOf[T]
  
  private def remap(treeMap: Int, leafMap: Int): HashMap[A, T] = {
    var oldLeafMap = this.leafMap
    var newLeafMap = leafMap
    var oldSlotMap = this.treeMap | this.leafMap
    var newSlotMap = treeMap | leafMap
    if (oldLeafMap == newLeafMap && oldSlotMap == newSlotMap)
      new HashMap(treeMap, leafMap, this.slots.clone)
    else {
      var i = 0
      var j = 0
      val slots = new Array[AnyRef](newSlotMap.countSetBits + newLeafMap.countSetBits)
      while (newSlotMap != 0) {
        if ((oldSlotMap & newSlotMap & 1) == 1) slots(j) = this.slots(i)
        if ((oldSlotMap & 1) == 1) i += 1
        if ((newSlotMap & 1) == 1) j += 1
        oldSlotMap >>>= 1
        newSlotMap >>>= 1
      }
      i = this.slots.length - 1
      j = slots.length - 1
      while (newLeafMap != 0) {
        if ((oldLeafMap & newLeafMap & 1) == 1) slots(j) = this.slots(i)
        if ((oldLeafMap & 1) == 1) i -= 1
        if ((newLeafMap & 1) == 1) j -= 1
        oldLeafMap >>>= 1
        newLeafMap >>>= 1
      }
      new HashMap(treeMap, leafMap, slots)
    }
  }
  
  @tailrec private def contains(key: A @uncheckedVariance, keyHash: Int, shift: Int): Boolean = {
    val branch = choose(keyHash, shift)
    (follow(branch): @switch) match {
      case VOID => false
      case LEAF => key == getKey(branch)
      case TREE => getTree(branch).contains(key, keyHash, shift + 5)
      case KNOT => getKnot(branch).contains(key)
    }
  }
  
  @tailrec private def apply(key: A @uncheckedVariance, keyHash: Int, shift: Int): T = {
    val branch = choose(keyHash, shift)
    (follow(branch): @switch) match {
      case VOID => throw new NoSuchElementException(key.toString)
      case LEAF =>
        if (key == getKey(branch)) getValue(branch)
        else throw new NoSuchElementException(key.toString)
      case TREE => getTree(branch).apply(key, keyHash, shift + 5)
      case KNOT => getKnot(branch).apply(key)
    }
  }
  
  @tailrec private def get(key: A @uncheckedVariance, keyHash: Int, shift: Int): Maybe[T] = {
    val branch = choose(keyHash, shift)
    (follow(branch): @switch) match {
      case VOID => Trap
      case LEAF => if (key == getKey(branch)) Free(getValue(branch)) else Trap
      case TREE => getTree(branch).get(key, keyHash, shift + 5)
      case KNOT => getKnot(branch).get(key)
    }
  }
  
  private def update[B >: A, U >: T](key: B, keyHash: Int, value: U, shift: Int): HashMap[B, U] = {
    val branch = choose(keyHash, shift)
    (follow(branch): @switch) match {
      case VOID => remap(treeMap, leafMap | branch).setLeaf(branch, key, value)
      case LEAF =>
        val leaf = getKey(branch)
        val leafHash = leaf.##
        if (keyHash == leafHash && key == leaf) this
        else if (keyHash != leafHash)
          remap(treeMap | branch, leafMap ^ branch).
            setTree(branch, merge(leaf, leafHash, getValue(branch), key, keyHash, value, shift + 5))
        else
          remap(treeMap | branch, leafMap).
            setKnot(branch, ArrayMap(leaf, getValue(branch), key, value))
      case TREE =>
        val oldTree = getTree(branch)
        val newTree = oldTree.update(key, keyHash, value, shift + 5)
        if (oldTree eq newTree) this
        else remap(treeMap, leafMap).setTree(branch, newTree)
      case KNOT =>
        val oldKnot = getKnot(branch)
        val newKnot = oldKnot + (key, value)
        if (oldKnot eq newKnot) this
        else remap(treeMap, leafMap).setKnot(branch, newKnot)
    }
  }
  
  private def remove(key: A @uncheckedVariance, keyHash: Int, shift: Int): HashMap[A, T] = {
    val branch = choose(keyHash, shift)
    (follow(branch): @switch) match {
      case VOID => this
      case LEAF =>
        if (key != getKey(branch)) this
        else remap(treeMap, leafMap ^ branch)
      case TREE =>
        val oldTree = getTree(branch)
        val newTree = oldTree.remove(key, keyHash, shift + 5)
        if (oldTree eq newTree) this
        else if (newTree.isEmpty) remap(treeMap ^ branch, leafMap)
        else if (newTree.isUnary)
          remap(treeMap ^ branch, leafMap | branch).
            setLeaf(branch, newTree.unaryKey, newTree.unaryValue)
        else remap(treeMap, leafMap).setTree(branch, newTree)
      case KNOT =>
        val oldKnot = getKnot(branch)
        val newKnot = oldKnot - key
        if (oldKnot eq newKnot) this
        else if (newKnot.isEmpty) remap(treeMap ^ branch, leafMap)
        else if (newKnot.isUnary)
          remap(treeMap ^ branch, leafMap | branch).
            setLeaf(branch, newKnot.unaryKey, newKnot.unaryValue)
        else remap(treeMap, leafMap).setKnot(branch, newKnot)
    }
  }
  
  private def merge[B >: A, U >: T](
      key0: B, hash0: Int, value0: U,
      key1: B, hash1: Int, value1: U,
      shift: Int)
    : HashMap[B, U] = {
    // assume(hash0 != hash1)
    val branch0 = choose(hash0, shift)
    val branch1 = choose(hash1, shift)
    val slotMap = branch0 | branch1
    if (branch0 == branch1) {
      val slots = new Array[AnyRef](1)
      slots(0) = merge(key0, hash0, value0, key1, hash1, value1, shift + 5)
      new HashMap(slotMap, 0, slots)
    }
    else {
      val slots = new Array[AnyRef](4)
      if (((branch0 - 1) & branch1) == 0) {
        slots(0) = key0.asInstanceOf[AnyRef]
        slots(1) = key1.asInstanceOf[AnyRef]
        slots(2) = value1.asInstanceOf[AnyRef]
        slots(3) = value0.asInstanceOf[AnyRef]
      }
      else {
        slots(0) = key1.asInstanceOf[AnyRef]
        slots(1) = key0.asInstanceOf[AnyRef]
        slots(2) = value0.asInstanceOf[AnyRef]
        slots(3) = value1.asInstanceOf[AnyRef]
      }
      new HashMap(0, slotMap, slots)
    }
  }
  
  override def iterator: Iterator[(A, T)] = new HashMapIterator(this)
  
  protected override def foreach[U](f: ((A, T)) => U) {
    var i = 0
    var j = 0
    var treeMap = this.treeMap
    var leafMap = this.leafMap
    while ((treeMap | leafMap) != 0) {
      ((leafMap & 1 | (treeMap & 1) << 1): @switch) match {
        case VOID => ()
        case LEAF => f((keyAt(i), valueAt(j))); i += 1; j += 1
        case TREE => treeAt(i).foreach(f); i += 1
        case KNOT => knotAt(i).foreach(f); i += 1
      }
      treeMap >>>= 1
      leafMap >>>= 1
    }
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

/** A factory for [[HashMap hash maps]].
  * @group Containers */
object HashMap extends MapFactory[HashMap] {
  implicit override def Builder[A : TypeHint, T : TypeHint]
    : Builder[Any, (A, T)] { type State = HashMap[A, T] } =
    new HashMapBuilder
  
  private[this] val empty = new HashMap[Nothing, Nothing](0, 0, new Array[AnyRef](0))
  override def empty[A : TypeHint, T : TypeHint]: HashMap[A, T] = empty
  
  override def toString: String = "HashMap"
  
  private[containers] final val VOID = 0
  private[containers] final val LEAF = 1
  private[containers] final val TREE = 2
  private[containers] final val KNOT = 3
}

private[containers] final class HashMapIterator[+A, +T](
    nodes: Array[AnyRef], private[this] var depth: Int,
    stack: Array[Int], private[this] var stackPointer: Int)
  extends Iterator[(A, T)] {
  
  import HashMap.{VOID, LEAF, TREE, KNOT}
  
  def this(tree: HashMap[A, T]) = {
    this(new Array[AnyRef](7), 0, new Array[Int](28), 0)
    node = tree
    i = 0
    j = 0
    treeMap = tree.treeMap
    leafMap = tree.leafMap
  }
  
  private[this] def node: AnyRef = nodes(depth)
  private[this] def node_=(node: AnyRef): Unit = nodes(depth) = node
  
  private[this] def i: Int = stack(stackPointer)
  private[this] def i_=(index: Int): Unit = stack(stackPointer) = index
  
  private[this] def j: Int = stack(stackPointer + 1)
  private[this] def j_=(index: Int): Unit = stack(stackPointer + 1) = index
  
  private[this] def treeMap: Int = stack(stackPointer + 2)
  private[this] def treeMap_=(treeMap: Int): Unit = stack(stackPointer + 2) = treeMap
  
  private[this] def leafMap: Int = stack(stackPointer + 3)
  private[this] def leafMap_=(leafMap: Int): Unit = stack(stackPointer + 3) = leafMap
  
  private[this] def follow: Int = leafMap & 1 | (treeMap & 1) << 1
  
  private[this] def push(tree: HashMap[A, T]) {
    depth += 1
    node = tree
    
    stackPointer += 4
    i = 0
    j = 0
    treeMap = tree.treeMap
    leafMap = tree.leafMap
  }
  
  private[this] def push(knot: ArrayMap[A, T]) {
    depth += 1
    node = knot
    
    stackPointer += 4
    i = 0
  }
  
  private[this] def pop() {
    node = null
    depth -= 1
    
    i = 0
    j = 0
    treeMap = 0
    leafMap = 0
    stackPointer -= 4
    
    i += 1
    treeMap >>>= 1
    leafMap >>>= 1
  }
  
  @tailrec override def isEmpty: Boolean = node match {
    case node: HashMap[A, T] =>
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
    case node: ArrayMap[A, T] =>
      if (i < node.size) false
      else { pop(); isEmpty }
  }
  
  @tailrec override def head: (A, T) = node match {
    case node: HashMap[A, T] =>
      if ((treeMap | leafMap) != 0) (follow: @switch) match {
        case VOID =>
          treeMap >>>= 1
          leafMap >>>= 1
          head
        case LEAF => (node.keyAt(i), node.valueAt(j))
        case TREE =>
          push(node.treeAt(i))
          head
        case KNOT =>
          push(node.knotAt(i))
          head
      }
      else if (depth > 0) { pop(); head }
      else throw new NoSuchElementException("Head of empty iterator.")
    case node: ArrayMap[A, T] =>
      if (i < node.size) (node.keyAt(i), node.valueAt(i))
      else { pop(); head }
  }
  
  @tailrec override def step(): Unit = node match {
    case node: HashMap[A, T] =>
      val slotMap = treeMap | leafMap
      if (slotMap != 0) {
        if ((slotMap & 1) == 1) i += 1
        if ((leafMap & 1) == 1) j += 1
        treeMap >>>= 1
        leafMap >>>= 1
      }
      else if (depth > 0) { pop(); step() }
      else throw new UnsupportedOperationException("Empty iterator step.")
    case node: ArrayMap[A, T] =>
      if (i < node.size) i += 1
      else { pop(); step() }
  }
  
  override def dup: Iterator[(A, T)] =
    new HashMapIterator(nodes.clone, depth, stack.clone, stackPointer)
}

private[containers] final class HashMapBuilder[A, T] extends Builder[Any, (A, T)] {
  override type State = HashMap[A, T]
  
  private[this] var these: HashMap[A, T] = HashMap.empty[A, T]
  
  override def append(entry: (A, T)): Unit = these += (entry._1, entry._2)
  
  override def appendAll(elems: Enumerator[(A, T)]) {
    if (these.isEmpty && elems.isInstanceOf[HashMap[_, _]])
      these = elems.asInstanceOf[HashMap[A, T]]
    else super.appendAll(elems)
  }
  
  override def expect(count: Int): this.type = this
  
  override def state: HashMap[A, T] = these
  
  override def clear(): Unit = these = HashMap.empty[A, T]
}
