//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import basis._
import basis.collections.generic._
import basis.util._
import scala.annotation._
import scala.annotation.unchecked._

final class HashTrieMap[+A, +T] private[collections] (
    private[collections] val treeMap: Int,
    private[collections] val leafMap: Int,
    slots: Array[AnyRef])
  extends Equals with Immutable with Family[HashTrieMap[_, _]] with Submap[A, T] { self =>

  import HashTrieMap.{ VOID, LEAF, TREE, KNOT }

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

  override def + [B >: A, U >: T](key: B, value: U): HashTrieMap[B, U] = update(key, key.##, value, 0)

  override def - (key: A @uncheckedVariance): HashTrieMap[A, T] = remove(key, key.##, 0)

  def keys: Set[A] = new Keys

  def values: Container[T] = new Values

  private def slotMap: Int = treeMap | leafMap

  private def choose(hash: Int, shift: Int): Int = 1 << ((hash >>> shift) & 0x1F)

  private def select(branch: Int): Int = (slotMap & (branch - 1)).countSetBits

  private def lookup(branch: Int): Int = (leafMap & (branch - 1)).countSetBits

  private def follow(branch: Int): Int =
    (if ((leafMap & branch) != 0) 1 else 0) | (if ((treeMap & branch) != 0) 2 else 0)

  private[collections] def keyAt(index: Int): A =
    slots(index).asInstanceOf[A]

  private def getKey(branch: Int): A =
    slots(select(branch)).asInstanceOf[A]

  private[collections] def valueAt(index: Int): T =
    slots(slots.length - index - 1).asInstanceOf[T]

  private def getValue(branch: Int): T =
    slots(slots.length - lookup(branch) - 1).asInstanceOf[T]

  private def setLeaf[B >: A, U >: T](branch: Int, key: B, value: U): this.type = {
    slots(select(branch)) = key.asInstanceOf[AnyRef]
    slots(slots.length - lookup(branch) - 1) = value.asInstanceOf[AnyRef]
    this
  }

  private[collections] def treeAt(index: Int): HashTrieMap[A, T] =
    slots(index).asInstanceOf[HashTrieMap[A, T]]

  private def getTree(branch: Int): HashTrieMap[A, T] =
    slots(select(branch)).asInstanceOf[HashTrieMap[A, T]]

  private def setTree[B >: A, U >: T](branch: Int, tree: HashTrieMap[B, U]): this.type = {
    slots(select(branch)) = tree
    this
  }

  private[collections] def knotAt(index: Int): ArrayMap[A, T] =
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

  private def remap(treeMap: Int, leafMap: Int): HashTrieMap[A, T] = {
    var oldLeafMap = this.leafMap
    var newLeafMap = leafMap
    var oldSlotMap = this.treeMap | this.leafMap
    var newSlotMap = treeMap | leafMap
    if (oldLeafMap == newLeafMap && oldSlotMap == newSlotMap)
      new HashTrieMap(treeMap, leafMap, this.slots.clone)
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
      new HashTrieMap(treeMap, leafMap, slots)
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
      case LEAF => if (key == getKey(branch)) Bind(getValue(branch)) else Trap
      case TREE => getTree(branch).get(key, keyHash, shift + 5)
      case KNOT => getKnot(branch).get(key)
    }
  }

  private def update[B >: A, U >: T](key: B, keyHash: Int, value: U, shift: Int): HashTrieMap[B, U] = {
    val branch = choose(keyHash, shift)
    (follow(branch): @switch) match {
      case VOID => remap(treeMap, leafMap | branch).setLeaf(branch, key, value)
      case LEAF =>
        val leaf = getKey(branch)
        val leafHash = leaf.##
        if (keyHash == leafHash && key == leaf) {
          if (value == getValue(branch)) this
          else remap(treeMap, leafMap).setLeaf(branch, key, value)
        }
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

  private def remove(key: A @uncheckedVariance, keyHash: Int, shift: Int): HashTrieMap[A, T] = {
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
    : HashTrieMap[B, U] = {
    // assume(hash0 != hash1)
    val branch0 = choose(hash0, shift)
    val branch1 = choose(hash1, shift)
    val slotMap = branch0 | branch1
    if (branch0 == branch1) {
      val slots = new Array[AnyRef](1)
      slots(0) = merge(key0, hash0, value0, key1, hash1, value1, shift + 5)
      new HashTrieMap(slotMap, 0, slots)
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
      new HashTrieMap(0, slotMap, slots)
    }
  }

  private[collections] def traverseKeys(f: A => Unit): Unit = {
    var i = 0
    var treeMap = this.treeMap
    var leafMap = this.leafMap
    while ((treeMap | leafMap) != 0) {
      ((leafMap & 1 | (treeMap & 1) << 1): @switch) match {
        case VOID => ()
        case LEAF => f(keyAt(i)); i += 1
        case TREE => treeAt(i) traverseKeys f; i += 1
        case KNOT => knotAt(i) traverseKeys f; i += 1
      }
      treeMap >>>= 1
      leafMap >>>= 1
    }
  }

  private[collections] def traverseValues(f: T => Unit): Unit = {
    var i = 0
    var j = 0
    var treeMap = this.treeMap
    var leafMap = this.leafMap
    while ((treeMap | leafMap) != 0) {
      ((leafMap & 1 | (treeMap & 1) << 1): @switch) match {
        case VOID => ()
        case LEAF => f(valueAt(j)); i += 1; j += 1
        case TREE => treeAt(i) traverseValues f; i += 1
        case KNOT => knotAt(i) traverseValues f; i += 1
      }
      treeMap >>>= 1
      leafMap >>>= 1
    }
  }

  override def traverse(f: (A, T) => Unit): Unit = {
    var i = 0
    var j = 0
    var treeMap = this.treeMap
    var leafMap = this.leafMap
    while ((treeMap | leafMap) != 0) {
      ((leafMap & 1 | (treeMap & 1) << 1): @switch) match {
        case VOID => ()
        case LEAF => f(keyAt(i), valueAt(j)); i += 1; j += 1
        case TREE => treeAt(i) traverse f; i += 1
        case KNOT => knotAt(i) traverse f; i += 1
      }
      treeMap >>>= 1
      leafMap >>>= 1
    }
  }

  override def traverse(f: ((A, T)) => Unit): Unit = {
    var i = 0
    var j = 0
    var treeMap = this.treeMap
    var leafMap = this.leafMap
    while ((treeMap | leafMap) != 0) {
      ((leafMap & 1 | (treeMap & 1) << 1): @switch) match {
        case VOID => ()
        case LEAF => f((keyAt(i), valueAt(j))); i += 1; j += 1
        case TREE => treeAt(i) traverse f; i += 1
        case KNOT => knotAt(i) traverse f; i += 1
      }
      treeMap >>>= 1
      leafMap >>>= 1
    }
  }

  override def iterator: Iterator[(A, T)] = new HashTrieMapEntryIterator(this)

  protected override def stringPrefix: String = "HashTrieMap"

  private final class Keys extends Set[A] {
    override def isEmpty: Boolean = self.isEmpty

    override def size: Int = self.size

    override def contains(elem: A @uncheckedVariance): Boolean = self.contains(elem)

    override def traverse(f: A => Unit): Unit = self.traverseKeys(f)

    override def iterator: Iterator[A] = new HashTrieMapKeyIterator(self)

    protected override def stringPrefix: String = "HashTrieMap"+"."+"Keys"
  }

  private final class Values extends Container[T] {
    override def traverse(f: T => Unit): Unit = self.traverseValues(f)

    override def iterator: Iterator[T] = new HashTrieMapValueIterator(self)

    protected override def stringPrefix: String = "HashTrieMap"+"."+"Values"
  }
}

object HashTrieMap extends MapFactory[HashTrieMap] {
  private[this] val Empty = new HashTrieMap[Nothing, Nothing](0, 0, new Array[AnyRef](0))
  override def empty[A, T]: HashTrieMap[A, T] = Empty

  override def from[A, T](elems: Traverser[(A, T)]): HashTrieMap[A, T] = {
    if (elems.isInstanceOf[HashTrieMap[_, _]]) elems.asInstanceOf[HashTrieMap[A, T]]
    else super.from(elems)
  }

  implicit override def Builder[A, T]: Builder[(A, T)] with State[HashTrieMap[A, T]] =
    new HashTrieMapBuilder[A, T]

  override def toString: String = "HashTrieMap"

  private[collections] final val VOID = 0
  private[collections] final val LEAF = 1
  private[collections] final val TREE = 2
  private[collections] final val KNOT = 3
}

private[collections] sealed abstract class HashTrieMapIterator[+A, +T] protected (
    protected[this] final val nodes: Array[AnyRef], protected[this] final var depth: Int,
    protected[this] final val stack: Array[Int], protected[this] final var stackPointer: Int) {

  import HashTrieMap.{ VOID, LEAF, TREE, KNOT }

  protected[this] final def init(tree: HashTrieMap[A, T]): Unit = {
    node = tree
    i = 0
    j = 0
    treeMap = tree.treeMap
    leafMap = tree.leafMap
  }

  protected[this] final def node: AnyRef = nodes(depth)
  protected[this] final def node_=(node: AnyRef): Unit = nodes(depth) = node

  protected[this] final def i: Int = stack(stackPointer)
  protected[this] final def i_=(index: Int): Unit = stack(stackPointer) = index

  protected[this] final def j: Int = stack(stackPointer + 1)
  protected[this] final def j_=(index: Int): Unit = stack(stackPointer + 1) = index

  protected[this] final def treeMap: Int = stack(stackPointer + 2)
  protected[this] final def treeMap_=(treeMap: Int): Unit = stack(stackPointer + 2) = treeMap

  protected[this] final def leafMap: Int = stack(stackPointer + 3)
  protected[this] final def leafMap_=(leafMap: Int): Unit = stack(stackPointer + 3) = leafMap

  protected[this] final def follow: Int = leafMap & 1 | (treeMap & 1) << 1

  protected[this] final def push(tree: HashTrieMap[A, T]): Unit = {
    depth += 1
    node = tree

    stackPointer += 4
    i = 0
    j = 0
    treeMap = tree.treeMap
    leafMap = tree.leafMap
  }

  protected[this] final def push(knot: ArrayMap[A, T]): Unit = {
    depth += 1
    node = knot

    stackPointer += 4
    i = 0
  }

  protected[this] final def pop(): Unit = {
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

  @tailrec final def isEmpty: Boolean = node match {
    case node: HashTrieMap[A, T] =>
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

  @tailrec final def entry: (A, T) = node match {
    case node: HashTrieMap[A, T] =>
      if ((treeMap | leafMap) != 0) (follow: @switch) match {
        case VOID =>
          treeMap >>>= 1
          leafMap >>>= 1
          entry
        case LEAF => (node.keyAt(i), node.valueAt(j))
        case TREE =>
          push(node.treeAt(i))
          entry
        case KNOT =>
          push(node.knotAt(i))
          entry
      }
      else if (depth > 0) { pop(); entry }
      else Iterator.empty.head
    case node: ArrayMap[A, T] =>
      if (i < node.size) (node.keyAt(i), node.valueAt(i))
      else { pop(); entry }
  }

  @tailrec final def key: A = node match {
    case node: HashTrieMap[A, T] =>
      if ((treeMap | leafMap) != 0) (follow: @switch) match {
        case VOID =>
          treeMap >>>= 1
          leafMap >>>= 1
          key
        case LEAF => node.keyAt(i)
        case TREE =>
          push(node.treeAt(i))
          key
        case KNOT =>
          push(node.knotAt(i))
          key
      }
      else if (depth > 0) { pop(); key }
      else Iterator.empty.head
    case node: ArrayMap[A, T] =>
      if (i < node.size) node.keyAt(i)
      else { pop(); key }
  }

  @tailrec final def value: T = node match {
    case node: HashTrieMap[A, T] =>
      if ((treeMap | leafMap) != 0) (follow: @switch) match {
        case VOID =>
          treeMap >>>= 1
          leafMap >>>= 1
          value
        case LEAF => node.valueAt(j)
        case TREE =>
          push(node.treeAt(i))
          value
        case KNOT =>
          push(node.knotAt(i))
          value
      }
      else if (depth > 0) { pop(); value }
      else Iterator.empty.head
    case node: ArrayMap[A, T] =>
      if (i < node.size) node.valueAt(i)
      else { pop(); value }
  }

  @tailrec final def step(): Unit = node match {
    case node: HashTrieMap[A, T] =>
      val slotMap = treeMap | leafMap
      if (slotMap != 0) {
        if ((slotMap & 1) == 1) i += 1
        if ((leafMap & 1) == 1) j += 1
        treeMap >>>= 1
        leafMap >>>= 1
      }
      else if (depth > 0) { pop(); step() }
      else Iterator.empty.step()
    case node: ArrayMap[A, T] =>
      if (i < node.size) i += 1
      else { pop(); step() }
  }
}

private[collections] final class HashTrieMapEntryIterator[+A, +T] private (
    _nodes: Array[AnyRef], _depth: Int,
    _stack: Array[Int], _stackPointer: Int)
  extends HashTrieMapIterator[A, T](_nodes, _depth, _stack, _stackPointer) with Iterator[(A, T)] {

  def this(tree: HashTrieMap[A, T]) = {
    this(new Array[AnyRef](7), 0, new Array[Int](28), 0)
    init(tree)
  }

  override def head: (A, T) = entry

  override def dup: Iterator[(A, T)] =
    new HashTrieMapEntryIterator(nodes.clone, depth, stack.clone, stackPointer)
}

private[collections] final class HashTrieMapKeyIterator[+A] private (
    _nodes: Array[AnyRef], _depth: Int,
    _stack: Array[Int], _stackPointer: Int)
  extends HashTrieMapIterator[A, Any](_nodes, _depth, _stack, _stackPointer) with Iterator[A] {

  def this(tree: HashTrieMap[A, Any]) = {
    this(new Array[AnyRef](7), 0, new Array[Int](28), 0)
    init(tree)
  }

  override def head: A = key

  override def dup: Iterator[A] =
    new HashTrieMapKeyIterator(nodes.clone, depth, stack.clone, stackPointer)
}

private[collections] final class HashTrieMapValueIterator[+T] private (
    _nodes: Array[AnyRef], _depth: Int,
    _stack: Array[Int], _stackPointer: Int)
  extends HashTrieMapIterator[Any, T](_nodes, _depth, _stack, _stackPointer) with Iterator[T] {

  def this(tree: HashTrieMap[Any, T]) = {
    this(new Array[AnyRef](7), 0, new Array[Int](28), 0)
    init(tree)
  }

  override def head: T = value

  override def dup: Iterator[T] =
    new HashTrieMapValueIterator(nodes.clone, depth, stack.clone, stackPointer)
}

private[collections] final class HashTrieMapBuilder[A, T] extends Builder[(A, T)] with State[HashTrieMap[A, T]] {
  private[this] var these: HashTrieMap[A, T] = HashTrieMap.empty[A, T]

  override def append(entry: (A, T)): Unit = these += (entry._1, entry._2)

  override def appendAll(elems: Traverser[(A, T)]): Unit = {
    if (these.isEmpty && elems.isInstanceOf[HashTrieMap[_, _]])
      these = elems.asInstanceOf[HashTrieMap[A, T]]
    else super.appendAll(elems)
  }

  override def expect(count: Int): this.type = this

  override def state: HashTrieMap[A, T] = these

  override def clear(): Unit = these = HashTrieMap.empty[A, T]

  override def toString: String = "HashTrieMap"+"."+"Builder"
}
