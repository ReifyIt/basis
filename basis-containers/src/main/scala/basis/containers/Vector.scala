/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.containers

import basis.collections._
import basis.runtime._

/** An indexed array-bitmapped trie.
  * 
  * ==Extensions==
  * $Extensions
  * $SequentialOps
  * 
  * @groupprio  Quantifying   -5
  * @groupprio  Indexing      -4
  * @groupprio  Iterating     -3
  * @groupprio  Traversing    -2
  * @groupprio  Classifying   -1
  * 
  * @define collection  vector
  */
sealed abstract class Vector[+A] extends Equals with Immutable with Family[Vector[A]] with Index[A] {
  /** Returns a copy of this $collection with the given element at the given index.
    * @group Indexing */
  def update[B >: A](index: Int, elem: B): Vector[B]
  
  protected override def stringPrefix: String = "Vector"
}

private[containers] final class Vector0 extends Vector[Nothing] {
  override def isEmpty: Boolean = true
  
  override def length: Int = 0
  
  override def apply(index: Int): Nothing =
    throw new IndexOutOfBoundsException(index.toString)
  
  override def update[B >: Nothing](index: Int, elem: B): Vector[B] =
    throw new IndexOutOfBoundsException(index.toString)
  
  override protected def foreach[U](f: Nothing => U): Unit = ()
}

private[containers] final class Vector1[+A](
    node1: Array[AnyRef],
    override val length: Int)
  extends Vector[A] {
  
  override def isEmpty: Boolean = false
  
  override def apply(index: Int): A = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    node1(index).asInstanceOf[A]
  }
  
  override def update[B >: A](index: Int, elem: B): Vector[B] = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newNode1 = new Array[AnyRef](node1.length)
    java.lang.System.arraycopy(node1, 0, newNode1, 0, node1.length)
    newNode1(index) = elem.asInstanceOf[AnyRef]
    new Vector1(newNode1, length)
  }
  
  override def iterator: Iterator[A] = new VectorIterator(node1, length)
  
  protected override def foreach[U](f: A => U): Unit = Vector.foreach1(node1)(f)
}

private[containers] final class Vector2[+A](
    node2: Array[Array[AnyRef]],
    override val length: Int)
  extends Vector[A] {
  
  override def isEmpty: Boolean = false
  
  override def apply(index: Int): A = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    (node2(index >>> 5 & 0x1F)
          (index       & 0x1F).asInstanceOf[A])
  }
  
  override def update[B >: A](index: Int, elem: B): Vector[B] = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newNode2 = new Array[Array[AnyRef]](node2.length)
    java.lang.System.arraycopy(node2, 0, newNode2, 0, node2.length)
    
    val node1 = newNode2((index >>> 5) & 0x1F)
    val newNode1 = new Array[AnyRef](node1.length)
    newNode2((index >>> 5) & 0x1F) = newNode1
    java.lang.System.arraycopy(node1, 0, newNode1, 0, node1.length)
    
    newNode1(index & 0x1F) = elem.asInstanceOf[AnyRef]
    new Vector2(newNode2, length)
  }
  
  override def iterator: Iterator[A] = new VectorIterator(node2, length)
  
  protected override def foreach[U](f: A => U): Unit = Vector.foreach2(node2)(f)
}

private[containers] final class Vector3[+A](
    node3: Array[Array[Array[AnyRef]]],
    override val length: Int)
  extends Vector[A] {
  
  override def isEmpty: Boolean = false
  
  override def apply(index: Int): A = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    (node3(index >>> 10 & 0x1F)
          (index >>>  5 & 0x1F)
          (index        & 0x1F).asInstanceOf[A])
  }
  
  override def update[B >: A](index: Int, elem: B): Vector[B] = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newNode3 = new Array[Array[Array[AnyRef]]](node3.length)
    java.lang.System.arraycopy(node3, 0, newNode3, 0, node3.length)
    
    val node2 = newNode3((index >>> 10) & 0x1F)
    val newNode2 = new Array[Array[AnyRef]](node2.length)
    newNode3((index >>> 10) & 0x1F) = newNode2
    java.lang.System.arraycopy(node2, 0, newNode2, 0, node2.length)
    
    val node1 = newNode2((index >>>  5) & 0x1F)
    val newNode1 = new Array[AnyRef](node1.length)
    newNode2((index >>>  5) & 0x1F) = newNode1
    java.lang.System.arraycopy(node1, 0, newNode1, 0, node1.length)
    
    newNode1(index & 0x1F) = elem.asInstanceOf[AnyRef]
    new Vector3(newNode3, length)
  }
  
  override def iterator: Iterator[A] = new VectorIterator(node3, length)
  
  protected override def foreach[U](f: A => U): Unit = Vector.foreach3(node3)(f)
}

private[containers] final class Vector4[+A](
    node4: Array[Array[Array[Array[AnyRef]]]],
    override val length: Int)
  extends Vector[A] {
  
  override def isEmpty: Boolean = false
  
  override def apply(index: Int): A = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    (node4(index >>> 15 & 0x1F)
          (index >>> 10 & 0x1F)
          (index >>>  5 & 0x1F)
          (index        & 0x1F).asInstanceOf[A])
  }
  
  override def update[B >: A](index: Int, elem: B): Vector[B] = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newNode4 = new Array[Array[Array[Array[AnyRef]]]](node4.length)
    java.lang.System.arraycopy(node4, 0, newNode4, 0, node4.length)
    
    val node3 = newNode4((index >>> 15) & 0x1F)
    val newNode3 = new Array[Array[Array[AnyRef]]](node3.length)
    newNode4((index >>> 15) & 0x1F) = newNode3
    java.lang.System.arraycopy(node3, 0, newNode3, 0, node3.length)
    
    val node2 = newNode3((index >>> 10) & 0x1F)
    val newNode2 = new Array[Array[AnyRef]](node2.length)
    newNode3((index >>> 10) & 0x1F) = newNode2
    java.lang.System.arraycopy(node2, 0, newNode2, 0, node2.length)
    
    val node1 = newNode2((index >>>  5) & 0x1F)
    val newNode1 = new Array[AnyRef](node1.length)
    newNode2((index >>>  5) & 0x1F) = newNode1
    java.lang.System.arraycopy(node1, 0, newNode1, 0, node1.length)
    
    newNode1(index & 0x1F) = elem.asInstanceOf[AnyRef]
    new Vector4(newNode4, length)
  }
  
  override def iterator: Iterator[A] = new VectorIterator(node4, length)
  
  protected override def foreach[U](f: A => U): Unit = Vector.foreach4(node4)(f)
}

private[containers] final class Vector5[+A](
    node5: Array[Array[Array[Array[Array[AnyRef]]]]],
    override val length: Int)
  extends Vector[A] {
  
  override def isEmpty: Boolean = false
  
  override def apply(index: Int): A = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    (node5(index >>> 20 & 0x1F)
          (index >>> 15 & 0x1F)
          (index >>> 10 & 0x1F)
          (index >>>  5 & 0x1F)
          (index        & 0x1F).asInstanceOf[A])
  }
  
  override def update[B >: A](index: Int, elem: B): Vector[B] = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newNode5 = new Array[Array[Array[Array[Array[AnyRef]]]]](node5.length)
    java.lang.System.arraycopy(node5, 0, newNode5, 0, node5.length)
    
    val node4 = newNode5((index >>> 20) & 0x1F)
    val newNode4 = new Array[Array[Array[Array[AnyRef]]]](node4.length)
    newNode5((index >>> 20) & 0x1F) = newNode4
    java.lang.System.arraycopy(node4, 0, newNode4, 0, node4.length)
    
    val node3 = newNode4((index >>> 15) & 0x1F)
    val newNode3 = new Array[Array[Array[AnyRef]]](node3.length)
    newNode4((index >>> 15) & 0x1F) = newNode3
    java.lang.System.arraycopy(node3, 0, newNode3, 0, node3.length)
    
    val node2 = newNode3((index >>> 10) & 0x1F)
    val newNode2 = new Array[Array[AnyRef]](node2.length)
    newNode3((index >>> 10) & 0x1F) = newNode2
    java.lang.System.arraycopy(node2, 0, newNode2, 0, node2.length)
    
    val node1 = newNode2((index >>>  5) & 0x1F)
    val newNode1 = new Array[AnyRef](node1.length)
    newNode2((index >>>  5) & 0x1F) = newNode1
    java.lang.System.arraycopy(node1, 0, newNode1, 0, node1.length)
    
    newNode1(index & 0x1F) = elem.asInstanceOf[AnyRef]
    new Vector5(newNode5, length)
  }
  
  override def iterator: Iterator[A] = new VectorIterator(node5, length)
  
  protected override def foreach[U](f: A => U): Unit = Vector.foreach5(node5)(f)
}

private[containers] final class Vector6[+A](
    node6: Array[Array[Array[Array[Array[Array[AnyRef]]]]]],
    override val length: Int)
  extends Vector[A] {
  
  override def isEmpty: Boolean = false
  
  override def apply(index: Int): A = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    (node6(index >>> 25 & 0x1F)
          (index >>> 20 & 0x1F)
          (index >>> 15 & 0x1F)
          (index >>> 10 & 0x1F)
          (index >>>  5 & 0x1F)
          (index        & 0x1F).asInstanceOf[A])
  }
  
  override def update[B >: A](index: Int, elem: B): Vector[B] = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newNode6 = new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](node6.length)
    java.lang.System.arraycopy(node6, 0, newNode6, 0, node6.length)
    val node5 = newNode6((index >>> 25) & 0x1F)
    val newNode5 = new Array[Array[Array[Array[Array[AnyRef]]]]](node5.length)
    java.lang.System.arraycopy(node5, 0, newNode5, 0, node5.length)
    val node4 = newNode5((index >>> 20) & 0x1F)
    val newNode4 = new Array[Array[Array[Array[AnyRef]]]](node4.length)
    java.lang.System.arraycopy(node4, 0, newNode4, 0, node4.length)
    val node3 = newNode4((index >>> 15) & 0x1F)
    val newNode3 = new Array[Array[Array[AnyRef]]](node3.length)
    java.lang.System.arraycopy(node3, 0, newNode3, 0, node3.length)
    val node2 = newNode3((index >>> 10) & 0x1F)
    val newNode2 = new Array[Array[AnyRef]](node2.length)
    java.lang.System.arraycopy(node2, 0, newNode2, 0, node2.length)
    val node1 = newNode2((index >>>  5) & 0x1F)
    val newNode1 = new Array[AnyRef](node1.length)
    java.lang.System.arraycopy(node1, 0, newNode1, 0, node1.length)
    newNode1(index & 0x1F) = elem.asInstanceOf[AnyRef]
    newNode2((index >>>  5) & 0x1F) = newNode1
    newNode3((index >>> 10) & 0x1F) = newNode2
    newNode4((index >>> 15) & 0x1F) = newNode3
    newNode5((index >>> 20) & 0x1F) = newNode4
    newNode6((index >>> 25) & 0x1F) = newNode5
    new Vector4(newNode4, length)
  }
  
  override def iterator: Iterator[A] = new VectorIterator(node6, length)
  
  protected override def foreach[U](f: A => U): Unit = Vector.foreach6(node6)(f)
}

/** A factory for [[Vector vectors]]. */
object Vector extends SeqFactory[Vector] {
  implicit override def Builder[A : TypeHint]
    : Builder[Any, A] { type State = Vector[A] } =
    new VectorBuilder
  
  private[this] val empty = new Vector0
  override def empty[A : TypeHint]: Vector[A] = empty
  
  private[containers] def foreach1[A, U](node1: Array[AnyRef])(f: A => U) {
    var i = 0
    val n = node1.length
    while (i < n) {
      f(node1(i).asInstanceOf[A])
      i += 1
    }
  }
  
  private[containers] def foreach2[A, U](node2: Array[Array[AnyRef]])(f: A => U) {
    var i = 0
    val n = node2.length
    while (i < n) {
      foreach1(node2(i))(f)
      i += 1
    }
  }
  
  private[containers] def foreach3[A, U](node3: Array[Array[Array[AnyRef]]])(f: A => U) {
    var i = 0
    val n = node3.length
    while (i < n) {
      foreach2(node3(i))(f)
      i += 1
    }
  }
  
  private[containers] def foreach4[A, U](node4: Array[Array[Array[Array[AnyRef]]]])(f: A => U) {
    var i = 0
    val n = node4.length
    while (i < n) {
      foreach3(node4(i))(f)
      i += 1
    }
  }
  
  private[containers] def foreach5[A, U](node5: Array[Array[Array[Array[Array[AnyRef]]]]])(f: A => U) {
    var i = 0
    val n = node5.length
    while (i < n) {
      foreach4(node5(i))(f)
      i += 1
    }
  }
  
  private[containers] def foreach6[A, U](node6: Array[Array[Array[Array[Array[Array[AnyRef]]]]]])(f: A => U) {
    var i = 0
    val n = node6.length
    while (i < n) {
      foreach5(node6(i))(f)
      i += 1
    }
  }
  
  override def toString: String = "Vector"
}

private[containers] final class VectorIterator[+A](
    private[this] val length: Int,
    private[this] var index: Int,
    private[this] var node1: Array[AnyRef],
    private[this] var node2: Array[Array[AnyRef]],
    private[this] var node3: Array[Array[Array[AnyRef]]],
    private[this] var node4: Array[Array[Array[Array[AnyRef]]]],
    private[this] var node5: Array[Array[Array[Array[Array[AnyRef]]]]],
    private[this] var node6: Array[Array[Array[Array[Array[Array[AnyRef]]]]]])
  extends Iterator[A] {
  
  def this(trie: AnyRef, length: Int) = {
    this(length, 0, null, null, null, null, null, null)
    if (length > (1 << 25)) {
      node6 = trie.asInstanceOf[Array[Array[Array[Array[Array[Array[AnyRef]]]]]]]
      node5 = node6(0)
      node4 = node5(0)
      node3 = node4(0)
      node2 = node3(0)
      node1 = node2(0)
    }
    else if (length > (1 << 20)) {
      node5 = trie.asInstanceOf[Array[Array[Array[Array[Array[AnyRef]]]]]]
      node4 = node5(0)
      node3 = node4(0)
      node2 = node3(0)
      node1 = node2(0)
    }
    else if (length > (1 << 15)) {
      node4 = trie.asInstanceOf[Array[Array[Array[Array[AnyRef]]]]]
      node3 = node4(0)
      node2 = node3(0)
      node1 = node2(0)
    }
    else if (length > (1 << 10)) {
      node3 = trie.asInstanceOf[Array[Array[Array[AnyRef]]]]
      node2 = node3(0)
      node1 = node2(0)
    }
    else if (length > (1 << 5)) {
      node2 = trie.asInstanceOf[Array[Array[AnyRef]]]
      node1 = node2(0)
    }
    else if (length > 0) {
      node1 = trie.asInstanceOf[Array[AnyRef]]
    }
    else throw new AssertionError
  }
  
  override def isEmpty: Boolean = index >= length
  
  override def head: A = {
    if (index >= length) throw new NoSuchElementException("Head of empty iterator.")
    node1(index & 0x1F).asInstanceOf[A]
  }
  
  override def step() {
    if (index >= length) throw new UnsupportedOperationException("Empty iterator step.")
    val diff = index ^ (index + 1)
    index += 1
    if (diff < length) {
      if (diff >= (1 << 25)) node5 = node6(index >>> 25 & 0x1F)
      if (diff >= (1 << 20)) node4 = node5(index >>> 20 & 0x1F)
      if (diff >= (1 << 15)) node3 = node4(index >>> 15 & 0x1F)
      if (diff >= (1 << 10)) node2 = node3(index >>> 10 & 0x1F)
      if (diff >= (1 <<  5)) node1 = node2(index >>>  5 & 0x1F)
    }
  }
  
  override def dup: Iterator[A] =
    new VectorIterator(length, index, node1, node2, node3, node4, node5, node6)
}

private[containers] final class VectorBuilder[A] extends Builder[Any, A] {
  override type State = Vector[A]
  
  private[this] var node1: Array[AnyRef] = _
  private[this] var node2: Array[Array[AnyRef]] = _
  private[this] var node3: Array[Array[Array[AnyRef]]] = _
  private[this] var node4: Array[Array[Array[Array[AnyRef]]]] = _
  private[this] var node5: Array[Array[Array[Array[Array[AnyRef]]]]] = _
  private[this] var node6: Array[Array[Array[Array[Array[Array[AnyRef]]]]]] = _
  
  private[this] var length: Int = 0
  
  override def append(elem: A) {
    if (length == 0) node1 = new Array[AnyRef](32)
    else {
      if ((length & 0x1FFFFFF) == 0) {
        if (length == 1 << 25) {
          node6 = new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](32)
          node6(0) = node5
        }
        node5 = new Array[Array[Array[Array[Array[AnyRef]]]]](32)
        node6(length >>> 25 & 0x1F) = node5
      }
      if ((length & 0xFFFFF) == 0) {
        if (length == 1 << 20) {
          node5 = new Array[Array[Array[Array[Array[AnyRef]]]]](32)
          node5(0) = node4
        }
        node4 = new Array[Array[Array[Array[AnyRef]]]](32)
        node5(length >>> 20 & 0x1F) = node4
      }
      if ((length & 0x7FFF) == 0) {
        if (length == 1 << 15) {
          node4 = new Array[Array[Array[Array[AnyRef]]]](32)
          node4(0) = node3
        }
        node3 = new Array[Array[Array[AnyRef]]](32)
        node4(length >>> 15 & 0x1F) = node3
      }
      if ((length & 0x3FF) == 0) {
        if (length == 1 << 10) {
          node3 = new Array[Array[Array[AnyRef]]](32)
          node3(0) = node2
        }
        node2 = new Array[Array[AnyRef]](32)
        node3(length >>> 10 & 0x1F) = node2
      }
      if ((length & 0x1F) == 0) {
        if (length == 1 << 5) {
          node2 = new Array[Array[AnyRef]](32)
          node2(0) = node1
        }
        node1 = new Array[AnyRef](32)
        node2(length >>> 5 & 0x1F) = node1
      }
    }
    node1(length & 0x1F) = elem.asInstanceOf[AnyRef]
    length += 1
  }
  
  override def expect(count: Int): this.type = this
  
  override def state: Vector[A] = {
    if (length == 0) Vector.empty[A]
    else {
      val last = length - 1
      val node1 = new Array[AnyRef]((last & 0x1F) + 1)
      java.lang.System.arraycopy(this.node1, 0, node1, 0, node1.length)
      if (length <= (1 << 5)) new Vector1(node1, length)
      else {
        val node2 = new Array[Array[AnyRef]]((last >>> 5 & 0x1F) + 1)
        java.lang.System.arraycopy(this.node2, 0, node2, 0, node2.length - 1)
        node2(node2.length - 1) = node1
        if (length <= (1 << 10)) new Vector2(node2, length)
        else {
          val node3 = new Array[Array[Array[AnyRef]]]((last >>> 10 & 0x1F) + 1)
          java.lang.System.arraycopy(this.node3, 0, node3, 0, node3.length - 1)
          node3(node3.length - 1) = node2
          if (length <= (1 << 15)) new Vector3(node3, length)
          else {
            val node4 = new Array[Array[Array[Array[AnyRef]]]]((last >>> 15 & 0x1F) + 1)
            java.lang.System.arraycopy(this.node4, 0, node4, 0, node4.length - 1)
            node4(node4.length - 1) = node3
            if (length <= (1 << 20)) new Vector4(node4, length)
            else {
              val node5 = new Array[Array[Array[Array[Array[AnyRef]]]]]((last >>> 20 & 0x1F) + 1)
              java.lang.System.arraycopy(this.node5, 0, node5, 0, node5.length - 1)
              node5(node5.length - 1) = node4
              if (length <= (1 << 25)) new Vector5(node5, length)
              else {
                val node6 = new Array[Array[Array[Array[Array[Array[AnyRef]]]]]]((last >>> 25 & 0x1F) + 1)
                java.lang.System.arraycopy(this.node6, 0, node6, 0, node6.length - 1)
                node6(node6.length - 1) = node5
                new Vector6(node6, length)
              }
            }
          }
        }
      }
    }
  }
  
  override def clear() {
    node1 = null
    node2 = null
    node3 = null
    node4 = null
    node5 = null
    node6 = null
    length = 0
  }
}
