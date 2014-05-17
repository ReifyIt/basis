//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import basis._
import basis.collections.generic._

sealed abstract class Vector[+A]
  extends Equals
  with Immutable
  with Family[Vector[_]]
  with Compound[Vector[A]]
  with IndexedSeq[A] {

  /** Returns a copy of this $collection with the given element at the given index.
    * @group Indexing */
  def update[B >: A](index: Int, elem: B): Vector[B]

  /** Returns a copy of this $collection with the given element appended.
    * @group Inserting */
  def :+ [B >: A](elem: B): Vector[B]

  protected override def stringPrefix: String = "Vector"
}

private[collections] final class Vector0 extends Vector[Nothing] {
  override def length: Int = 0

  override def apply(index: Int): Nothing =
    throw new IndexOutOfBoundsException(index.toString)

  override def update[B](index: Int, elem: B): Vector[B] =
    throw new IndexOutOfBoundsException(index.toString)

  override def :+ [B](elem: B): Vector[B] = {
    val newNode1 = new Array[AnyRef](1)
    newNode1(0) = elem.asInstanceOf[AnyRef]
    new Vector1(newNode1, 1)
  }

  override def traverse(f: Nothing => Unit): Unit = ()

  override def iterator: Iterator[Nothing] = new VectorIterator(null, 0)

  override def segments: Iterator[Vector[Nothing]] = new VectorSplitter(null, 0)
}

private[collections] final class Vector1[+A](
    private[collections] val node1: Array[AnyRef],
    override val length: Int)
  extends Vector[A] {

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

  override def :+ [B >: A](elem: B): Vector[B] = {
    val length  = this.length
    val length1 = length & 0x1F

    val newNode1 = new Array[AnyRef](length1 + 1)
    newNode1(length1) = elem.asInstanceOf[AnyRef]

    if (length1 != 0) {
      java.lang.System.arraycopy(node1, 0, newNode1, 0, length1)
      new Vector1(newNode1, length + 1)
    }
    else {
      val newNode2 = new Array[Array[AnyRef]](2)
      newNode2(0) = node1
      newNode2(1) = newNode1
      new Vector2(newNode2, length + 1)
    }
  }

  override def traverse(f: A => Unit): Unit = Vector.traverse1(node1)(f)

  override def iterator: Iterator[A] = new VectorIterator(node1, length)

  override def segments: Iterator[Vector[A]] = new VectorSplitter(node1, length)
}

private[collections] final class Vector2[+A](
    private[collections] val node2: Array[Array[AnyRef]],
    override val length: Int)
  extends Vector[A] {

  override def apply(index: Int): A = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    (node2(index >>> 5 & 0x1F)
          (index       & 0x1F).asInstanceOf[A])
  }

  override def update[B >: A](index: Int, elem: B): Vector[B] = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
    val newNode2 = new Array[Array[AnyRef]](node2.length)
    java.lang.System.arraycopy(node2, 0, newNode2, 0, node2.length)

    val node1 = newNode2(index >>> 5 & 0x1F)
    val newNode1 = new Array[AnyRef](node1.length)
    newNode2(index >>> 5 & 0x1F) = newNode1
    java.lang.System.arraycopy(node1, 0, newNode1, 0, node1.length)

    newNode1(index & 0x1F) = elem.asInstanceOf[AnyRef]
    new Vector2(newNode2, length)
  }

  override def :+ [B >: A](elem: B): Vector[B] = {
    val length  = this.length
    val length1 = length       & 0x1F
    val length2 = length >>> 5 & 0x1F

    val newNode1 = new Array[AnyRef](length1 + 1)
    newNode1(length1) = elem.asInstanceOf[AnyRef]
    val newNode2 = new Array[Array[AnyRef]](length2 + 1)
    newNode2(length2) = newNode1

    if ((length & 0x3FF) != 0) {
      java.lang.System.arraycopy(node2, 0, newNode2, 0, length2)
      if (length1 != 0) {
        val node1 = node2(length2)
        java.lang.System.arraycopy(node1, 0, newNode1, 0, length1)
      }
      new Vector2(newNode2, length + 1)
    }
    else {
      val newNode3 = new Array[Array[Array[AnyRef]]](2)
      newNode3(0) = node2
      newNode3(1) = newNode2
      new Vector3(newNode3, length + 1)
    }
  }

  override def traverse(f: A => Unit): Unit = Vector.traverse2(node2)(f)

  override def iterator: Iterator[A] = new VectorIterator(node2, length)

  override def segments: Iterator[Vector[A]] = new VectorSplitter(node2, length)
}

private[collections] final class Vector3[+A](
    private[collections] val node3: Array[Array[Array[AnyRef]]],
    override val length: Int)
  extends Vector[A] {

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

    val node2 = newNode3(index >>> 10 & 0x1F)
    val newNode2 = new Array[Array[AnyRef]](node2.length)
    newNode3(index >>> 10 & 0x1F) = newNode2
    java.lang.System.arraycopy(node2, 0, newNode2, 0, node2.length)

    val node1 = newNode2(index >>>  5 & 0x1F)
    val newNode1 = new Array[AnyRef](node1.length)
    newNode2(index >>>  5 & 0x1F) = newNode1
    java.lang.System.arraycopy(node1, 0, newNode1, 0, node1.length)

    newNode1(index & 0x1F) = elem.asInstanceOf[AnyRef]
    new Vector3(newNode3, length)
  }

  override def :+ [B >: A](elem: B): Vector[B] = {
    val length  = this.length
    val length1 = length        & 0x1F
    val length2 = length >>>  5 & 0x1F
    val length3 = length >>> 10 & 0x1F

    val newNode1 = new Array[AnyRef](length1 + 1)
    newNode1(length1) = elem.asInstanceOf[AnyRef]
    val newNode2 = new Array[Array[AnyRef]](length2 + 1)
    newNode2(length2) = newNode1
    val newNode3 = new Array[Array[Array[AnyRef]]](length3 + 1)
    newNode3(length3) = newNode2

    if ((length & 0x7FFF) != 0) {
      java.lang.System.arraycopy(node3, 0, newNode3, 0, length3)
      if ((length & 0x3FF) != 0) {
        val node2 = node3(length3)
        java.lang.System.arraycopy(node2, 0, newNode2, 0, length2)
        if (length1 != 0) {
          val node1 = node2(length2)
          java.lang.System.arraycopy(node1, 0, newNode1, 0, length1)
        }
      }
      new Vector3(newNode3, length + 1)
    }
    else {
      val newNode4 = new Array[Array[Array[Array[AnyRef]]]](2)
      newNode4(0) = node3
      newNode4(1) = newNode3
      new Vector4(newNode4, length + 1)
    }
  }

  override def traverse(f: A => Unit): Unit = Vector.traverse3(node3)(f)

  override def iterator: Iterator[A] = new VectorIterator(node3, length)

  override def segments: Iterator[Vector[A]] = new VectorSplitter(node3, length)
}

private[collections] final class Vector4[+A](
    private[collections] val node4: Array[Array[Array[Array[AnyRef]]]],
    override val length: Int)
  extends Vector[A] {

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

    val node3 = newNode4(index >>> 15 & 0x1F)
    val newNode3 = new Array[Array[Array[AnyRef]]](node3.length)
    newNode4(index >>> 15 & 0x1F) = newNode3
    java.lang.System.arraycopy(node3, 0, newNode3, 0, node3.length)

    val node2 = newNode3(index >>> 10 & 0x1F)
    val newNode2 = new Array[Array[AnyRef]](node2.length)
    newNode3(index >>> 10 & 0x1F) = newNode2
    java.lang.System.arraycopy(node2, 0, newNode2, 0, node2.length)

    val node1 = newNode2(index >>>  5 & 0x1F)
    val newNode1 = new Array[AnyRef](node1.length)
    newNode2(index >>>  5 & 0x1F) = newNode1
    java.lang.System.arraycopy(node1, 0, newNode1, 0, node1.length)

    newNode1(index & 0x1F) = elem.asInstanceOf[AnyRef]
    new Vector4(newNode4, length)
  }

  override def :+ [B >: A](elem: B): Vector[B] = {
    val length  = this.length
    val length1 = length        & 0x1F
    val length2 = length >>>  5 & 0x1F
    val length3 = length >>> 10 & 0x1F
    val length4 = length >>> 15 & 0x1F

    val newNode1 = new Array[AnyRef](length1 + 1)
    newNode1(length1) = elem.asInstanceOf[AnyRef]
    val newNode2 = new Array[Array[AnyRef]](length2 + 1)
    newNode2(length2) = newNode1
    val newNode3 = new Array[Array[Array[AnyRef]]](length3 + 1)
    newNode3(length3) = newNode2
    val newNode4 = new Array[Array[Array[Array[AnyRef]]]](length4 + 1)
    newNode4(length4) = newNode3

    if ((length & 0xFFFFF) != 0) {
      java.lang.System.arraycopy(node4, 0, newNode4, 0, length4)
      if ((length & 0x7FFF) != 0) {
        val node3 = node4(length4)
        java.lang.System.arraycopy(node3, 0, newNode3, 0, length3)
        if ((length & 0x3FF) != 0) {
          val node2 = node3(length3)
          java.lang.System.arraycopy(node2, 0, newNode2, 0, length2)
          if (length1 != 0) {
            val node1 = node2(length2)
            java.lang.System.arraycopy(node1, 0, newNode1, 0, length1)
          }
        }
      }
      new Vector4(newNode4, length + 1)
    }
    else {
      val newNode5 = new Array[Array[Array[Array[Array[AnyRef]]]]](2)
      newNode5(0) = node4
      newNode5(1) = newNode4
      new Vector5(newNode5, length + 1)
    }
  }

  override def traverse(f: A => Unit): Unit = Vector.traverse4(node4)(f)

  override def iterator: Iterator[A] = new VectorIterator(node4, length)

  override def segments: Iterator[Vector[A]] = new VectorSplitter(node4, length)
}

private[collections] final class Vector5[+A](
    private[collections] val node5: Array[Array[Array[Array[Array[AnyRef]]]]],
    override val length: Int)
  extends Vector[A] {

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

    val node4 = newNode5(index >>> 20 & 0x1F)
    val newNode4 = new Array[Array[Array[Array[AnyRef]]]](node4.length)
    newNode5(index >>> 20 & 0x1F) = newNode4
    java.lang.System.arraycopy(node4, 0, newNode4, 0, node4.length)

    val node3 = newNode4(index >>> 15 & 0x1F)
    val newNode3 = new Array[Array[Array[AnyRef]]](node3.length)
    newNode4(index >>> 15 & 0x1F) = newNode3
    java.lang.System.arraycopy(node3, 0, newNode3, 0, node3.length)

    val node2 = newNode3(index >>> 10 & 0x1F)
    val newNode2 = new Array[Array[AnyRef]](node2.length)
    newNode3(index >>> 10 & 0x1F) = newNode2
    java.lang.System.arraycopy(node2, 0, newNode2, 0, node2.length)

    val node1 = newNode2(index >>>  5 & 0x1F)
    val newNode1 = new Array[AnyRef](node1.length)
    newNode2(index >>>  5 & 0x1F) = newNode1
    java.lang.System.arraycopy(node1, 0, newNode1, 0, node1.length)

    newNode1(index & 0x1F) = elem.asInstanceOf[AnyRef]
    new Vector5(newNode5, length)
  }

  override def :+ [B >: A](elem: B): Vector[B] = {
    val length  = this.length
    val length1 = length        & 0x1F
    val length2 = length >>>  5 & 0x1F
    val length3 = length >>> 10 & 0x1F
    val length4 = length >>> 15 & 0x1F
    val length5 = length >>> 20 & 0x1F

    val newNode1 = new Array[AnyRef](length1 + 1)
    newNode1(length1) = elem.asInstanceOf[AnyRef]
    val newNode2 = new Array[Array[AnyRef]](length2 + 1)
    newNode2(length2) = newNode1
    val newNode3 = new Array[Array[Array[AnyRef]]](length3 + 1)
    newNode3(length3) = newNode2
    val newNode4 = new Array[Array[Array[Array[AnyRef]]]](length4 + 1)
    newNode4(length4) = newNode3
    val newNode5 = new Array[Array[Array[Array[Array[AnyRef]]]]](length5 + 1)
    newNode5(length5) = newNode4

    if ((length & 0x1FFFFFF) != 0) {
      java.lang.System.arraycopy(node5, 0, newNode5, 0, length5)
      if ((length & 0xFFFFF) != 0) {
        val node4 = node5(length5)
        java.lang.System.arraycopy(node4, 0, newNode4, 0, length4)
        if ((length & 0x7FFF) != 0) {
          val node3 = node4(length4)
          java.lang.System.arraycopy(node3, 0, newNode3, 0, length3)
          if ((length & 0x3FF) != 0) {
            val node2 = node3(length3)
            java.lang.System.arraycopy(node2, 0, newNode2, 0, length2)
            if (length1 != 0) {
              val node1 = node2(length2)
              java.lang.System.arraycopy(node1, 0, newNode1, 0, length1)
            }
          }
        }
      }
      new Vector5(newNode5, length + 1)
    }
    else {
      val newNode6 = new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](2)
      newNode6(0) = node5
      newNode6(1) = newNode5
      new Vector6(newNode6, length + 1)
    }
  }

  override def traverse(f: A => Unit): Unit = Vector.traverse5(node5)(f)

  override def iterator: Iterator[A] = new VectorIterator(node5, length)

  override def segments: Iterator[Vector[A]] = new VectorSplitter(node5, length)
}

private[collections] final class Vector6[+A](
    private[collections] val node6: Array[Array[Array[Array[Array[Array[AnyRef]]]]]],
    override val length: Int)
  extends Vector[A] {

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

    val node5 = newNode6(index >>> 25 & 0x1F)
    val newNode5 = new Array[Array[Array[Array[Array[AnyRef]]]]](node5.length)
    newNode6(index >>> 25 & 0x1F) = newNode5
    java.lang.System.arraycopy(node5, 0, newNode5, 0, node5.length)

    val node4 = newNode5(index >>> 20 & 0x1F)
    val newNode4 = new Array[Array[Array[Array[AnyRef]]]](node4.length)
    newNode5(index >>> 20 & 0x1F) = newNode4
    java.lang.System.arraycopy(node4, 0, newNode4, 0, node4.length)

    val node3 = newNode4(index >>> 15 & 0x1F)
    val newNode3 = new Array[Array[Array[AnyRef]]](node3.length)
    newNode4(index >>> 15 & 0x1F) = newNode3
    java.lang.System.arraycopy(node3, 0, newNode3, 0, node3.length)

    val node2 = newNode3(index >>> 10 & 0x1F)
    val newNode2 = new Array[Array[AnyRef]](node2.length)
    newNode3(index >>> 10 & 0x1F) = newNode2
    java.lang.System.arraycopy(node2, 0, newNode2, 0, node2.length)

    val node1 = newNode2(index >>>  5 & 0x1F)
    val newNode1 = new Array[AnyRef](node1.length)
    newNode2(index >>>  5 & 0x1F) = newNode1
    java.lang.System.arraycopy(node1, 0, newNode1, 0, node1.length)

    newNode1(index & 0x1F) = elem.asInstanceOf[AnyRef]
    new Vector4(newNode4, length)
  }

  override def :+ [B >: A](elem: B): Vector[B] = {
    val length  = this.length
    val length1 = length        & 0x1F
    val length2 = length >>>  5 & 0x1F
    val length3 = length >>> 10 & 0x1F
    val length4 = length >>> 15 & 0x1F
    val length5 = length >>> 20 & 0x1F
    val length6 = length >>> 25 & 0x1F

    val newNode1 = new Array[AnyRef](length1 + 1)
    newNode1(length1) = elem.asInstanceOf[AnyRef]
    val newNode2 = new Array[Array[AnyRef]](length2 + 1)
    newNode2(length2) = newNode1
    val newNode3 = new Array[Array[Array[AnyRef]]](length3 + 1)
    newNode3(length3) = newNode2
    val newNode4 = new Array[Array[Array[Array[AnyRef]]]](length4 + 1)
    newNode4(length4) = newNode3
    val newNode5 = new Array[Array[Array[Array[Array[AnyRef]]]]](length5 + 1)
    newNode5(length5) = newNode4
    val newNode6 = new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](length6 + 1)
    newNode6(length6) = newNode5

    if ((length & 0x3FFFFFFF) != 0) {
      java.lang.System.arraycopy(node6, 0, newNode6, 0, length6)
      if ((length & 0x1FFFFFF) != 0) {
        val node5 = node6(length6)
        java.lang.System.arraycopy(node5, 0, newNode5, 0, length5)
        if ((length & 0xFFFFF) != 0) {
          val node4 = node5(length5)
          java.lang.System.arraycopy(node4, 0, newNode4, 0, length4)
          if ((length & 0x7FFF) != 0) {
            val node3 = node4(length4)
            java.lang.System.arraycopy(node3, 0, newNode3, 0, length3)
            if ((length & 0x3FF) != 0) {
              val node2 = node3(length3)
              java.lang.System.arraycopy(node2, 0, newNode2, 0, length2)
              if (length1 != 0) {
                val node1 = node2(length2)
                java.lang.System.arraycopy(node1, 0, newNode1, 0, length1)
              }
            }
          }
        }
      }
      new Vector6(newNode6, length + 1)
    }
    else throw new UnsupportedOperationException("maximum vector length exceeded")
  }

  override def traverse(f: A => Unit): Unit = Vector.traverse6(node6)(f)

  override def iterator: Iterator[A] = new VectorIterator(node6, length)

  override def segments: Iterator[Vector[A]] = new VectorSplitter(node6, length)
}

object Vector extends SeqFactory[Vector] {
  private[collections] val Empty = new Vector0
  override def empty[A]: Vector[A] = Empty

  override def from[A](elems: Traverser[A]): Vector[A] = {
    if (elems.isInstanceOf[Vector[_]]) elems.asInstanceOf[Vector[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[Vector[A]] =
    new VectorBuilder[A]

  private[collections] def traverse1[A](node1: Array[AnyRef])(f: A => Unit): Unit = {
    var i = 0
    val n = node1.length
    while (i < n) {
      f(node1(i).asInstanceOf[A])
      i += 1
    }
  }

  private[collections] def traverse2[A](node2: Array[Array[AnyRef]])(f: A => Unit): Unit = {
    var i = 0
    val n = node2.length
    while (i < n) {
      traverse1(node2(i))(f)
      i += 1
    }
  }

  private[collections] def traverse3[A](node3: Array[Array[Array[AnyRef]]])(f: A => Unit): Unit = {
    var i = 0
    val n = node3.length
    while (i < n) {
      traverse2(node3(i))(f)
      i += 1
    }
  }

  private[collections] def traverse4[A](node4: Array[Array[Array[Array[AnyRef]]]])(f: A => Unit): Unit = {
    var i = 0
    val n = node4.length
    while (i < n) {
      traverse3(node4(i))(f)
      i += 1
    }
  }

  private[collections] def traverse5[A](node5: Array[Array[Array[Array[Array[AnyRef]]]]])(f: A => Unit): Unit = {
    var i = 0
    val n = node5.length
    while (i < n) {
      traverse4(node5(i))(f)
      i += 1
    }
  }

  private[collections] def traverse6[A](node6: Array[Array[Array[Array[Array[Array[AnyRef]]]]]])(f: A => Unit): Unit = {
    var i = 0
    val n = node6.length
    while (i < n) {
      traverse5(node6(i))(f)
      i += 1
    }
  }

  override def toString: String = "Vector"
}

private[collections] final class VectorIterator[+A](
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
  }

  override def isEmpty: Boolean = index >= length

  override def head: A = {
    if (index >= length) throw new NoSuchElementException("Head of empty iterator.")
    node1(index & 0x1F).asInstanceOf[A]
  }

  override def step(): Unit = {
    if (index >= length) throw new UnsupportedOperationException("Empty iterator step.")
    val diff = index ^ (index + 1)
    index += 1
    if (index < length && diff >= (1 << 5)) {
      if (diff >= (1 << 10)) {
        if (diff >= (1 << 15)) {
          if (diff >= (1 << 20)) {
            if (diff >= (1 << 25)) {
              node5 = node6(index >>> 25 & 0x1F)
            }
            node4 = node5(index >>> 20 & 0x1F)
          }
          node3 = node4(index >>> 15 & 0x1F)
        }
        node2 = node3(index >>> 10 & 0x1F)
      }
      node1 = node2(index >>> 5 & 0x1F)
    }
  }

  override def dup: Iterator[A] =
    new VectorIterator(length, index, node1, node2, node3, node4, node5, node6)
}

private[collections] final class VectorSplitter[+A](
    private[this] val length: Int,
    private[this] var index: Int,
    private[this] var node1: Array[AnyRef],
    private[this] var node2: Array[Array[AnyRef]],
    private[this] var node3: Array[Array[Array[AnyRef]]],
    private[this] var node4: Array[Array[Array[Array[AnyRef]]]],
    private[this] var node5: Array[Array[Array[Array[Array[AnyRef]]]]],
    private[this] var node6: Array[Array[Array[Array[Array[Array[AnyRef]]]]]])
  extends Iterator[Vector[A]] {

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
  }

  override def isEmpty: Boolean = index >= length

  override def head: Vector[A] = {
    if (index >= length) Iterator.empty.head
    new Vector1(node1, node1.length)
  }

  override def step(): Unit = {
    if (index >= length) Iterator.empty.step()
    val diff = index ^ (index + 32)
    index += 32
    if (index < length) {
      if (diff >= (1 << 10)) {
        if (diff >= (1 << 15)) {
          if (diff >= (1 << 20)) {
            if (diff >= (1 << 25)) {
              node5 = node6(index >>> 25 & 0x1F)
            }
            node4 = node5(index >>> 20 & 0x1F)
          }
          node3 = node4(index >>> 15 & 0x1F)
        }
        node2 = node3(index >>> 10 & 0x1F)
      }
      node1 = node2(index >>> 5 & 0x1F)
    }
  }

  override def dup: Iterator[Vector[A]] =
    new VectorSplitter(length, index, node1, node2, node3, node4, node5, node6)
}

private[collections] final class VectorBuilder[A] extends Builder[A] with State[Vector[A]] {
  private[this] var node1: Array[AnyRef] = _
  private[this] var node2: Array[Array[AnyRef]] = _
  private[this] var node3: Array[Array[Array[AnyRef]]] = _
  private[this] var node4: Array[Array[Array[Array[AnyRef]]]] = _
  private[this] var node5: Array[Array[Array[Array[Array[AnyRef]]]]] = _
  private[this] var node6: Array[Array[Array[Array[Array[Array[AnyRef]]]]]] = _

  private[this] var length: Int = 0

  private[this] var aliased: Int = 0

  private[this] def gotoNode1(): Unit = {
    if (length >= (1 << 5)) gotoNode2()
    if (aliased == 1 || (length & 0x1F) == 0) {
      val oldNode1 = node1
      node1 = new Array[AnyRef](32)
      if (aliased == 1) {
        java.lang.System.arraycopy(oldNode1, 0, node1, 0, oldNode1.length)
        aliased = 0
      }
      if (length == (1 << 5)) node2(0) = oldNode1
      if (length >= (1 << 5)) node2(length >>> 5 & 0x1F) = node1
    }
  }

  private[this] def gotoNode2(): Unit = {
    if (length >= (1 << 10)) gotoNode3()
    if (aliased == 2 || (length & 0x3FF) == 0 || length == (1 << 5)) {
      val oldNode2 = node2
      node2 = new Array[Array[AnyRef]](32)
      if (aliased == 2) {
        java.lang.System.arraycopy(oldNode2, 0, node2, 0, oldNode2.length)
        aliased = 1
      }
      if (length == (1 << 10)) node3(0) = oldNode2
      if (length >= (1 << 10)) node3(length >>> 10 & 0x1F) = node2
    }
  }

  private[this] def gotoNode3(): Unit = {
    if (length >= (1 << 15)) gotoNode4()
    if (aliased == 3 || (length & 0x7FFF) == 0 || length == (1 << 10)) {
      val oldNode3 = node3
      node3 = new Array[Array[Array[AnyRef]]](32)
      if (aliased == 3) {
        java.lang.System.arraycopy(oldNode3, 0, node3, 0, oldNode3.length)
        aliased = 2
      }
      if (length == (1 << 15)) node4(0) = oldNode3
      if (length >= (1 << 15)) node4(length >>> 15 & 0x1F) = node3
    }
  }

  private[this] def gotoNode4(): Unit = {
    if (length >= (1 << 20)) gotoNode5()
    if (aliased == 4 || (length & 0xFFFFF) == 0 || length == (1 << 15)) {
      val oldNode4 = node4
      node4 = new Array[Array[Array[Array[AnyRef]]]](32)
      if (aliased == 4) {
        java.lang.System.arraycopy(oldNode4, 0, node4, 0, oldNode4.length)
        aliased = 3
      }
      if (length == (1 << 20)) node5(0) = oldNode4
      if (length >= (1 << 20)) node5(length >>> 20 & 0x1F) = node4
    }
  }

  private[this] def gotoNode5(): Unit = {
    if (length >= (1 << 25)) gotoNode6()
    if (aliased == 5 || (length & 0x1FFFFFF) == 0 || length == (1 << 20)) {
      val oldNode5 = node5
      node5 = new Array[Array[Array[Array[Array[AnyRef]]]]](32)
      if (aliased == 5) {
        java.lang.System.arraycopy(oldNode5, 0, node5, 0, oldNode5.length)
        aliased = 4
      }
      if (length == (1 << 25)) node6(0) = oldNode5
      if (length >= (1 << 25)) node6(length >>> 25 & 0x1F) = node5
    }
  }

  private[this] def gotoNode6(): Unit = {
    if (length >= (1 << 30)) throw new UnsupportedOperationException("maximum length exceeded")
    if (aliased == 6 || length == (1 << 25)) {
      val oldNode6 = node6
      node6 = new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](32)
      if (aliased == 6) {
        java.lang.System.arraycopy(oldNode6, 0, node6, 0, oldNode6.length)
        aliased = 5
      }
    }
  }

  override def append(elem: A): Unit = {
    gotoNode1()
    node1(length & 0x1F) = elem.asInstanceOf[AnyRef]
    length += 1
  }

  override def appendAll(elems: Traverser[A]): Unit = elems match {
    case elems: Vector0 if length == 0 => ()
    case elems: Vector1[A] if (length & 0x1F) == 0 =>
      gotoNode2()
      if (length == (1 << 5)) node2(0) = node1
      node1 = elems.node1
      node2(length >>> 5 & 0x1F) = node1
      length += elems.length
      if (elems.length < (1 << 5)) aliased = 1
    case elems: Vector2[A] if (length & 0x3FF) == 0 =>
      gotoNode3()
      if (length == (1 << 10)) node3(0) = node2
      node2 = elems.node2
      node1 = node2(node2.length - 1)
      node3(length >>> 10 & 0x1F) = node2
      length += elems.length
      if (elems.length < (1 << 10)) aliased = 2
    case elems: Vector3[A] if (length & 0x7FFF) == 0 =>
      gotoNode4()
      if (length == (1 << 15)) node4(0) = node3
      node3 = elems.node3
      node2 = node3(node3.length - 1)
      node1 = node2(node2.length - 1)
      node4(length >>> 15 & 0x1F) = node3
      length += elems.length
      if (elems.length < (1 << 15)) aliased = 3
    case elems: Vector4[A] if (length & 0xFFFFF) == 0 =>
      gotoNode5()
      if (length == (1 << 20)) node5(0) = node4
      node4 = elems.node4
      node3 = node4(node4.length - 1)
      node2 = node3(node3.length - 1)
      node1 = node2(node2.length - 1)
      node5(length >>> 20 & 0x1F) = node4
      length += elems.length
      if (elems.length < (1 << 20)) aliased = 4
    case elems: Vector5[A] if (length & 0x1FFFFFF) == 0 =>
      gotoNode6()
      if (length == (1 << 25)) node6(0) = node5
      node5 = elems.node5
      node4 = node5(node5.length - 1)
      node3 = node4(node4.length - 1)
      node2 = node3(node3.length - 1)
      node1 = node2(node2.length - 1)
      node6(length >>> 25 & 0x1F) = node5
      length += elems.length
      if (elems.length < (1 << 25)) aliased = 5
    case elems: Vector6[A] if length == 0 =>
      node6 = elems.node6
      node5 = node6(node6.length - 1)
      node4 = node5(node5.length - 1)
      node3 = node4(node4.length - 1)
      node2 = node3(node3.length - 1)
      node1 = node2(node2.length - 1)
      length = elems.length
      if (elems.length < (1 << 30)) aliased = 6
    case _ => super.appendAll(elems)
  }

  private[this] def alias: Vector[A] = if (length == 0) Vector.empty else alias1

  private[this] def alias1: Vector[A] = {
    if ((length & 0x1F) != 0) {
      val last1 = (length - 1) & 0x1F
      val oldNode1 = node1
      node1 = new Array[AnyRef](last1 + 1)
      java.lang.System.arraycopy(oldNode1, 0, node1, 0, last1 + 1)
      aliased = 1
    }
    if (length <= (1 << 5)) new Vector1(node1, length) else alias2
  }

  private[this] def alias2: Vector[A] = {
    if (aliased == 1 || (length >>> 5 & 0x1F) != 0) {
      val last2 = (length - 1) >>> 5 & 0x1F
      val oldNode2 = node2
      node2 = new Array[Array[AnyRef]](last2 + 1)
      java.lang.System.arraycopy(oldNode2, 0, node2, 0, last2)
      node2(last2) = node1
      aliased = 2
    }
    if (length <= (1 << 10)) new Vector2(node2, length) else alias3
  }

  private[this] def alias3: Vector[A] = {
    if (aliased == 2 || (length >>> 10 & 0x1F) != 0) {
      val last3 = (length - 1) >>> 10 & 0x1F
      val oldNode3 = node3
      node3 = new Array[Array[Array[AnyRef]]](last3 + 1)
      java.lang.System.arraycopy(oldNode3, 0, node3, 0, last3)
      node3(last3) = node2
      aliased = 3
    }
    if (length <= (1 << 15)) new Vector3(node3, length) else alias4
  }

  private[this] def alias4: Vector[A] = {
    if (aliased == 3 || (length >>> 15 & 0x1F) != 0) {
      val last4 = (length - 1) >>> 15 & 0x1F
      val oldNode4 = node4
      node4 = new Array[Array[Array[Array[AnyRef]]]](last4 + 1)
      java.lang.System.arraycopy(oldNode4, 0, node4, 0, last4)
      node4(last4) = node3
      aliased = 4
    }
    if (length <= (1 << 20)) new Vector4(node4, length) else alias5
  }

  private[this] def alias5: Vector[A] = {
    if (aliased == 4 || (length >>> 20 & 0x1F) != 0) {
      val last5 = (length - 1) >>> 20 & 0x1F
      val oldNode5 = node5
      node5 = new Array[Array[Array[Array[Array[AnyRef]]]]](last5 + 1)
      java.lang.System.arraycopy(oldNode5, 0, node5, 0, last5)
      node5(last5) = node4
      aliased = 5
    }
    if (length <= (1 << 25)) new Vector5(node5, length) else alias6
  }

  private[this] def alias6: Vector[A] = {
    if (aliased == 5 || (length >>> 25 & 0x1F) != 0) {
      val last6 = (length - 1) >>> 25 & 0x1F
      val oldNode6 = node6
      node6 = new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](last6 + 1)
      java.lang.System.arraycopy(oldNode6, 0, node6, 0, last6)
      node6(last6) = node5
      aliased = 6
    }
    new Vector6(node6, length)
  }

  override def state: Vector[A] = alias

  override def clear(): Unit = {
    node1 = null
    node2 = null
    node3 = null
    node4 = null
    node5 = null
    node6 = null
    length = 0
    aliased = 0
  }

  override def expect(count: Int): this.type = this

  override def toString: String = "Vector"+"."+"Builder"
}
