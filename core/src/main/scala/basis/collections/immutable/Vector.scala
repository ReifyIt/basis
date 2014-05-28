//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import basis._
import scala.annotation.unchecked._

final class Vector[+A] private[collections] (
    private[collections] val prefix: Array[AnyRef],
    private[collections] var branch: Vector[Vector[A @uncheckedVariance]],
    private[collections] val suffix: Array[AnyRef],
    override val length: Int)
  extends Equals
  with Immutable
  with Family[Vector[_]]
  with Deque[A]
  with IndexedSeq[A] {

  private[collections] def this(infix: Array[AnyRef]) =
    this(infix, Vector.empty[Vector[A]], Vector.EmptyRefArray, infix.length)

  override def isEmpty: Boolean = length == 0

  override def apply(index: Int): A = {
    val n = index - prefix.length
    if (n < 0) prefix(index).asInstanceOf[A]
    else {
      val j = n - (branch.length << 5)
      if (j < 0) branch(n >> 5)(n & 0x1F)
      else suffix(j).asInstanceOf[A]
    }
  }

  def update[B >: A](index: Int, elem: B): Vector[B] = {
    val a = prefix.length
    val n = index - a
    if (n < 0) {
      val newPrefix = new Array[AnyRef](a)
      System.arraycopy(prefix, 0, newPrefix, 0, a)
      newPrefix(index) = elem.asInstanceOf[AnyRef]
      new Vector[B](newPrefix, branch, suffix, length)
    }
    else {
      val j = n - (branch.length << 5)
      if (j < 0) new Vector[B](prefix, branch.update(n >> 5, branch(n >> 5).update(n & 0x1F, elem)), suffix, length)
      else {
        val b = suffix.length
        val newSuffix = new Array[AnyRef](b)
        System.arraycopy(suffix, 0, newSuffix, 0, b)
        newSuffix(j) = elem.asInstanceOf[AnyRef]
        new Vector[B](prefix, branch, newSuffix, length)
      }
    }
  }

  override def head: A = {
    if (length == 0) throw new NoSuchElementException
    prefix(0).asInstanceOf[A]
  }

  override def tail: Vector[A] = {
    if (length == 0) throw new UnsupportedOperationException("tail of empty vector")
    drop(1)
  }

  override def body: Vector[A] = {
    if (length == 0) throw new UnsupportedOperationException("body of empty vector")
    take(length - 1)
  }

  override def foot: A = {
    if (length == 0) throw new NoSuchElementException
    if (length <= 32) prefix(prefix.length - 1).asInstanceOf[A]
    else suffix(suffix.length - 1).asInstanceOf[A]
  }

  def drop(lower: Int): Vector[A] = {
    val n = lower - prefix.length
    val k = length - lower
    if (lower <= 0) this
    else if (lower >= length) Vector.empty[A]
    else if (n == 0) {
      if (branch.length > 0) new Vector[A](branch.head.prefix, branch.tail, suffix, k)
      else new Vector[A](suffix, Vector.empty[Vector[A]], Vector.EmptyRefArray, k)
    }
    else if (n < 0) {
      val newPrefix = new Array[AnyRef](-n)
      System.arraycopy(prefix, lower, newPrefix, 0, -n)
      new Vector[A](newPrefix, branch, suffix, k)
    }
    else {
      val j = n - (branch.length << 5)
      if (j < 0) {
        val split = branch.drop(n >> 5)
        val splitPrefix = split.head.prefix
        val newPrefix = new Array[AnyRef](splitPrefix.length - (n & 0x1F))
        System.arraycopy(splitPrefix, n & 0x1F, newPrefix, 0, newPrefix.length)
        new Vector[A](newPrefix, split.tail, suffix, k)
      }
      else {
        val newPrefix = new Array[AnyRef](k)
        System.arraycopy(suffix, j, newPrefix, 0, k)
        new Vector[A](newPrefix, Vector.empty[Vector[A]], Vector.EmptyRefArray, k)
      }
    }
  }

  def take(upper: Int): Vector[A] = {
    val n = upper - prefix.length
    if (upper <= 0) Vector.empty[A]
    else if (upper >= length) this
    else if (n == 0) new Vector[A](prefix, Vector.empty[Vector[A]], Vector.EmptyRefArray, upper)
    else if (n < 0) {
      val newPrefix = new Array[AnyRef](upper)
      System.arraycopy(prefix, 0, newPrefix, 0, upper)
      new Vector[A](newPrefix, Vector.empty[Vector[A]], Vector.EmptyRefArray, upper)
    }
    else {
      val j = n - (branch.length << 5)
      if (j == 0) {
        if (branch.length > 0) new Vector[A](prefix, branch.body, branch.foot.prefix, upper)
        else new Vector[A](suffix, Vector.empty[Vector[A]], Vector.EmptyRefArray, upper)
      }
      else if (j < 0) {
        val split = branch.take(((n + 0x1F) & 0xFFFFFFE0) >> 5)
        val splitSuffix = split.foot.prefix
        val newSuffix = new Array[AnyRef](((((n & 0x1F) ^ 0x1F) + 1) & 0x20) | (n & 0x1F))
        System.arraycopy(splitSuffix, 0, newSuffix, 0, newSuffix.length)
        new Vector[A](prefix, split.body, newSuffix, upper)
      }
      else {
        val newSuffix = new Array[AnyRef](j)
        System.arraycopy(suffix, 0, newSuffix, 0, j)
        new Vector[A](prefix, branch, newSuffix, upper)
      }
    }
  }

  override def :+ [B >: A](elem: B): Vector[B] = {
    val i = prefix.length
    val j = suffix.length
    val n = branch.length
    if (n == 0 && j == 0 && i < 32) {
      val newPrefix = new Array[AnyRef](i + 1)
      System.arraycopy(prefix, 0, newPrefix, 0, i)
      newPrefix(i) = elem.asInstanceOf[AnyRef]
      new Vector[B](newPrefix, branch, suffix, length + 1)
    }
    else if (n == 0 && i + j < 64) {
      // assume(i + j > 32)
      val newPrefix = new Array[AnyRef](32)
      System.arraycopy(prefix, 0, newPrefix, 0, i)
      System.arraycopy(suffix, 0, newPrefix, i, 32 - i)
      val newSuffix = new Array[AnyRef](i + j - 32 + 1)
      System.arraycopy(suffix, 32 - i, newSuffix, 0, i + j - 32)
      newSuffix(i + j - 32) = elem.asInstanceOf[AnyRef]
      new Vector[B](newPrefix, branch, newSuffix, length + 1)
    }
    else if (j < 32) {
      val newSuffix = new Array[AnyRef](j + 1)
      System.arraycopy(suffix, 0, newSuffix, 0, j)
      newSuffix(j) = elem.asInstanceOf[AnyRef]
      new Vector[B](prefix, branch, newSuffix, length + 1)
    }
    else {
      val newSuffix = new Array[AnyRef](1)
      newSuffix(0) = elem.asInstanceOf[AnyRef]
      new Vector[B](prefix, branch :+ new Vector[B](suffix), newSuffix, length + 1)
    }
  }

  override def +: [B >: A](elem: B): Vector[B] = {
    val i = prefix.length
    val j = suffix.length
    val n = branch.length
    if (n == 0 && j == 0 && i < 32) {
      val newPrefix = new Array[AnyRef](1 + i)
      newPrefix(0) = elem.asInstanceOf[AnyRef]
      System.arraycopy(prefix, 0, newPrefix, 1, i)
      new Vector[B](newPrefix, branch, suffix, 1 + length)
    }
    else if (n == 0 && i + j < 64) {
      // assume(i + j > 32)
      val newPrefix = new Array[AnyRef](1 + i + j - 32)
      newPrefix(0) = elem.asInstanceOf[AnyRef]
      System.arraycopy(prefix, 0, newPrefix, 1, i + j - 32)
      val newSuffix = new Array[AnyRef](32)
      System.arraycopy(prefix, i + j - 32, newSuffix, 0, 32 - j)
      System.arraycopy(suffix, 0, newSuffix, 32 - j, j)
      new Vector[B](newPrefix, branch, newSuffix, 1 + length)
    }
    else if (i < 32) {
      val newPrefix = new Array[AnyRef](1 + i)
      newPrefix(0) = elem.asInstanceOf[AnyRef]
      System.arraycopy(prefix, 0, newPrefix, 1, i)
      new Vector[B](newPrefix, branch, suffix, 1 + length)
    }
    else {
      val newPrefix = new Array[AnyRef](1)
      newPrefix(0) = elem.asInstanceOf[AnyRef]
      new Vector[B](newPrefix, new Vector[B](prefix) +: branch, suffix, 1 + length)
    }
  }

  override def :: [B >: A](elem: B): Vector[B] = elem +: this

  override def traverse(f: A => Unit): Unit = Vector.traverse1(this)(f)

  protected override def stringPrefix: String = "Vector"
}

object Vector extends generic.SeqFactory[Vector] {
  private[collections] val EmptyRefArray: Array[AnyRef] = new Array[AnyRef](0)

  private[this] val Empty: Vector[Nothing] = {
    val empty = new Vector[Nothing](EmptyRefArray, null, EmptyRefArray, 0)
    empty.branch = empty
    empty
  }

  override def empty[A]: Vector[A] = Empty

  override def from[A](elems: Traverser[A]): Vector[A] = {
    if (elems.isInstanceOf[Vector[_]]) elems.asInstanceOf[Vector[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[Vector[A]] = new VectorBuilder[A]

  override def toString: String = "Vector"

  private[collections] def traverse1[A](xs: Vector[A])(f: A => Unit): Unit = {
    traverse1(xs.prefix)(f)
    traverse2(xs.branch)(f)
    traverse1(xs.suffix)(f)
  }

  private[collections] def traverse2[A](xs: Vector[Vector[A]])(f: A => Unit): Unit = {
    traverse2(xs.prefix)(f)
    traverse3(xs.branch)(f)
    traverse2(xs.suffix)(f)
  }

  private[collections] def traverse3[A](xs: Vector[Vector[Vector[A]]])(f: A => Unit): Unit = {
    traverse3(xs.prefix)(f)
    traverse4(xs.branch)(f)
    traverse3(xs.suffix)(f)
  }

  private[collections] def traverse4[A](xs: Vector[Vector[Vector[Vector[A]]]])(f: A => Unit): Unit = {
    traverse4(xs.prefix)(f)
    traverse5(xs.branch)(f)
    traverse4(xs.suffix)(f)
  }

  private[collections] def traverse5[A](xs: Vector[Vector[Vector[Vector[Vector[A]]]]])(f: A => Unit): Unit = {
    traverse5(xs.prefix)(f)
    val branch = xs.branch
    var i = 0
    val n = branch.length
    while (i < n) {
      traverse5(branch(i))(f)
      i += 1
    }
    traverse5(xs.suffix)(f)
  }

  private[collections] def traverse1[A](array: Array[AnyRef])(f: A => Unit): Unit = {
    var i = 0
    val n = array.length
    while (i < n) {
      f(array(i).asInstanceOf[A])
      i += 1
    }
  }

  private[collections] def traverse2[A](array: Array[AnyRef])(f: A => Unit): Unit = {
    var i = 0
    val n = array.length
    while (i < n) {
      traverse1(array(i).asInstanceOf[Vector[A]])(f)
      i += 1
    }
  }

  private[collections] def traverse3[A](array: Array[AnyRef])(f: A => Unit): Unit = {
    var i = 0
    val n = array.length
    while (i < n) {
      traverse2(array(i).asInstanceOf[Vector[Vector[A]]])(f)
      i += 1
    }
  }

  private[collections] def traverse4[A](array: Array[AnyRef])(f: A => Unit): Unit = {
    var i = 0
    val n = array.length
    while (i < n) {
      traverse3(array(i).asInstanceOf[Vector[Vector[Vector[A]]]])(f)
      i += 1
    }
  }

  private[collections] def traverse5[A](array: Array[AnyRef])(f: A => Unit): Unit = {
    var i = 0
    val n = array.length
    while (i < n) {
      traverse4(array(i).asInstanceOf[Vector[Vector[Vector[Vector[A]]]]])(f)
      i += 1
    }
  }
}

private[collections] final class VectorBuilder[A] extends Builder[A] with State[Vector[A]] {
  private[this] var these: Vector[A] = Vector.empty

  override def append(elem: A): Unit = these = these :+ elem

  override def appendAll(elems: Traverser[A]): Unit = {
    if (these.isEmpty && elems.isInstanceOf[Vector[_]])
      these = elems.asInstanceOf[Vector[A]]
    else super.appendAll(elems)
  }

  override def expect(count: Int): this.type = this

  override def state: Vector[A] = these

  override def clear(): Unit = these = Vector.empty[A]

  override def toString: String = "Vector"+"."+"Builder"
}
