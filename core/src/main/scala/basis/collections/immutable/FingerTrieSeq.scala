//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import basis._
import basis.util._

final class FingerTrieSeq[+A] private[collections] (
    private[collections] val prefix: Array[AnyRef],
    private[collections] var branch: FingerTrieSeq[Array[AnyRef]],
    private[collections] val suffix: Array[AnyRef],
    override val length: Int)
  extends Equals
  with Immutable
  with Family[FingerTrieSeq[_]]
  with Deque[A]
  with IndexedSeq[A] {

  private[collections] def this(infix: Array[AnyRef]) =
    this(infix, FingerTrieSeq.empty, FingerTrieSeq.EmptyRefArray, infix.length)

  override def isEmpty: Boolean = length == 0

  override def apply(index: Int): A = {
    val n = index - prefix.length
    if (n < 0) prefix(index).asInstanceOf[A]
    else {
      val j = n - (branch.length << 5)
      if (j < 0) branch(n >> 5)(n & 0x1F).asInstanceOf[A]
      else suffix(j).asInstanceOf[A]
    }
  }

  def update[B >: A](index: Int, elem: B): FingerTrieSeq[B] = {
    val a = prefix.length
    val n = index - a
    if (n < 0) {
      val newPrefix = new Array[AnyRef](a)
      System.arraycopy(prefix, 0, newPrefix, 0, a)
      newPrefix(index) = elem.asInstanceOf[AnyRef]
      new FingerTrieSeq(newPrefix, branch, suffix, length)
    }
    else {
      val j = n - (branch.length << 5)
      if (j < 0) {
        val oldInfix = branch(n >> 5)
        val newInfix = new Array[AnyRef](32)
        System.arraycopy(oldInfix, 0, newInfix, 0, 32)
        newInfix(n & 0x1F) = elem.asInstanceOf[AnyRef]
        new FingerTrieSeq(prefix, branch.update(n >> 5, newInfix), suffix, length)
      }
      else {
        val b = suffix.length
        val newSuffix = new Array[AnyRef](b)
        System.arraycopy(suffix, 0, newSuffix, 0, b)
        newSuffix(j) = elem.asInstanceOf[AnyRef]
        new FingerTrieSeq(prefix, branch, newSuffix, length)
      }
    }
  }

  override def head: A = {
    if (length == 0) throw new NoSuchElementException("head of empty finger trie")
    prefix(0).asInstanceOf[A]
  }

  override def tail: FingerTrieSeq[A] = {
    if (length == 0) throw new UnsupportedOperationException("tail of empty finger trie")
    drop(1)
  }

  override def body: FingerTrieSeq[A] = {
    if (length == 0) throw new UnsupportedOperationException("body of empty finger trie")
    take(length - 1)
  }

  override def foot: A = {
    if (length == 0) throw new NoSuchElementException("foot of empty finger trie")
    if (length <= 32) prefix(prefix.length - 1).asInstanceOf[A]
    else suffix(suffix.length - 1).asInstanceOf[A]
  }

  def drop(lower: Int): FingerTrieSeq[A] = {
    val n = lower - prefix.length
    val k = length - lower
    if (lower <= 0) this
    else if (lower >= length) FingerTrieSeq.empty
    else if (n == 0) {
      if (branch.length > 0) new FingerTrieSeq(branch.head, branch.tail, suffix, k)
      else new FingerTrieSeq(suffix, FingerTrieSeq.empty, FingerTrieSeq.EmptyRefArray, k)
    }
    else if (n < 0) {
      val newPrefix = new Array[AnyRef](-n)
      System.arraycopy(prefix, lower, newPrefix, 0, -n)
      new FingerTrieSeq(newPrefix, branch, suffix, k)
    }
    else {
      val j = n - (branch.length << 5)
      if (j < 0) {
        val split = branch.drop(n >> 5)
        val splitPrefix = split.head
        val newPrefix = new Array[AnyRef](splitPrefix.length - (n & 0x1F))
        System.arraycopy(splitPrefix, n & 0x1F, newPrefix, 0, newPrefix.length)
        new FingerTrieSeq(newPrefix, split.tail, suffix, k)
      }
      else {
        val newPrefix = new Array[AnyRef](k)
        System.arraycopy(suffix, j, newPrefix, 0, k)
        new FingerTrieSeq(newPrefix, FingerTrieSeq.empty, FingerTrieSeq.EmptyRefArray, k)
      }
    }
  }

  def take(upper: Int): FingerTrieSeq[A] = {
    val n = upper - prefix.length
    if (upper <= 0) FingerTrieSeq.empty
    else if (upper >= length) this
    else if (n == 0) new FingerTrieSeq(prefix, FingerTrieSeq.empty, FingerTrieSeq.EmptyRefArray, upper)
    else if (n < 0) {
      val newPrefix = new Array[AnyRef](upper)
      System.arraycopy(prefix, 0, newPrefix, 0, upper)
      new FingerTrieSeq(newPrefix, FingerTrieSeq.empty, FingerTrieSeq.EmptyRefArray, upper)
    }
    else {
      val j = n - (branch.length << 5)
      if (j == 0) {
        if (branch.length > 0) new FingerTrieSeq(prefix, branch.body, branch.foot, upper)
        else new FingerTrieSeq(suffix, FingerTrieSeq.empty, FingerTrieSeq.EmptyRefArray, upper)
      }
      else if (j < 0) {
        val split = branch.take(((n + 0x1F) & 0xFFFFFFE0) >> 5)
        val splitSuffix = split.foot
        val newSuffix = new Array[AnyRef](((((n & 0x1F) ^ 0x1F) + 1) & 0x20) | (n & 0x1F))
        System.arraycopy(splitSuffix, 0, newSuffix, 0, newSuffix.length)
        new FingerTrieSeq(prefix, split.body, newSuffix, upper)
      }
      else {
        val newSuffix = new Array[AnyRef](j)
        System.arraycopy(suffix, 0, newSuffix, 0, j)
        new FingerTrieSeq(prefix, branch, newSuffix, upper)
      }
    }
  }

  def slice(lower: Int, upper: Int): FingerTrieSeq[A] = {
    if (lower >= upper) FingerTrieSeq.empty
    else drop(lower).take(upper - (0 max lower))
  }

  override def :+ [B >: A](elem: B): FingerTrieSeq[B] = {
    val i = prefix.length
    val j = suffix.length
    val n = branch.length
    if (n == 0 && j == 0 && i < 32) {
      val newPrefix = new Array[AnyRef](i + 1)
      System.arraycopy(prefix, 0, newPrefix, 0, i)
      newPrefix(i) = elem.asInstanceOf[AnyRef]
      new FingerTrieSeq(newPrefix, branch, suffix, length + 1)
    }
    else if (n == 0 && i + j < 64) {
      // assume(i + j > 32)
      val newPrefix = new Array[AnyRef](32)
      System.arraycopy(prefix, 0, newPrefix, 0, i)
      System.arraycopy(suffix, 0, newPrefix, i, 32 - i)
      val newSuffix = new Array[AnyRef](i + j - 32 + 1)
      System.arraycopy(suffix, 32 - i, newSuffix, 0, i + j - 32)
      newSuffix(i + j - 32) = elem.asInstanceOf[AnyRef]
      new FingerTrieSeq(newPrefix, branch, newSuffix, length + 1)
    }
    else if (j < 32) {
      val newSuffix = new Array[AnyRef](j + 1)
      System.arraycopy(suffix, 0, newSuffix, 0, j)
      newSuffix(j) = elem.asInstanceOf[AnyRef]
      new FingerTrieSeq(prefix, branch, newSuffix, length + 1)
    }
    else {
      val newSuffix = new Array[AnyRef](1)
      newSuffix(0) = elem.asInstanceOf[AnyRef]
      new FingerTrieSeq(prefix, branch :+ suffix, newSuffix, length + 1)
    }
  }

  override def +: [B >: A](elem: B): FingerTrieSeq[B] = {
    val i = prefix.length
    val j = suffix.length
    val n = branch.length
    if (n == 0 && j == 0 && i < 32) {
      val newPrefix = new Array[AnyRef](1 + i)
      newPrefix(0) = elem.asInstanceOf[AnyRef]
      System.arraycopy(prefix, 0, newPrefix, 1, i)
      new FingerTrieSeq(newPrefix, branch, suffix, 1 + length)
    }
    else if (n == 0 && i + j < 64) {
      // assume(i + j > 32)
      val newPrefix = new Array[AnyRef](1 + i + j - 32)
      newPrefix(0) = elem.asInstanceOf[AnyRef]
      System.arraycopy(prefix, 0, newPrefix, 1, i + j - 32)
      val newSuffix = new Array[AnyRef](32)
      System.arraycopy(prefix, i + j - 32, newSuffix, 0, 32 - j)
      System.arraycopy(suffix, 0, newSuffix, 32 - j, j)
      new FingerTrieSeq(newPrefix, branch, newSuffix, 1 + length)
    }
    else if (i < 32) {
      val newPrefix = new Array[AnyRef](1 + i)
      newPrefix(0) = elem.asInstanceOf[AnyRef]
      System.arraycopy(prefix, 0, newPrefix, 1, i)
      new FingerTrieSeq(newPrefix, branch, suffix, 1 + length)
    }
    else {
      val newPrefix = new Array[AnyRef](1)
      newPrefix(0) = elem.asInstanceOf[AnyRef]
      new FingerTrieSeq(newPrefix, prefix +: branch, suffix, 1 + length)
    }
  }

  override def :: [B >: A](elem: B): FingerTrieSeq[B] = elem +: this

  override def iterator: Iterator[A] = new FingerTrieSeqIterator(this)

  override def traverse(f: A => Unit): Unit = traverse1(this)(f)

  private[this] def traverse1(xs: FingerTrieSeq[A])(f: A => Unit): Unit = {
    traverse1(xs.prefix)(f)
    traverse2(xs.branch)(f)
    traverse1(xs.suffix)(f)
  }

  private[this] def traverse2(xs: FingerTrieSeq[Array[AnyRef]])(f: A => Unit): Unit = {
    traverse2(xs.prefix)(f)
    traverse3(xs.branch.asInstanceOf[FingerTrieSeq[FingerTrieSeq[Array[AnyRef]]]])(f)
    traverse2(xs.suffix)(f)
  }

  private[this] def traverse3(xs: FingerTrieSeq[FingerTrieSeq[Array[AnyRef]]])(f: A => Unit): Unit = {
    traverse3(xs.prefix)(f)
    traverse4(xs.branch.asInstanceOf[FingerTrieSeq[FingerTrieSeq[FingerTrieSeq[Array[AnyRef]]]]])(f)
    traverse3(xs.suffix)(f)
  }

  private[this] def traverse4(xs: FingerTrieSeq[FingerTrieSeq[FingerTrieSeq[Array[AnyRef]]]])(f: A => Unit): Unit = {
    traverse4(xs.prefix)(f)
    traverse5(xs.branch.asInstanceOf[FingerTrieSeq[FingerTrieSeq[FingerTrieSeq[FingerTrieSeq[Array[AnyRef]]]]]])(f)
    traverse4(xs.suffix)(f)
  }

  private[this] def traverse5(xs: FingerTrieSeq[FingerTrieSeq[FingerTrieSeq[FingerTrieSeq[Array[AnyRef]]]]])(f: A => Unit): Unit = {
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

  private[this] def traverse1(array: Array[AnyRef])(f: A => Unit): Unit = {
    var i = 0
    val n = array.length
    while (i < n) {
      f(array(i).asInstanceOf[A])
      i += 1
    }
  }

  private[this] def traverse2(array: Array[AnyRef])(f: A => Unit): Unit = {
    var i = 0
    val n = array.length
    while (i < n) {
      traverse1(array(i).asInstanceOf[Array[AnyRef]])(f)
      i += 1
    }
  }

  private[this] def traverse3(array: Array[AnyRef])(f: A => Unit): Unit = {
    var i = 0
    val n = array.length
    while (i < n) {
      traverse2(array(i).asInstanceOf[Array[AnyRef]])(f)
      i += 1
    }
  }

  private[this] def traverse4(array: Array[AnyRef])(f: A => Unit): Unit = {
    var i = 0
    val n = array.length
    while (i < n) {
      traverse3(array(i).asInstanceOf[Array[AnyRef]])(f)
      i += 1
    }
  }

  private[this] def traverse5(array: Array[AnyRef])(f: A => Unit): Unit = {
    var i = 0
    val n = array.length
    while (i < n) {
      traverse4(array(i).asInstanceOf[Array[AnyRef]])(f)
      i += 1
    }
  }

  protected override def stringPrefix: String = "FingerTrieSeq"
}

object FingerTrieSeq extends generic.SeqFactory[FingerTrieSeq] {
  private[collections] val EmptyRefArray: Array[AnyRef] = new Array[AnyRef](0)

  private[this] val Empty: FingerTrieSeq[Nothing] = {
    val empty = new FingerTrieSeq[Nothing](EmptyRefArray, null, EmptyRefArray, 0)
    empty.branch = empty
    empty
  }

  override def empty[A]: FingerTrieSeq[A] = Empty

  override def from[A](elems: Traverser[A]): FingerTrieSeq[A] = {
    if (elems.isInstanceOf[FingerTrieSeq[_]]) elems.asInstanceOf[FingerTrieSeq[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[FingerTrieSeq[A]] = new FingerTrieSeqBuilder[A]

  override def toString: String = "FingerTrieSeq"
}

private[collections] final class FingerTrieSeqArraySegmenter(
    private[this] var prefix: Array[AnyRef],
    private[this] var branch: FingerTrieSeq[Array[AnyRef]],
    private[this] var suffix: Array[AnyRef],
    private[this] var inner: FingerTrieSeqArraySegmenter,
    private[this] var infix: Array[AnyRef],
    private[this] var phase: Int,
    private[this] var index: Int)
  extends Iterator[Array[AnyRef]] {

  def this(trie: FingerTrieSeq[_]) =
    this(trie.prefix, trie.branch, trie.suffix, null, null, if (!trie.isEmpty) 0 else 3, 0)

  override def isEmpty: Boolean = phase >= 3

  override def head: Array[AnyRef] = phase match {
    case 0 => prefix
    case 1 => infix(index).asInstanceOf[Array[AnyRef]]
    case 2 => suffix
    case _ => Iterator.empty.head
  }

  override def step(): Unit = phase match {
    case 0 =>
      prefix = null
      if (branch.length > 0) {
        inner = new FingerTrieSeqArraySegmenter(branch)
        infix = inner.head
        branch = null
        phase = 1
      }
      else if (suffix.length > 0) {
        branch = null
        phase = 2
      }
      else {
        branch = null
        suffix = null
        phase = 3
      }
    case 1 =>
      index += 1
      if (index >= infix.length) {
        inner.step()
        if (!inner.isEmpty) {
          infix = inner.head
          index = 0
        }
        else {
          inner = null
          phase = 2
        }
      }
    case 2 =>
      suffix = null
      phase = 3
    case _ =>
      Iterator.empty.step()
  }

  override def dup: FingerTrieSeqArraySegmenter =
    new FingerTrieSeqArraySegmenter(prefix, branch, suffix, if (inner != null) inner.dup else null, infix, phase, index)
}

private[collections] final class FingerTrieSeqIterator[+A](
    private[this] val segmenter: FingerTrieSeqArraySegmenter,
    private[this] var buffer: Array[AnyRef],
    private[this] var index: Int)
  extends Iterator[A] {

  def this(trie: FingerTrieSeq[A]) = {
    this(new FingerTrieSeqArraySegmenter(trie), null, 0)
    if (!segmenter.isEmpty) {
      buffer = segmenter.head
      segmenter.step()
    }
    else buffer = FingerTrieSeq.EmptyRefArray
  }

  override def isEmpty: Boolean = index >= buffer.length

  override def head: A = {
    if (index >= buffer.length) Iterator.empty.head
    buffer(index).asInstanceOf[A]
  }

  override def step(): Unit = {
    if (index >= buffer.length) Iterator.empty.step()
    index += 1
    if (index >= buffer.length) {
      if (!segmenter.isEmpty) {
        buffer = segmenter.head
        segmenter.step()
      }
      else buffer = FingerTrieSeq.EmptyRefArray
      index = 0
    }
  }

  override def dup: Iterator[A] = new FingerTrieSeqIterator(segmenter.dup, buffer, index)
}

private[collections] final class FingerTrieSeqBuilder[A](
    private[this] var prefix: Array[AnyRef],
    private[this] var branch: FingerTrieSeqBuilder[Array[AnyRef]],
    private[this] var buffer: Array[AnyRef],
    private[this] var length: Int)
  extends Builder[A] with State[FingerTrieSeq[A]] {

  def this(that: FingerTrieSeq[A]) =
    this(if (that.length > 32) that.prefix else null,
         if (that.length > 64) new FingerTrieSeqBuilder(that.branch) else null,
         if (that.length > 32) that.suffix else if (that.length > 0) that.prefix else null,
         that.length)

  def this() = this(null, null, null, 0)

  private[this] def skew: Int = (if (prefix != null) length - prefix.length else length) & 0x1F

  override def append(elem: A): Unit = {
    val offset = skew
    if (offset == 0) {
      if (buffer != null) {
        if (prefix == null) prefix = buffer
        else {
          if (branch == null) branch = new FingerTrieSeqBuilder[Array[AnyRef]]
          branch.append(buffer)
        }
      }
      buffer = new Array[AnyRef](32)
    }
    else if (buffer.length < 32) {
      val newBuffer = new Array[AnyRef](32)
      System.arraycopy(buffer, 0, newBuffer, 0, offset)
      buffer = newBuffer
    }
    buffer(offset) = elem.asInstanceOf[AnyRef]
    length += 1
  }

  override def appendAll(elems: Traverser[A]): Unit = {
    if (elems.isInstanceOf[FingerTrieSeq[_]]) {
      val that = elems.asInstanceOf[FingerTrieSeq[A]]
      val offset = skew
      if (that.length == 0) ()
      else if (length == 0) {
        if (that.length > 32) {
          prefix = that.prefix
          if (that.length > 64) branch = new FingerTrieSeqBuilder(that.branch)
          buffer = that.suffix
        }
        else buffer = that.prefix
        length = that.length
      }
      else if (((offset + that.prefix.length) & 0x1F) == 0) {
        if (buffer.length < 32) {
          val newBuffer = new Array[AnyRef](32)
          System.arraycopy(buffer, 0, newBuffer, 0, offset)
          buffer = newBuffer
        }
        if (offset > 0) System.arraycopy(that.prefix, 0, buffer, offset, 32 - offset)
        else {
          if (prefix == null) prefix = buffer
          else {
            if (branch == null) branch = new FingerTrieSeqBuilder[Array[AnyRef]]
            branch.append(buffer)
          }
          buffer = that.prefix
        }
        if (that.suffix.length > 0) {
          if (branch == null) branch = new FingerTrieSeqBuilder[Array[AnyRef]]
          branch.append(buffer)
          branch.appendAll(that.branch)
          buffer = that.suffix
        }
        length += that.length
      }
      else super.appendAll(elems)
    }
    else super.appendAll(elems)
  }

  override def expect(count: Int): this.type = this

  override def state: FingerTrieSeq[A] = {
    if (length == 0) FingerTrieSeq.empty
    else {
      val offset = skew
      if (offset != 0 && offset != buffer.length) {
        val suffix = new Array[AnyRef](offset)
        System.arraycopy(buffer, 0, suffix, 0, offset)
        buffer = suffix
      }
      if (prefix == null) new FingerTrieSeq[A](buffer, FingerTrieSeq.empty, FingerTrieSeq.EmptyRefArray, length)
      else if (branch == null) new FingerTrieSeq[A](prefix, FingerTrieSeq.empty, buffer, length)
      else new FingerTrieSeq[A](prefix, branch.state, buffer, length)
    }
  }

  override def clear(): Unit = {
    prefix = null
    branch = null
    buffer = null
    length = 0
  }

  override def toString: String = "FingerTrieSeq"+"."+"Builder"
}
