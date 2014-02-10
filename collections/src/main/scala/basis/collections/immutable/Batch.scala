//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import basis.collections.generic._
import scala.Predef.<:<

abstract class Batch[+A] private[collections]
  extends Equals
  with Immutable
  with Family[Batch[_]]
  with IndexedSeq[A]
  with Deque[A] {

  override def body: Batch[A]

  override def tail: Batch[A]

  /** Returns a copy of this $collection with the given element at the given index.
    * @group Indexing */
  def update[B >: A](index: Int, elem: B): Batch[B]

  /** Returns all but the first `lower` elements of this $collection.
    * @group Slicing */
  def drop(lower: Int): Batch[A]

  /** Returns the first `upper` elements of this $collection.
    * @group Slicing */
  def take(upper: Int): Batch[A]

  override def :+ [B >: A](elem: B): Batch[B]

  override def +: [B >: A](elem: B): Batch[B]

  override def :: [B >: A](elem: B): Batch[B] = elem +: this

  override def traverse(f: A => Unit): Unit

  /** Applies a side-effecting function to each nested element of this $collection.
    * @group Traversing */
  def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Batch[B]): Unit = {
    var i = 0
    val n = length
    while (i < n) {
      this(i) traverse f
      i += 1
    }
  }

  protected override def stringPrefix: String = "Batch"
}

object Batch extends generic.SeqFactory[Batch] {
  private[this] val Empty: Batch[Nothing] = new Batch0
  override def empty[A]: Batch[A] = Empty

  override def from[A](elems: Traverser[A]): Batch[A] = {
    if (elems.isInstanceOf[Batch[_]]) elems.asInstanceOf[Batch[A]]
    else super.from(elems)
  }

  implicit override def Builder[A](): Builder[A] with State[Batch[A]] =
    new BatchBuilder[A]

  override def toString: String = "Batch"
}

private[collections] final class Batch0 extends Batch[Nothing] {
  override def isEmpty: Boolean = true

  override def length: Int = 0

  override def apply(index: Int): Nothing = throw new IndexOutOfBoundsException(index.toString)

  override def update[B](index: Int, elem: B): Batch[B] = throw new IndexOutOfBoundsException(index.toString)

  override def head: Nothing = throw new NoSuchElementException("head of empty batch")

  override def tail: Batch[Nothing] = throw new UnsupportedOperationException("tail of empty batch")

  override def body: Batch[Nothing] = throw new UnsupportedOperationException("body of empty batch")

  override def foot: Nothing = throw new NoSuchElementException("foot of empty batch")

  override def drop(lower: Int): Batch[Nothing] = this

  override def take(upper: Int): Batch[Nothing] = this

  override def :+ [B](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) new IntBatch1(elem.asInstanceOf[Int]).asInstanceOf[Batch[B]]
    else if (elem.isInstanceOf[Long]) new LongBatch1(elem.asInstanceOf[Long]).asInstanceOf[Batch[B]]
    else if (elem.isInstanceOf[Float]) new FloatBatch1(elem.asInstanceOf[Float]).asInstanceOf[Batch[B]]
    else if (elem.isInstanceOf[Double]) new DoubleBatch1(elem.asInstanceOf[Double]).asInstanceOf[Batch[B]]
    else new RefBatch1(elem)
  }

  override def +: [B](elem: B): Batch[B] = {
    if (elem.isInstanceOf[Int]) new IntBatch1(elem.asInstanceOf[Int]).asInstanceOf[Batch[B]]
    else if (elem.isInstanceOf[Long]) new LongBatch1(elem.asInstanceOf[Long]).asInstanceOf[Batch[B]]
    else if (elem.isInstanceOf[Float]) new FloatBatch1(elem.asInstanceOf[Float]).asInstanceOf[Batch[B]]
    else if (elem.isInstanceOf[Double]) new DoubleBatch1(elem.asInstanceOf[Double]).asInstanceOf[Batch[B]]
    else new RefBatch1(elem)
  }

  override def traverse(f: Nothing => Unit): Unit = ()

  override def flatTraverse[B](f: B => Unit)(implicit isNested: Nothing <:< Batch[B]): Unit = ()
}

private[collections] final class BatchBuilder[A] extends Builder[A] with State[Batch[A]] {
  private[this] var these: Batch[A] = Batch.empty

  override def append(elem: A): Unit = these = these :+ elem

  override def appendAll(elems: Traverser[A]): Unit = {
    if (these.isEmpty && elems.isInstanceOf[Batch[_]])
      these = elems.asInstanceOf[Batch[A]]
    else super.appendAll(elems)
  }

  override def expect(count: Int): this.type = this

  override def state: Batch[A] = these

  override def clear(): Unit = these = Batch.empty

  override def toString: String = "Batch"+"."+"Builder"+"()"
}
