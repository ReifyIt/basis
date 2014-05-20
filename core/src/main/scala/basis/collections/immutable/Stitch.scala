//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import basis._
import scala.Predef.<:<

abstract class Stitch[+A] private[collections]
  extends Equals
  with Immutable
  with Family[Stitch[_]]
  with IndexedSeq[A]
  with Deque[A] {

  override def body: Stitch[A]

  override def tail: Stitch[A]

  /** Returns a copy of this $collection with the given element at the given index.
    * @group Indexing */
  def update[B >: A](index: Int, elem: B): Stitch[B]

  /** Returns all but the first `lower` elements of this $collection.
    * @group Slicing */
  def drop(lower: Int): Stitch[A]

  /** Returns the first `upper` elements of this $collection.
    * @group Slicing */
  def take(upper: Int): Stitch[A]

  override def :+ [B >: A](elem: B): Stitch[B]

  override def +: [B >: A](elem: B): Stitch[B]

  override def :: [B >: A](elem: B): Stitch[B] = elem +: this

  override def traverse(f: A => Unit): Unit

  /** Applies a side-effecting function to each nested element of this $collection.
    * @group Traversing */
  def flatTraverse[B](f: B => Unit)(implicit isNested: A <:< Stitch[B]): Unit = {
    var i = 0
    val n = length
    while (i < n) {
      this(i).traverse(f)
      i += 1
    }
  }

  protected override def stringPrefix: String = "Stitch"
}

object Stitch extends generic.SeqFactory[Stitch] {
  private[this] val Empty: Stitch[Nothing] = new Stitch0
  override def empty[A]: Stitch[A] = Empty

  override def from[A](elems: Traverser[A]): Stitch[A] = {
    if (elems.isInstanceOf[Stitch[_]]) elems.asInstanceOf[Stitch[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[Stitch[A]] =
    new StitchBuilder[A]

  override def toString: String = "Stitch"
}

private[collections] final class Stitch0 extends Stitch[Nothing] {
  override def isEmpty: Boolean = true

  override def length: Int = 0

  override def apply(index: Int): Nothing = throw new IndexOutOfBoundsException(index.toString)

  override def update[B](index: Int, elem: B): Stitch[B] = throw new IndexOutOfBoundsException(index.toString)

  override def head: Nothing = throw new NoSuchElementException("head of empty stitch")

  override def tail: Stitch[Nothing] = throw new UnsupportedOperationException("tail of empty stitch")

  override def body: Stitch[Nothing] = throw new UnsupportedOperationException("body of empty stitch")

  override def foot: Nothing = throw new NoSuchElementException("foot of empty stitch")

  override def drop(lower: Int): Stitch[Nothing] = this

  override def take(upper: Int): Stitch[Nothing] = this

  override def :+ [B](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) new IntStitch1(elem.asInstanceOf[Int]).asInstanceOf[Stitch[B]]
    else if (elem.isInstanceOf[Long]) new LongStitch1(elem.asInstanceOf[Long]).asInstanceOf[Stitch[B]]
    else if (elem.isInstanceOf[Float]) new FloatStitch1(elem.asInstanceOf[Float]).asInstanceOf[Stitch[B]]
    else if (elem.isInstanceOf[Double]) new DoubleStitch1(elem.asInstanceOf[Double]).asInstanceOf[Stitch[B]]
    else new RefStitch1(elem)
  }

  override def +: [B](elem: B): Stitch[B] = {
    if (elem.isInstanceOf[Int]) new IntStitch1(elem.asInstanceOf[Int]).asInstanceOf[Stitch[B]]
    else if (elem.isInstanceOf[Long]) new LongStitch1(elem.asInstanceOf[Long]).asInstanceOf[Stitch[B]]
    else if (elem.isInstanceOf[Float]) new FloatStitch1(elem.asInstanceOf[Float]).asInstanceOf[Stitch[B]]
    else if (elem.isInstanceOf[Double]) new DoubleStitch1(elem.asInstanceOf[Double]).asInstanceOf[Stitch[B]]
    else new RefStitch1(elem)
  }

  override def traverse(f: Nothing => Unit): Unit = ()

  override def flatTraverse[B](f: B => Unit)(implicit isNested: Nothing <:< Stitch[B]): Unit = ()
}

private[collections] final class StitchBuilder[A] extends Builder[A] with State[Stitch[A]] {
  private[this] var these: Stitch[A] = Stitch.empty

  override def append(elem: A): Unit = these = these :+ elem

  override def appendAll(elems: Traverser[A]): Unit = {
    if (these.isEmpty && elems.isInstanceOf[Stitch[_]])
      these = elems.asInstanceOf[Stitch[A]]
    else super.appendAll(elems)
  }

  override def expect(count: Int): this.type = this

  override def state: Stitch[A] = these

  override def clear(): Unit = these = Stitch.empty

  override def toString: String = "Stitch"+"."+"Builder"
}
