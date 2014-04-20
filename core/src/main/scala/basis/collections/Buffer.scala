//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis._

/** A mutable sequence of elements.
  *
  * @define collection  buffer
  */
trait Buffer[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) A] extends Seq[A] with Builder[A] {
  def apply(index: Int): A

  def update(index: Int, elem: A): Unit

  def prepend(elem: A): Unit

  def prependAll(elems: Traverser[A]): Unit =
    elems.traverse(new Buffer.Insert(this, 0))

  def insert(index: Int, elem: A): Unit

  def insertAll(index: Int, elems: Traverser[A]): Unit = {
    if (index < 0 || index > length) throw new IndexOutOfBoundsException(index.toString)
    elems.traverse(new Buffer.Insert(this, index))
  }

  def remove(index: Int): A

  def remove(index: Int, count: Int): Unit = {
    if (count < 0) throw new IllegalArgumentException("negative count")
    if (index < 0) throw new IndexOutOfBoundsException(index.toString)
    var i = index + count - 1
    if (i >= length) throw new IndexOutOfBoundsException(i.toString)
    while (i >= index) {
      remove(i)
      i -= 1
    }
  }

  def +=: (elem: A): this.type = {
    prepend(elem)
    this
  }

  def ++=: (elems: Traverser[A]): this.type = {
    prependAll(elems)
    this
  }

  override def += (elem: A): this.type = {
    append(elem)
    this
  }

  override def ++= (elems: Traverser[A]): this.type = {
    appendAll(elems)
    this
  }

  override def state: State = throw new UnsupportedOperationException("Buffer has undefined state")

  def copy: Buffer[A]
}

object Buffer extends generic.SeqFactory[Buffer] {
  override def empty[A]: Buffer[A] = mutable.ListBuffer.empty[A]

  override def from[A](elems: Traverser[A]): Buffer[A] = {
    if (elems.isInstanceOf[Buffer[_]]) elems.asInstanceOf[Buffer[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[Buffer[A]] =
    mutable.ListBuffer.Builder[A]

  private[collections] final class Append[-A](b: Builder[A]) extends scala.runtime.AbstractFunction1[A, Unit] {
    override def apply(elem: A): Unit = b.append(elem)
  }

  private[collections] final class Insert[-A](b: Buffer[A], private[this] var i: Int) extends scala.runtime.AbstractFunction1[A, Unit] {
    override def apply(x: A): Unit = { b.insert(i, x); i += 1 }
  }
}
