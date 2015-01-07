//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis._

/** A recursively decomposable sequence of elements.
  *
  * @define collection  linear sequence
  */
trait LinearSeq[@specialized(Int, Long, Float, Double, Boolean) +A]
  extends Any with Equals with Family[LinearSeq[_]] with Seq[A] {

  def head: A

  def tail: LinearSeq[A]

  override def iterator: Iterator[A] = new LinearSeqIterator(this)

  override def traverse(f: A => Unit): Unit = {
    var xs = this
    while (!xs.isEmpty) {
      f(xs.head)
      xs = xs.tail
    }
  }

  override def equals(other: Any): Boolean = {
    (this.asInstanceOf[AnyRef] eq other.asInstanceOf[AnyRef]) ||
    other.isInstanceOf[LinearSeq[_]] && {
      val that = other.asInstanceOf[LinearSeq[_]]
      that.canEqual(this) && {
        var these = this
        var those = that
        while (!these.isEmpty && !those.isEmpty) {
          if (these.head != those.head) return false
          these = these.tail
          those = those.tail
        }
        these.isEmpty && those.isEmpty
      }
    }
    other.isInstanceOf[Seq[_]] && {
      val that = other.asInstanceOf[Seq[_]]
      that.canEqual(this) && {
        val these = this.iterator
        val those = that.iterator
        while (!these.isEmpty && !those.isEmpty) {
          if (these.head != those.head) return false
          these.step()
          those.step()
        }
        these.isEmpty && those.isEmpty
      }
    }
  }

  override def hashCode: Int = {
    import basis.util.MurmurHash3._
    var h = seed[Seq[_]]
    var xs = this
    while (!xs.isEmpty) {
      h = mix(h, hash(xs.head))
      xs = xs.tail
    }
    mash(h)
  }
}

object LinearSeq extends generic.SeqFactory[LinearSeq] {
  override def empty[A]: LinearSeq[A] = immutable.List.empty[A]

  override def from[A](elems: Traverser[A]): LinearSeq[A] = {
    if (elems.isInstanceOf[LinearSeq[_]]) elems.asInstanceOf[LinearSeq[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[LinearSeq[A]] =
    immutable.List.Builder[A]

  override def toString: String = "LinearSeq"
}

private[collections] final class LinearSeqIterator
    [@specialized(Int, Long, Float, Double, Boolean) +A]
    (private[this] var xs: LinearSeq[A])
  extends Iterator[A] {

  override def isEmpty: Boolean = xs.isEmpty

  override def head: A = if (!xs.isEmpty) xs.head else Iterator.empty.head

  override def step(): Unit = if (!xs.isEmpty) xs = xs.tail else Iterator.empty.step()

  override def dup: Iterator[A] = new LinearSeqIterator(xs)
}
