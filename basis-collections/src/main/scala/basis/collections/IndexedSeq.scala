//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

trait IndexedSeq[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) +A]
  extends Any with Equals with Family[IndexedSeq[_]] with Seq[A] {

  def apply(index: Int): A

  override def iterator: Iterator[A] = new IndexedSeqIterator(this)

  override def traverse(f: A => Unit): Unit = {
    var i = 0
    val n = length
    while (i < n) {
      f(this(i))
      i += 1
    }
  }

  override def equals(other: Any): Boolean = {
    (this.asInstanceOf[AnyRef] eq other.asInstanceOf[AnyRef]) ||
    other.isInstanceOf[IndexedSeq[_]] && {
      val that = other.asInstanceOf[IndexedSeq[_]]
      val n = length
      that.canEqual(this) && that.length == n && {
        var i = 0
        while (i < n) {
          if (this(i) != that(i)) return false
          i += 1
        }
        true
      }
    } ||
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
    var i = 0
    val n = length
    while (i < n) {
      h = mix(h, hash(this(i)))
      i += 1
    }
    mash(h)
  }
}

object IndexedSeq extends generic.SeqFactory[IndexedSeq] {
  override def empty[A]: IndexedSeq[A] = immutable.Vector.empty[A]

  override def from[A](elems: Traverser[A]): IndexedSeq[A] = {
    if (elems.isInstanceOf[IndexedSeq[_]]) elems.asInstanceOf[IndexedSeq[A]]
    else super.from(elems)
  }

  implicit override def Builder[A](): Builder[A] with State[IndexedSeq[A]] =
    immutable.Vector.Builder[A]()

  override def toString: String = "IndexedSeq"
}

private[collections] final class IndexedSeqIterator
    [@specialized(Byte, Short, Int, Long, Float, Double, Boolean) +A]
    (private[this] val xs: IndexedSeq[A], private[this] var i: Int, private[this] val n: Int)
  extends Iterator[A] {

  def this(xs: IndexedSeq[A]) = this(xs, 0, xs.length)

  override def isEmpty: Boolean = i >= n

  override def head: A = if (i < n) xs(i) else Iterator.empty.head

  override def step(): Unit = if (i < n) i += 1 else Iterator.empty.step()

  override def dup: Iterator[A] = new IndexedSeqIterator(xs, i, n)
}
