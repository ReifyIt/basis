//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis._

/** An orderly traversable collection of elements.
  *
  * @define collection  sequence
  */
trait Seq[+A] extends Any with Equals with Family[Seq[_]] with Container[A] {
  def length: Int = {
    var n = 0
    val these = iterator
    while (!these.isEmpty) {
      n += 1
      these.step()
    }
    n
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Seq[_]]

  override def equals(other: Any): Boolean = {
    (this.asInstanceOf[AnyRef] eq other.asInstanceOf[AnyRef]) ||
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
    val these = iterator
    while (!these.isEmpty) {
      h = mix(h, hash(these.head))
      these.step()
    }
    mash(h)
  }
}

object Seq extends generic.SeqFactory[Seq] {
  override def empty[A]: Seq[A] = immutable.FingerTrieSeq.empty[A]

  override def from[A](elems: Traverser[A]): Seq[A] = {
    if (elems.isInstanceOf[Seq[_]]) elems.asInstanceOf[Seq[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[Seq[A]] =
    immutable.FingerTrieSeq.Builder[A]

  override def toString: String = "Seq"
}
