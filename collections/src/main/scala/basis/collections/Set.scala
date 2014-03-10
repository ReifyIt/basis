//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import scala.annotation.unchecked._

trait Set[+A] extends Any with Equals with Family[Set[_]] with Container[A] {
  def size: Int = {
    var n = 0
    val these = iterator
    while (!these.isEmpty) {
      n += 1
      these.step()
    }
    n
  }

  def contains(elem: A @uncheckedVariance): Boolean = {
    val these = iterator
    while (!these.isEmpty) {
      if (elem == these.head) return true
      these.step()
    }
    false
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Set[_]]

  override def equals(other: Any): Boolean = {
    (this.asInstanceOf[AnyRef] eq other.asInstanceOf[AnyRef]) ||
    (other.isInstanceOf[Set[_]]) && {
      val that = other.asInstanceOf[Set[A]]
      that.canEqual(this) && this.size == that.size && {
        val those = that.iterator
        while (!those.isEmpty) {
          if (!contains(those.head)) return false
          those.step()
        }
        true
      }
    }
  }

  override def hashCode: Int = {
    import basis.util.MurmurHash3._
    var a, b = 0
    var c = 1
    val these = iterator
    while (!these.isEmpty) {
      val h = hash(these.head)
      a ^= h
      b += h
      if (h != 0) c *= h
      these.step()
    }
    mash(mix(mix(mix(seed[Set[_]], a), b), c))
  }
}

object Set extends generic.SetFactory[Set] {
  override def empty[A]: Set[A] = immutable.HashSet.empty[A]

  override def from[A](elems: Traverser[A]): Set[A] = {
    if (elems.isInstanceOf[Set[_]]) elems.asInstanceOf[Set[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[Set[A]] =
    immutable.HashSet.Builder[A]

  override def toString: String = "Set"
}
