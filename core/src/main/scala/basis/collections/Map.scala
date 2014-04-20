//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis._
import basis.text._
import scala.annotation.unchecked._

/** An association of unique keys with values.
  *
  * @define collection  map
  */
trait Map[+A, +T] extends Any with Equals with Family[Map[_, _]] with Container[(A, T)] {
  def size: Int = {
    var n = 0
    val these = iterator
    while (!these.isEmpty) {
      n += 1
      these.step()
    }
    n
  }

  def contains(key: A @uncheckedVariance): Boolean = {
    val these = iterator
    while (!these.isEmpty) {
      if (key == these.head._1) return true
      these.step()
    }
    false
  }

  def apply(key: A @uncheckedVariance): T = {
    val these = iterator
    while (!these.isEmpty) {
      val entry = these.head
      if (key == entry._1) return entry._2
      these.step()
    }
    throw new NoSuchElementException(key.toString)
  }

  def get(key: A @uncheckedVariance): Maybe[T] = {
    val these = iterator
    while (!these.isEmpty) {
      val entry = these.head
      if (key == entry._1) return Bind(entry._2)
      these.step()
    }
    Trap
  }

  def traverse(f: (A, T) => Unit): Unit = {
    val these = iterator
    while (!these.isEmpty) {
      val entry = these.head
      f(entry._1, entry._2)
      these.step()
    }
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Map[_, _]]

  override def equals(other: Any): Boolean = {
    (this.asInstanceOf[AnyRef] eq other.asInstanceOf[AnyRef]) ||
    other.isInstanceOf[Map[_, _]] && {
      val that = other.asInstanceOf[Map[A, _]]
      that.canEqual(this) && this.size == that.size && {
        val those = that.iterator
        while (!those.isEmpty) {
          if (!contains(those.head._1)) return false
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
      val h = hash(these.head._1)
      a ^= h
      b += h
      if (c != 0) c *= h
      these.step()
    }
    mash(mix(mix(mix(seed[Map[_, _]], a), b), c))
  }

  override def toString: String = {
    val s = UString.Builder
    s.append(stringPrefix)
    s.append('(')
    val these = iterator
    if (!isEmpty) {
      val entry = these.head
      s.show(entry._1)
      s.append(" -> ")
      s.show(entry._2)
      these.step()
      while (!these.isEmpty) {
        val entry = these.head
        s.append(", ")
        s.show(entry._1)
        s.append(" -> ")
        s.show(entry._2)
        these.step()
      }
    }
    s.append(')')
    s.state.toString
  }
}

object Map extends generic.MapFactory[Map] {
  override def empty[A, T]: Map[A, T] = immutable.HashMap.empty[A, T]

  override def from[A, T](elems: Traverser[(A, T)]): Map[A, T] = {
    if (elems.isInstanceOf[Map[_, _]]) elems.asInstanceOf[Map[A, T]]
    else super.from(elems)
  }

  implicit override def Builder[A, T]: Builder[(A, T)] with State[Map[A, T]] =
    immutable.HashMap.Builder[A, T]

  override def toString: String = "Map"
}
