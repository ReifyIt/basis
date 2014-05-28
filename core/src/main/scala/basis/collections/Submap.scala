//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis._
import scala.annotation.unchecked._

/** A decomposable map.
  *
  * @define collection  map
  */
trait Submap[+A, +T] extends Any with Equals with Family[Submap[_, _]] with Map[A, T] {
  def + [B >: A, U >: T](key: B, value: U): Submap[B, U]

  def - (key: A @uncheckedVariance): Submap[A, T]
}

object Submap extends generic.MapFactory[Submap] {
  override def empty[A, T]: Submap[A, T] = immutable.HashTrieMap.empty[A, T]

  override def from[A, T](elems: Traverser[(A, T)]): Submap[A, T] = {
    if (elems.isInstanceOf[Submap[_, _]]) elems.asInstanceOf[Submap[A, T]]
    else super.from(elems)
  }

  implicit override def Builder[A, T]: Builder[(A, T)] with State[Submap[A, T]] =
    immutable.HashTrieMap.Builder[A, T]

  override def toString: String = "Submap"
}
