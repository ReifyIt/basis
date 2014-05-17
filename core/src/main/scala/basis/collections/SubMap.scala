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
trait SubMap[+A, +T] extends Any with Equals with Family[SubMap[_, _]] with Map[A, T] {
  def + [B >: A, U >: T](key: B, value: U): SubMap[B, U]

  def - (key: A @uncheckedVariance): SubMap[A, T]
}

object SubMap extends generic.MapFactory[SubMap] {
  override def empty[A, T]: SubMap[A, T] = immutable.HashMap.empty[A, T]

  override def from[A, T](elems: Traverser[(A, T)]): SubMap[A, T] = {
    if (elems.isInstanceOf[SubMap[_, _]]) elems.asInstanceOf[SubMap[A, T]]
    else super.from(elems)
  }

  implicit override def Builder[A, T]: Builder[(A, T)] with State[SubMap[A, T]] =
    immutable.HashMap.Builder[A, T]

  override def toString: String = "SubMap"
}
