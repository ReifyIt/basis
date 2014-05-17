//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis._
import scala.annotation.unchecked._

/** A decomposable set.
  *
  * @define collection  set
  */
trait SubSet[+A] extends Any with Equals with Family[SubSet[_]] with Set[A] {
  def + [B >: A](elem: B): SubSet[B]

  def - (elem: A @uncheckedVariance): SubSet[A]
}

object SubSet extends generic.SetFactory[SubSet] {
  override def empty[A]: SubSet[A] = immutable.HashSet.empty[A]

  override def from[A](elems: Traverser[A]): SubSet[A] = {
    if (elems.isInstanceOf[SubSet[_]]) elems.asInstanceOf[SubSet[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[SubSet[A]] =
    immutable.HashSet.Builder[A]

  override def toString: String = "SubSet"
}
