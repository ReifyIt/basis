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
trait Subset[+A] extends Any with Equals with Family[Subset[_]] with Set[A] {
  def + [B >: A](elem: B): Subset[B]

  def - (elem: A @uncheckedVariance): Subset[A]
}

object Subset extends generic.SetFactory[Subset] {
  override def empty[A]: Subset[A] = immutable.HashTrieSet.empty[A]

  override def from[A](elems: Traverser[A]): Subset[A] = {
    if (elems.isInstanceOf[Subset[_]]) elems.asInstanceOf[Subset[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[Subset[A]] =
    immutable.HashTrieSet.Builder[A]

  override def toString: String = "Subset"
}
