//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis._

/** A double-ended queue.
  *
  * @define collection  deque
  */
trait Deque[+A] extends Any with Equals with Family[Deque[_]] with BilinearSeq[A] with Stack[A] with Queue[A] {
  def :+ [B >: A](elem: B): Deque[B]

  def +: [B >: A](elem: B): Deque[B]
}

object Deque extends generic.SeqFactory[Deque] {
  override def empty[A]: Deque[A] = immutable.Vector.empty[A]

  override def from[A](elems: Traverser[A]): Deque[A] = {
    if (elems.isInstanceOf[Deque[_]]) elems.asInstanceOf[Deque[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[Deque[A]] =
    immutable.Vector.Builder[A]

  override def toString: String = "Deque"
}
