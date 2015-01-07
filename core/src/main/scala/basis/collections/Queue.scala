//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis._

/** An appendanle linear sequence. Forms a first-in first-out (FIFO) queue.
  *
  * @define collection  queue
  */
trait Queue[+A] extends Any with Equals with Family[Queue[_]] with LinearSeq[A] {
  def :+ [B >: A](elem: B): Queue[B]
}

object Queue extends generic.SeqFactory[Queue] {
  override def empty[A]: Queue[A] = immutable.FingerTrieSeq.empty[A]

  override def from[A](elems: Traverser[A]): Queue[A] = {
    if (elems.isInstanceOf[Queue[_]]) elems.asInstanceOf[Queue[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[Queue[A]] =
    immutable.FingerTrieSeq.Builder[A]

  override def toString: String = "Queue"
}
