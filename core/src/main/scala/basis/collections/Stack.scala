//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis._

/** A prependable linear sequence. Forms a last-in first-out (LIFO) queue.
  *
  * @define collection  stack
  */
trait Stack[+A] extends Any with Equals with Family[Stack[_]] with LinearSeq[A] {
  def :: [B >: A](elem: B): Stack[B]
}

object Stack extends generic.SeqFactory[Stack] {
  override def empty[A]: Stack[A] = immutable.List.empty[A]

  override def from[A](elems: Traverser[A]): Stack[A] = {
    if (elems.isInstanceOf[Stack[_]]) elems.asInstanceOf[Stack[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[Stack[A]] =
    immutable.List.Builder[A]

  override def toString: String = "Stack"
}
