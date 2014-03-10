//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

trait Deque[+A] extends Any with Equals with Family[Deque[_]] with BilinearSeq[A] with Stack[A] with Queue[A] {
  /** Returns this $collection with an appended element.
    * @group Combining */
  def :+ [B >: A](elem: B): Deque[B]

  /** Returns this $collection with a prepended element.
    * @group Combining */
  def +: [B >: A](elem: B): Deque[B]
}

object Deque extends generic.SeqFactory[Deque] {
  override def empty[A]: Deque[A] = immutable.Batch.empty[A]

  override def from[A](elems: Traverser[A]): Deque[A] = {
    if (elems.isInstanceOf[Deque[_]]) elems.asInstanceOf[Deque[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[Deque[A]] =
    immutable.Batch.Builder[A]

  override def toString: String = "Deque"
}
