//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

trait Queue[+A] extends Any with Equals with Family[Queue[_]] with LinearSeq[A] {
  /** Returns this $collection with an appended element.
    * @group Combining */
  def :+ [B >: A](elem: B): Queue[B]
}

object Queue extends generic.SeqFactory[Queue] {
  override def empty[A]: Queue[A] = immutable.Batch.empty[A]

  override def from[A](elems: Traverser[A]): Queue[A] = {
    if (elems.isInstanceOf[Queue[_]]) elems.asInstanceOf[Queue[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[Queue[A]] =
    immutable.Batch.Builder[A]

  override def toString: String = "Queue"
}
