//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis._

/** A bidirectionally decomposable sequence of elements.
  *
  * @define collection  bilinear sequence
  */
trait BilinearSeq[@specialized(Int, Long, Float, Double, Boolean) +A]
  extends Any with Equals with Family[BilinearSeq[_]] with LinearSeq[A] {

  override def head: A

  override def tail: BilinearSeq[A]

  /** Returns all elements except the last of this non-empty $collection.
    * @group Decomposing */
  def body: BilinearSeq[A]

  /** Returns the last element of this non-empty $collection.
    * @group Decomposing */
  def foot: A
}

object BilinearSeq extends generic.SeqFactory[BilinearSeq] {
  override def empty[A]: BilinearSeq[A] = immutable.Batch.empty[A]

  override def from[A](elems: Traverser[A]): BilinearSeq[A] = {
    if (elems.isInstanceOf[BilinearSeq[_]]) elems.asInstanceOf[BilinearSeq[A]]
    else super.from(elems)
  }

  implicit override def Builder[A]: Builder[A] with State[BilinearSeq[A]] =
    immutable.Batch.Builder[A]

  override def toString: String = "BilinearSeq"
}
