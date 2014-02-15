//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import org.scalatest.enablers._

object CollectionEnablers {
  implicit object CollectionEmptiness extends Emptiness[Collection[Any]] {
    override def isEmpty(xs: Collection[Any]): Boolean = xs.isEmpty
  }

  implicit object IteratorEmptiness extends Emptiness[Iterator[Any]] {
    override def isEmpty(xs: Iterator[Any]): Boolean = xs.isEmpty
  }

  implicit object MapSize extends Size[Map[Any, Any]] {
    override def sizeOf(xs: Map[Any, Any]): Long = xs.size.toLong
  }

  implicit object SeqLength extends Length[Seq[Any]] {
    override def lengthOf(xs: Seq[Any]): Long = xs.length.toLong
  }

  implicit object SetSize extends Size[Set[Any]] {
    override def sizeOf(xs: Set[Any]): Long = xs.size.toLong
  }
}
