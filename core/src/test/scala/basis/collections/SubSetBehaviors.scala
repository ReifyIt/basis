//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import org.scalatest._

trait SubSetBehaviors extends SetBehaviors { this: FlatSpec =>
  import CollectionGenerators._

  override type Coll[X] <: SubSet[X]

  def GenericSubSet(): Unit = {
    it should "remove elements from small sets" in {
      var n = 1
      while (n <= 1024) {
        decomposeSubSet(n)
        n += 1
      }
    }

    it should "remove entries from large sets" in {
      decomposeSubSet(1 << 15)
    }
  }

  private def decomposeSubSet(n: Int): Unit = {
    var xs = Coll.range(1, n): SubSet[Int]
    var i = n
    while (i > 0) {
      var sum = 0L
      xs.traverse(sum += _)
      val expected = i.toLong * (i.toLong + 1L) / 2L
      if (sum != expected) fail(s"sum of remaining $i of $n entries was $sum; expected $expected:")
      xs -= i
      i -= 1
    }
  }
}
