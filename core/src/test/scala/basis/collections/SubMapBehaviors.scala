//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import org.scalatest._

trait SubMapBehaviors extends MapBehaviors { this: FlatSpec =>
  import CollectionGenerators._

  override type Coll[X, Y] <: SubMap[X, Y]

  def GenericSubMap(): Unit = {
    it should "remove entries from small maps" in {
      var n = 1
      while (n <= 1024) {
        decomposeSubMap(n)
        n += 1
      }
    }

    it should "remove entries from large maps" in {
      decomposeSubMap(1 << 15)
    }
  }

  private def decomposeSubMap(n: Int): Unit = {
    var xs = Coll.range(1, n): SubMap[Int, Int]
    var i = n
    while (i > 0) withClue(s"sum of first $i of $n entries:") {
      var sum = 0L
      xs.traverse(sum += _._2)
      val expected = i.toLong * (i.toLong + 1L) / 2L
      if (sum != expected) fail(s"sum of remaining $i of $n entries was $sum; expected $expected:")
      xs -= i
      i -= 1
    }
  }
}
