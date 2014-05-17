//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import org.scalatest._

trait SubMapBehaviors extends MapBehaviors { this: FunSpec =>
  import CollectionGenerators._
  import Matchers._

  def GenericSubMap[CC[X, Y] <: SubMap[X, Y]](CC: generic.MapFactory[CC]) = describe(s"Generic $CC submaps") {
    def decompose(n: Int): Unit = {
      var ns = CC.range(1, n): SubMap[Int, Int]
      var i = n
      while (i > 0) withClue(s"sum of first $i of $n entries") {
        var sum = 0L
        ns.traverse(sum += _._2)
        sum should equal (i.toLong * (i.toLong + 1L) / 2L)
        ns -= i
        i -= 1
      }
    }

    it("should remove entries from small maps") {
      var n = 1
      while (n <= 1024) {
        decompose(n)
        n += 1
      }
    }

    it("should remove entries from large maps") {
      decompose(1 << 15)
    }
  }
}
