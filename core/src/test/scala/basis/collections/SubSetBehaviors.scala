//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import org.scalatest._

trait SubSetBehaviors extends SetBehaviors { this: FunSpec =>
  import CollectionGenerators._
  import Matchers._

  def GenericSubSet[CC[X] <: SubSet[X]](CC: generic.SetFactory[CC]) = describe(s"Generic $CC subsets") {
    def decompose(n: Int): Unit = {
      var ns = CC.range(1, n): SubSet[Int]
      var i = n
      while (i > 0) withClue(s"sum of first $i of $n elements") {
        var sum = 0L
        ns.traverse(sum += _)
        sum should equal (i.toLong * (i.toLong + 1L) / 2L)
        ns -= i
        i -= 1
      }
    }

    it("should remove elements from small sets") {
      var n = 1
      while (n <= 1024) {
        decompose(n)
        n += 1
      }
    }

    it("should remove entries from large sets") {
      decompose(1 << 15)
    }
  }
}
