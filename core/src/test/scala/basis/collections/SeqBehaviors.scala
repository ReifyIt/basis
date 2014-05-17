//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import org.scalatest._

trait SeqBehaviors extends ContainerBehaviors { this: FunSpec =>
  import CollectionEnablers._
  import CollectionGenerators._
  import Matchers._

  def GenericSeq[CC[X] <: Seq[X]](CC: generic.SeqFactory[CC]) = describe(s"Generic $CC sequences") {
    it("should have a zero length sequence") {
      (CC.empty: Seq[Any]) should have length 0
    }

    it("should sequentially traverse elements") {
      var n = 1
      while (n <= 1024) {
        val ns = CC.range(1, n)
        var i = 1
        ns.traverse { x =>
          withClue(s"element $i") (x should equal (i))
          i += 1
        }
        withClue("number of traversed elements") ((i - 1) should equal (n))
        n += 1
      }
    }

    it("should sequentially iterate elements") {
      var n = 1
      while (n <= 1024) {
        val ns = CC.range(1, n).iterator
        var i = 1
        while (!ns.isEmpty) {
          withClue(s"element $i") (ns.head should equal (i))
          ns.step()
          i += 1
        }
        withClue("number of iterated elements") ((i - 1) should equal (n))
        n += 1
      }
    }
  }
}
