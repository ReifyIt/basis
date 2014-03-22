//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import org.scalatest._

trait CollectionBehaviors { this: FunSpec =>
  import CollectionEnablers._
  import CollectionGenerators._
  import Matchers._

  def GenericCollection[CC[X] <: Collection[X]](CC: generic.CollectionFactory[CC]) = describe(s"generic $CC collections") {
    it("should have an empty collection") {
      CC.empty shouldBe empty
    }

    it("should build and traverse unary collections") {
      val xs = (CC.Builder[String] += "unit").state
      var q = false
      xs.traverse {
        case "unit" if !q => q = true
        case "unit" => fail("Traversed expected element more than once")
        case elem => fail(s"Traversed unexpected element $elem")
      }
      withClue("traversed element") (q should be (true))
    }

    it("should build and traverse n-ary collections") {
      var n = 2
      while (n <= 1024) {
        val ns = FirstNIntegers(CC, n)
        var sum = 0
        ns.traverse(sum += _)
        withClue(s"sum of first $n integers") {
          sum should equal (n * (n + 1) / 2)
        }
        n += 1
      }
    }
  }
}
