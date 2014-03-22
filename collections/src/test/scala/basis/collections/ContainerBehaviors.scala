//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import org.scalatest._

trait ContainerBehaviors extends CollectionBehaviors { this: FunSpec =>
  import CollectionEnablers._
  import CollectionGenerators._
  import Matchers._

  def GenericContainer[CC[X] <: Container[X]](CC: generic.CollectionFactory[CC]) = describe(s"generic $CC containers") {
    it("should have an empty container") {
      CC.empty.iterator shouldBe empty
    }

    it("should build and iterate over unary containers") {
      val xs = (CC.Builder[String] += "unit").state.iterator
      var q = false
      while (!xs.isEmpty) {
        xs.head match {
          case "unit" if !q => q = true
          case "unit" => fail("Traversed expected element more than once")
          case elem => fail(s"Traversed unexpected element $elem")
        }
        xs.step()
      }
      withClue("traversed element") (q should be (true))
    }

    it("should build and iterate over n-ary containers") {
      var n = 2
      while (n <= 1024) {
        val ns = FirstNIntegers(CC, n).iterator
        var sum = 0
        while (!ns.isEmpty) {
          sum += ns.head
          ns.step()
        }
        withClue(s"sum of first $n integers") {
          sum should equal (n * (n + 1) / 2)
        }
        n += 1
      }
    }
  }
}
