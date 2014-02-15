//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis.util._
import org.scalatest._

trait MapBehaviors { this: FunSpec =>
  import CollectionEnablers._
  import CollectionGenerators._
  import Matchers._

  def GenericMap[CC[X, Y] <: Map[X, Y]](CC: generic.MapFactory[CC]) = describe(s"generic $CC maps") {
    it("should have an empty map") {
      CC.empty shouldBe empty
    }

    it("should have a zero size map") {
      (CC.empty: Map[Any, Any]) should have size 0
    }

    it("should build and traverse unary maps") {
      val xs = (CC.Builder[String, Int]() += "one" -> 1).state
      var q = false
      xs.traverse(_ match {
        case ("one", 1) if !q => q = true
        case ("one", 1) => fail("Traversed expected entry more than once")
        case entry => fail(s"Traversed unexpected entry $entry")
      })
      withClue("traversed element") (q should be (true))
    }

    it("should build and iterate over unary maps") {
      val xs = (CC.Builder[String, Int]() += "one" -> 1).state.iterator
      var q = false
      while (!xs.isEmpty) {
        xs.head match {
          case ("one", 1) if !q => q = true
          case ("one", 1) => fail("Traversed expected entry more than once")
          case entry => fail(s"Traversed unexpected entry $entry")
        }
        xs.step()
      }
      withClue("traversed element") (q should be (true))
    }

    it("should build and traverse n-ary maps") {
      var n = 2
      while (n <= 1024) {
        val ns = FirstNIntegers(CC, n)
        var sum = 0
        ns.traverse { entry =>
          withClue(entry.toString) (entry._1 should equal (entry._2))
          sum += entry._2
        }
        withClue(s"sum of first $n integers") {
          sum should equal (n * (n + 1) / 2)
        }
        n += 1
      }
    }

    it("should build and iterate over n-ary maps") {
      var n = 2
      while (n <= 1024) {
        val ns = FirstNIntegers(CC, n).iterator
        var sum = 0
        while (!ns.isEmpty) {
          val entry = ns.head
          withClue(entry.toString) (entry._1 should equal (entry._2))
          sum += entry._2
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
