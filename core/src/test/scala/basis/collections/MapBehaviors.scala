//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import basis.util._
import org.scalatest._

trait MapBehaviors { this: FlatSpec =>
  import CollectionEnablers._
  import CollectionGenerators._
  import Matchers._

  type Coll[X, Y] <: Map[X, Y]
  val Coll: generic.MapFactory[Coll]

  def GenericMap(): Unit = {
    it should "have an empty map" in {
      Coll.empty shouldBe empty
    }

    it should "have a zero size map" in {
      (Coll.empty: Map[Any, Any]) should have size 0
    }

    it should "build and traverse unary maps" in {
      val xs = (Coll.Builder[String, Int] += "one" -> 1).state
      var q = false
      xs.traverse(_ match {
        case ("one", 1) if !q => q = true
        case ("one", 1) => fail("Traversed expected entry more than once")
        case entry => fail(s"Traversed unexpected entry $entry")
      })
      withClue("traversed element:") (q should be (true))
    }

    it should "build and iterate over unary maps" in {
      val xs = (Coll.Builder[String, Int] += "one" -> 1).state.iterator
      var q = false
      while (!xs.isEmpty) {
        xs.head match {
          case ("one", 1) if !q => q = true
          case ("one", 1) => fail("Traversed expected entry more than once")
          case entry => fail(s"Traversed unexpected entry $entry")
        }
        xs.step()
      }
      withClue("traversed element:") (q should be (true))
    }

    it should "build and traverse n-ary maps" in {
      traverseMaps()
    }

    it should "build and iterate over n-ary maps" in {
      iterateMaps()
    }
  }

  private def traverseMap(n: Int): Unit = withClue(s"sum of first $n integers:") {
    val xs = Coll.range(1, n)
    var sum = 0L
    xs.traverse { entry =>
      if (entry._1 != entry._2) entry._1 should equal (entry._2)
      sum += entry._2
    }
    sum should equal (n.toLong * (n.toLong + 1L) / 2L)
  }

  private def traverseMaps(): Unit = {
    var k = 4
    while (k <= 20) {
      val n = 1 << k
      traverseMap(n - 1)
      traverseMap(n)
      traverseMap(n + 1)
      k += 4
    }
  }

  private def iterateMap(n: Int): Unit = withClue(s"sum of first $n integers:") {
    val xs = Coll.range(1, n).iterator
    var sum = 0L
    while (!xs.isEmpty) {
      val entry = xs.head
      if (entry._1 != entry._2) entry._1 should equal (entry._2)
      sum += entry._2
      xs.step()
    }
    sum should equal (n.toLong * (n.toLong + 1L) / 2L)
  }

  private def iterateMaps(): Unit = {
    var k = 4
    while (k <= 20) {
      val n = 1 << k
      iterateMap(n - 1)
      iterateMap(n)
      iterateMap(n + 1)
      k += 4
    }
  }
}
