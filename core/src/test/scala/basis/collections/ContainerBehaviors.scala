//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import org.scalatest._

trait ContainerBehaviors extends CollectionBehaviors { this: FlatSpec =>
  import CollectionEnablers._
  import CollectionGenerators._
  import Matchers._

  override type Coll[X] <: Container[X]

  def GenericContainer(): Unit = {
    it should "have an empty container" in {
      Coll.empty.iterator shouldBe empty
    }

    it should "build and iterate over unary containers" in {
      val xs = (Coll.Builder[String] += "unit").state.iterator
      var q = false
      while (!xs.isEmpty) {
        xs.head match {
          case "unit" if !q => q = true
          case "unit" => fail("Traversed expected element more than once")
          case elem => fail(s"Traversed unexpected element $elem")
        }
        xs.step()
      }
      withClue("traversed element:") (q should be (true))
    }

    it should "build and iterate over n-ary containers" in {
      iterateContainers()
    }
  }

  private def iterateContainer(n: Int): Unit = withClue(s"sum of first $n integers:") {
    val xs = Coll.range(1, n).iterator
    var sum = 0L
    while (!xs.isEmpty) {
      sum += xs.head
      xs.step()
    }
    sum should equal (n.toLong * (n.toLong + 1L) / 2L)
  }

  private def iterateContainers(): Unit = {
    var k = 4
    while (k <= 20) {
      val n = 1 << k
      iterateContainer(n - 1)
      iterateContainer(n)
      iterateContainer(n + 1)
      k += 4
    }
  }
}
