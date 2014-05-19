//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import org.scalatest._

trait CollectionBehaviors { this: FlatSpec =>
  import CollectionEnablers._
  import CollectionGenerators._
  import Matchers._

  type Coll[X] <: Collection[X]
  val Coll: generic.CollectionFactory[Coll]

  def GenericCollection(): Unit = {
    it should "have an empty collection" in {
      Coll.empty shouldBe empty
    }

    it should "build and traverse unary collections" in {
      val xs = (Coll.Builder[String] += "unit").state
      var q = false
      xs.traverse {
        case "unit" if !q => q = true
        case "unit" => fail("Traversed expected element more than once")
        case elem => fail(s"Traversed unexpected element $elem")
      }
      withClue("traversed element:") (q should be (true))
    }

    it should "build and traverse n-ary collections" in {
      traverseCollections()
    }
  }

  def GenericCollectionBuilder(): Unit = {
    it should "concatenate small collections" in {
      concatSmall(alias = false)
    }

    it should "concatenate small collections with intermediate aliasing" in {
      concatSmall(alias = true)
    }

    it should "concatenate large collections" in {
      concatLarge(alias = false)
    }

    it should "concatenate large collections with intermediate aliasing" in {
      concatLarge(alias = true)
    }
  }

  private def traverseCollection(n: Int): Unit = withClue(s"sum of first $n integers:") {
    val xs = Coll.range(1, n)
    var sum = 0L
    xs.traverse(sum += _)
    sum should equal (n.toLong * (n.toLong + 1L) / 2L)
  }

  private def traverseCollections(): Unit = {
    var k = 4
    while (k <= 20) {
      val n = 1 << k
      traverseCollection(n - 1)
      traverseCollection(n)
      traverseCollection(n + 1)
      k += 4
    }
  }

  private def concat(i: Int, j: Int, alias: Boolean): Unit = withClue(s"[1..$i] ++ [${i + 1}..${i + j}]:") {
    val xs = Coll.range(1, i)
    val ys = Coll.range(i + 1, i + j)

    val b = Coll.Builder[Int]
    b.appendAll(xs)
    if (alias) b.state
    b.appendAll(ys)
    val ns = b.state

    var sum = 0L
    ns.traverse(sum += _)
    val n = (i + j).toLong
    sum should equal (n * (n + 1L) / 2L)
  }

  private def concatSmall(alias: Boolean): Unit = {
    var n = 2
    while (n <= (1 << 10) + 1) {
      var i = 1
      var j = n - 1
      while (i < n) {
        concat(i, j, alias)
        i += 1
        j -= 1
      }
      n += 1
    }
  }

  private def concatLarge(alias: Boolean): Unit = {
    var k = 2
    while (k <= 20) {
      val n = 1 << k
      concat(1, n, alias)
      concat(n, 1, alias)
      concat(n, n, alias)
      k += 1
    }
  }
}
