//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections

import org.scalatest._

trait SeqBehaviors extends ContainerBehaviors { this: FlatSpec =>
  import CollectionEnablers._
  import Matchers._

  override type Coll[X] <: Seq[X]
  override val Coll: generic.SeqFactory[Coll]

  def GenericSeq(): Unit = {
    it should "have a zero length sequence" in {
      (Coll.empty: Seq[Any]) should have length 0
    }

    it should "sequentially traverse AnyRef sequences" in {
      traverseSeqs(boxSeries)
    }

    it should "sequentially iterate over AnyRef sequences" in {
      iterateSeqs(boxSeries)
    }
  }

  def SpecializedSeq(): Unit = {
    it should "sequentially traverse Int sequences" in {
      traverseSeqs(intSeries)
    }

    it should "sequentially iterate over Int sequences" in {
      iterateSeqs(intSeries)
    }

    it should "sequentially traverse Long sequences" in {
      traverseSeqs(longSeries)
    }

    it should "sequentially iterate over Long sequences" in {
      iterateSeqs(longSeries)
    }

    it should "sequentially traverse Float sequences" in {
      traverseSeqs(floatSeries)
    }

    it should "sequentially iterate over Float sequences" in {
      iterateSeqs(floatSeries)
    }

    it should "sequentially traverse Double sequences" in {
      traverseSeqs(doubleSeries)
    }

    it should "sequentially iterate over Double sequences" in {
      iterateSeqs(doubleSeries)
    }
  }

  private def intSeries: Int => Int = i => i

  private def longSeries: Int => Long = i => i.toLong

  private def floatSeries: Int => Float = i => i.toFloat

  private def doubleSeries: Int => Double = i => i.toDouble

  private def boxSeries: Int => Box = i => new Box(i)

  private def traverseSeq[T](length: Int)(series: Int => T): Unit = {
    val xs = Coll.tabulate(length)(series)
    var i = 0
    xs.traverse { x =>
      if (x != series(i)) withClue(s"element $i:") (x should equal (series(i)))
      i += 1
    }
    withClue("number of traversed elements:") (i should equal (length))
  }

  private def traverseSeqs[T](series: Int => T): Unit = {
    var k = 4
    while (k <= 20) {
      val n = 1 << k
      traverseSeq(n - 1)(series)
      traverseSeq(n)(series)
      traverseSeq(n + 1)(series)
      k += 4
    }
  }

  private def iterateSeq[T](length: Int)(series: Int => T): Unit = {
    val xs = Coll.tabulate(length)(series).iterator
    var i = 0
    while (!xs.isEmpty) {
      if (xs.head != series(i)) withClue(s"element $i:") (xs.head should equal (series(i)))
      xs.step()
      i += 1
    }
    withClue("number of iterated elements:") (i should equal (length))
  }

  private def iterateSeqs[T](series: Int => T): Unit = {
    var k = 4
    while (k <= 20) {
      val n = 1 << k
      iterateSeq(n - 1)(series)
      iterateSeq(n)(series)
      iterateSeq(n + 1)(series)
      k += 4
    }
  }
}
