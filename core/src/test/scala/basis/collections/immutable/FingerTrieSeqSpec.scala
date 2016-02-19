//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.collections
package immutable

import org.scalatest._

class FingerTrieSeqSpec extends FlatSpec with Matchers with SeqBehaviors {
  override def suiteName = "FingerTrieSeq specification"

  override type Coll[X] = FingerTrieSeq[X]
  override val Coll = FingerTrieSeq

  it should behave like GenericCollection()
  it should behave like GenericContainer()
  it should behave like GenericSeq()

  it should behave like GenericCollectionBuilder()

  it should "drop arbitrary subsequences" in {
    dropSeqs(boxSeries)
  }

  it should "take arbitrary subsequences" in {
    takeSeqs(boxSeries)
  }

  it should "shift fifo subsequences" in {
    shiftFifoSeqs()
  }

  it should "reverse shift fifo subsequences" in {
    reverseShiftFifoSeqs()
  }

  private def boxSeries: Int => Box = i => new Box(i)

  private def dropSeq[T](length: Int)(series: Int => T): Unit = {
    val xs = FingerTrieSeq.tabulate(length)(series)
    var l = 0
    while (l < length) {
      val ys = xs.drop(l)
      var i = l
      ys.traverse { y =>
        if (y != series(i)) withClue(s"element $i in drop $l of $length:") (y should equal (series(i)))
        i += 1
      }
      withClue(s"number of traversed elements in drop $l of $length:") (i should equal (length))
      l += 1
    }
  }

  private def dropSeqs[T](series: Int => T): Unit = {
    dropSeq((1 << 10) + 1)(series)
    dropSeq((1 << 15) + 1)(series)
  }

  private def takeSeq[T](length: Int)(series: Int => T): Unit = {
    val xs = FingerTrieSeq.tabulate(length)(series)
    var u = 1
    while (u <= length) try {
      val ys = xs.take(u)
      var i = 0
      ys.traverse { y =>
        if (y != series(i)) withClue(s"element $i in take $u of $length:") (y should equal (series(i)))
        i += 1
      }
      withClue(s"number of traversed elements in take $u of $length:") (i should equal (u))
      u += 1
    }
    catch {
      case e: Exception =>
        info(s"take $u of $length")
        throw e
    }
  }

  private def takeSeqs[T](series: Int => T): Unit = {
    takeSeq((1 << 10) + 1)(series)
    takeSeq((1 << 15) + 1)(series)
  }

  private def shiftFifoSeq(pushCount: Int, pollCount: Int): Unit = {
    var xs = FingerTrieSeq.empty[Int]
    var i = 0
    while (i < pushCount) {
      xs :+= i
      i += 1
    }
    var j = 0
    while (j < pollCount) {
      xs = xs.tail
      j += 1
    }
    while (i < pushCount + pollCount) {
      xs :+= i
      i += 1
    }
    i = 0
    while (i < pushCount) {
      xs(i) should equal (i + pollCount)
      i += 1
    }
  }

  private def shiftFifoSeqs(): Unit = {
    var i = 0
    while (i < 128) {
      var j = 0
      while (j < i) {
        shiftFifoSeq(i, j)
        j += 1
      }
      i += 1
    }
  }

  private def reverseShiftFifoSeq(pushCount: Int, pollCount: Int): Unit = {
    var xs = FingerTrieSeq.empty[Int]
    var i = 0
    while (i < pushCount) {
      xs +:= i
      i += 1
    }
    var j = 0
    while (j < pollCount) {
      xs = xs.body
      j += 1
    }
    while (i < pushCount + pollCount) {
      xs +:= i
      i += 1
    }
    i = 0
    while (i < pushCount) {
      xs(i) should equal (pushCount + pollCount - i - 1)
      i += 1
    }
  }

  private def reverseShiftFifoSeqs(): Unit = {
    var i = 0
    while (i < 128) {
      var j = 0
      while (j < i) {
        reverseShiftFifoSeq(i, j)
        j += 1
      }
      i += 1
    }
  }
}
