//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import org.scalatest._

class LongOpsSpec extends FlatSpec with Matchers {
  override def suiteName = "LongOps specification"

  "The LongToOps implicit conversion" should "add the abs macro" in {
    def test(x: Long): Unit = x.abs should equal (java.lang.Math.abs(x))
    test(-1L)
  }

  it should "add the countLeadingZeros macro" in {
    def test(x: Long): Unit = x.countLeadingZeros should equal (java.lang.Long.numberOfLeadingZeros(x))
    test(1L)
  }

  it should "add the countSetBits macro" in {
    def test(x: Long): Unit = x.countSetBits should equal (java.lang.Long.bitCount(x))
    test(0x7F7F7F7F7F7F7F7FL)
  }

  it should "add the countTrailingZeros macro" in {
    def test(x: Long): Unit = x.countTrailingZeros should equal (java.lang.Long.numberOfTrailingZeros(x))
    test(0x7F7F7F7F00000000L)
  }

  it should "add the max macro" in {
    def test(x: Long, y: Long): Unit = (x max y) should equal (java.lang.Math.max(x, y))
    test(-1L, 1L)
  }

  it should "add the min macro" in {
    def test(x: Long, y: Long): Unit = (x min y) should equal (java.lang.Math.min(x, y))
    test(-1L, 1L)
  }

  it should "add the signum macro" in {
    def test(x: Long): Unit = x.signum should equal (java.lang.Long.signum(x))
    test(Long.MinValue)
  }

  it should "add the toDoubleBits macro" in {
    def test(x: Long): Unit = x.toDoubleBits should equal (java.lang.Double.longBitsToDouble(x))
    test(0x3ff0000000000000L)
  }
}
