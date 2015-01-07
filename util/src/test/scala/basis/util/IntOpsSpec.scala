//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import org.scalatest._

class IntOpsSpec extends FlatSpec with Matchers {
  override def suiteName = "IntOps specification"

  "The IntToOps implicit conversion" should "add the abs macro" in {
    def test(x: Int): Unit = x.abs should equal (java.lang.Math.abs(x))
    test(-1)
  }

  it should "add the countLeadingZeros macro" in {
    def test(x: Int): Unit = x.countLeadingZeros should equal (java.lang.Integer.numberOfLeadingZeros(x))
    test(1)
  }

  it should "add the countSetBits macro" in {
    def test(x: Int): Unit = x.countSetBits should equal (java.lang.Integer.bitCount(x))
    test(0x7F7F7F7F)
  }

  it should "add the countTrailingZeros macro" in {
    def test(x: Int): Unit = x.countTrailingZeros should equal (java.lang.Integer.numberOfTrailingZeros(x))
    test(0x7F7F0000)
  }

  it should "add the max macro" in {
    def test(x: Int, y: Int): Unit = (x max y) should equal (java.lang.Math.max(x, y))
    test(-1, 1)
  }

  it should "add the min macro" in {
    def test(x: Int, y: Int): Unit = (x min y) should equal (java.lang.Math.min(x, y))
    test(-1, 1)
  }

  it should "add the signum macro" in {
    def test(x: Int): Unit = x.signum should equal (java.lang.Integer.signum(x))
    test(Int.MinValue)
  }

  it should "add the toFloatBits macro" in {
    def test(x: Int): Unit = x.toFloatBits should equal (java.lang.Float.intBitsToFloat(x))
    test(0x3f800000)
  }
}
