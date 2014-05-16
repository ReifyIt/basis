//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import org.scalatest._

class FloatOpsSpec extends FlatSpec with Matchers {
  override def suiteName = "FloatOps specification"

  "The FloatToOps implicit conversion" should "add the abs macro" in {
    def test(x: Float): Unit = x.abs should equal (java.lang.Math.abs(x))
    test(-1.0F)
  }

  it should "add the isInfinite macro" in {
    def test(x: Float): Unit = x.isInfinite should equal (java.lang.Float.isInfinite(x))
    test(Float.NegativeInfinity)
    test(0.0F)
    test(Float.PositiveInfinity)
  }

  it should "add the isNaN macro" in {
    def test(x: Float): Unit = x.isNaN should equal (java.lang.Float.isNaN(x))
    test(0.0F)
    test(Float.NaN)
  }

  it should "add the max macro" in {
    def test(x: Float, y: Float): Unit = (x max y) should equal (java.lang.Math.max(x, y))
    test(-1.0F, 1.0F)
  }

  it should "add the min macro" in {
    def test(x: Float, y: Float): Unit = (x min y) should equal (java.lang.Math.min(x, y))
    test(-1.0F, 1.0F)
  }

  it should "add the toIntBits macro" in {
    def test(x: Float): Unit = x.toIntBits should equal (java.lang.Float.floatToIntBits(x))
    test(Float.MinPositiveValue)
  }

  it should "add the toRawIntBits macro" in {
    def test(x: Float): Unit = x.toRawIntBits should equal (java.lang.Float.floatToRawIntBits(x))
    test(java.lang.Float.intBitsToFloat(0x7fc00001))
  }
}
