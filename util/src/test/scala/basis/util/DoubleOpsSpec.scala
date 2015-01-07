//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.util

import org.scalatest._

class DoubleOpsSpec extends FlatSpec with Matchers {
  override def suiteName = "DoubleOps specification"

  "The DoubleToOps implicit conversion" should "add the abs macro" in {
    def test(x: Double): Unit = x.abs should equal (java.lang.Math.abs(x))
    test(-1.0)
  }

  it should "add the acos macro" in {
    def test(x: Double): Unit = x.acos should equal (java.lang.Math.acos(x))
    test(0.0)
  }

  it should "add the asin macro" in {
    def test(x: Double): Unit = x.asin should equal (java.lang.Math.asin(x))
    test(1.0)
  }

  it should "add the unary atan macro" in {
    def test(x: Double): Unit = x.atan should equal (java.lang.Math.atan(x))
    test(1.0)
  }

  it should "add the binary atan macro" in {
    def test(y: Double, x: Double): Unit = (y atan x) should equal (java.lang.Math.atan2(y, x))
    test(1.0, 0.0)
  }

  it should "add the cbrt macro" in {
    def test(x: Double): Unit = x.cbrt should equal (java.lang.Math.cbrt(x))
    test(8.0)
  }

  it should "add the cos macro" in {
    def test(x: Double): Unit = x.cos should equal (java.lang.Math.cos(x))
    test(0.0)
  }

  it should "add the cosh macro" in {
    def test(x: Double): Unit = x.cosh should equal (java.lang.Math.cosh(x))
    test(0.0)
  }

  it should "add the exp macro" in {
    def test(x: Double): Unit = x.exp should equal (java.lang.Math.exp(x))
    test(0.0)
  }

  it should "add the isInfinite macro" in {
    def test(x: Double): Unit = x.isInfinite should equal (java.lang.Double.isInfinite(x))
    test(Double.NegativeInfinity)
    test(0.0)
    test(Double.PositiveInfinity)
  }

  it should "add the isNaN macro" in {
    def test(x: Double): Unit = x.isNaN should equal (java.lang.Double.isNaN(x))
    test(0.0)
    test(Double.NaN)
  }

  it should "add the log10 macro" in {
    def test(x: Double): Unit = x.log10 should equal (java.lang.Math.log10(x))
    test(1.0)
  }

  it should "add the log macro" in {
    def test(x: Double): Unit = x.log should equal (java.lang.Math.log(x))
    test(1.0)
  }

  it should "add the max macro" in {
    def test(x: Double, y: Double): Unit = (x max y) should equal (java.lang.Math.max(x, y))
    test(-1.0, 1.0)
  }

  it should "add the min macro" in {
    def test(x: Double, y: Double): Unit = (x min y) should equal (java.lang.Math.min(x, y))
    test(-1.0, 1.0)
  }

  it should "add the pow macro" in {
    def test(x: Double, y: Double): Unit = (x pow y) should equal (java.lang.Math.pow(x, y))
    test(2.0, 2.0)
  }

  it should "add the sin macro" in {
    def test(x: Double): Unit = x.sin should equal (java.lang.Math.sin(x))
    test(java.lang.Math.PI)
  }

  it should "add the sinh macro" in {
    def test(x: Double): Unit = x.sinh should equal (java.lang.Math.sinh(x))
    test(1.0)
  }

  it should "add the sqrt macro" in {
    def test(x: Double): Unit = x.sqrt should equal (java.lang.Math.sqrt(x))
    test(2.0)
  }

  it should "add the tan macro" in {
    def test(x: Double): Unit = x.tan should equal (java.lang.Math.tan(x))
    test(0.0)
  }

  it should "add the tanh macro" in {
    def test(x: Double): Unit = x.tanh should equal (java.lang.Math.tanh(x))
    test(1.0)
  }

  it should "add the toLongBits macro" in {
    def test(x: Double): Unit = x.toLongBits should equal (java.lang.Double.doubleToLongBits(x))
    test(Double.MinPositiveValue)
  }

  it should "add the toRawLongBits macro" in {
    def test(x: Double): Unit = x.toRawLongBits should equal (java.lang.Double.doubleToRawLongBits(x))
    test(java.lang.Double.longBitsToDouble(0x7ff8000000000000L))
  }
}
