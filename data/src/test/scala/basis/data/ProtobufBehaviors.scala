//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.collections._
import org.scalatest._
import org.scalatest.matchers._

trait ProtobufBehaviors { this: FlatSpec =>
  import Matchers._
  import Protobuf._

  def ProtobufTranscoder[Data <: Loader](Data: DataFactory[Data]): Unit = {
    def transcoder[T](implicit T: Protobuf[T]): Matcher[T] = new Transcoder()(T, Data)

    def testVarint(transcode: Matcher[Long]): Unit = {
      var k = 0
      while (k < 64) {
        val n = 1L << k
        n should (transcode)
        (n + 1) should (transcode)
        (n - 1) should (transcode)
        k += 1
      }
      Int.MaxValue.toLong should (transcode)
      Int.MinValue.toLong should (transcode)
      Long.MaxValue should (transcode)
      Long.MinValue should (transcode)
    }

    it should "transcode varint protobuf values" in {
      testVarint(transcoder(Varint))
    }

    it should "transcode varint protobuf fields" in {
      testVarint(transcoder(Required(1)(Varint)))
    }

    def testSInt32(transcode: Matcher[Int]): Unit = {
      0 should (transcode)
      1 should (transcode)
      2 should (transcode)
      -1 should (transcode)
      -2 should (transcode)
      Int.MaxValue should (transcode)
      Int.MinValue should (transcode)
    }

    it should "transcode SInt32 protobuf values" in {
      testSInt32(transcoder(SInt32))
    }

    it should "transcode SInt32 protobuf fields" in {
      testSInt32(transcoder(Required(1)(SInt32)))
    }

    def testSInt64(transcode: Matcher[Long]): Unit = {
      0L should (transcode)
      1L should (transcode)
      2L should (transcode)
      -1L should (transcode)
      -2L should (transcode)
      (Long.MaxValue >> 1) should (transcode)
      (Long.MinValue >> 1) should (transcode)
    }

    it should "transcode SInt64 protobuf values" in {
      testSInt64(transcoder(SInt64))
    }

    it should "transcode SInt64 protobuf fields" in {
      testSInt64(transcoder(Required(1)(SInt64)))
    }

    def testSFixed32(transcode: Matcher[Int]): Unit = {
      0 should (transcode)
      1 should (transcode)
      -1 should (transcode)
      Int.MaxValue should (transcode)
      Int.MinValue should (transcode)
    }

    it should "transcode SFixed32 protobuf values" in {
      testSFixed32(transcoder(SFixed32))
    }

    it should "transcode SFixed32 protobuf fields" in {
      testSFixed32(transcoder(Required(1)(SFixed32)))
    }

    def testSFixed64(transcode: Matcher[Long]): Unit = {
      0L should (transcode)
      1L should (transcode)
      -1L should (transcode)
      Long.MaxValue should (transcode)
      Long.MinValue should (transcode)
    }

    it should "transcode SFixed64 protobuf values" in {
      testSFixed64(transcoder(SFixed64))
    }

    it should "transcode SFixed64 protobuf fields" in {
      testSFixed64(transcoder(Required(1)(SFixed64)))
    }

    def testFloat(transcode: Matcher[Float]): Unit = {
      0.0F should (transcode)
      scala.Float.MinPositiveValue should (transcode)
      scala.Float.PositiveInfinity should (transcode)
    }

    it should "transcode Float protobuf values" in {
      testFloat(transcoder(Float))
    }

    it should "transcode Float protobuf fields" in {
      testFloat(transcoder(Required(1)(Float)))
    }

    def testDouble(transcode: Matcher[Double]): Unit = {
      0.0 should (transcode)
      scala.Double.MinPositiveValue should (transcode)
      scala.Double.PositiveInfinity should (transcode)
    }

    it should "transcode Double protobuf values" in {
      testDouble(transcoder(Double))
    }

    it should "transcode Double protobuf fields" in {
      testDouble(transcoder(Required(1)(Double)))
    }

    def testBool(transcode: Matcher[Boolean]): Unit = {
      true should (transcode)
      false should (transcode)
    }

    it should "transcode Bool protobuf values" in {
      testBool(transcoder(Bool))
    }

    it should "transcode Bool protobuf fields" in {
      testBool(transcoder(Required(1)(Bool)))
    }

    def testString(transcode: Matcher[String]): Unit = {
      "" should (transcode)
      "Hello, world!" should (transcode)
    }

    it should "transcode String protobuf values" in {
      testString(transcoder(String))
    }

    it should "transcode String protobuf fields" in {
      testString(transcoder(Required(1)(String)))
    }

    def testRepeated(transcode: Matcher[Seq[Long]]): Unit = {
      Seq.empty should (transcode)
      Seq(0L) should (transcode)
      Seq(1L, -1L) should (transcode)
      Seq(Int.MaxValue, Int.MinValue, Long.MaxValue, Long.MinValue) should (transcode)
    }

    it should "transcode packed repeated protobuf values" in {
      testRepeated(transcoder(Repeated(Seq, Varint)))
    }

    it should "transcode packed repeated protobuf fields" in {
      testRepeated(transcoder(Required(1)(Repeated(Seq, Varint))))
    }
  }

  protected final class Transcoder[T, Data <: Loader](implicit T: Protobuf[T], Data: DataFactory[Data]) extends Matcher[T] {
    private def transcoded(value: T): T = {
      val framer = Data.Framer
      T.write(framer, value)
      T.read(framer.state.reader(0L))
    }

    def apply(x: T): MatchResult = {
      val y = transcoded(x)
      MatchResult(
        x == y,
        s"$x improperly transcoded to $y",
        s"$x properly transcoded")
    }
  }
}
