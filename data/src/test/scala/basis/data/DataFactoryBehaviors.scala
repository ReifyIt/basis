//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import org.scalatest._

trait DataFactoryBehaviors { this: FlatSpec =>
  import Matchers._
  import HexMatchers._

  def DataSerializer(Data: DataFactory[Loader]): Unit = {
    def serialize[T](count: Int, offset: Int)(series: Int => T)(implicit T: Frame[T]): Unit = {
      val framer = Data.Framer
      var i = 0
      while (i < offset) {
        framer.writeByte(0.toByte)
        i += 1
      }
      i = 0
      while (i < count) {
        framer.write(series(i))
        i += 1
      }

      val reader = framer.state.reader(0L)
      i = 0
      while (i < offset) {
        reader.readByte() should equalByte (0.toByte)
        i += 1
      }
      i = 0
      try while (i < count) {
        if (reader.read[T]() != series(i)) fail(s"incorrect value $i of $count (offset $offset)")
        i += 1
      }
      catch {
        case e: Exception =>
          info(s"value $i of $count (offset $offset)")
          throw e
      }
    }

    lazy val byteSeries: Int => Byte = i => (~i).toByte

    lazy val shortSeries: Int => Short = i => (~i).toShort

    lazy val intSeries: Int => Int = i => ~i

    lazy val longSeries: Int => Long = i => (~i).toLong

    def serializeBlocks[T : Frame](offset: Int)(series: Int => T): Unit = {
      var k = 3
      while (k <= 23) {
        val count = 1 << k
        serialize(count - 1, offset)(series)
        serialize(count,     offset)(series)
        serialize(count + 1, offset)(series)
        k += 5
      }
    }

    it should "serialize Byte data" in {
      serializeBlocks(offset = 0)(byteSeries)
    }

    it should "serialize Short data" in {
      serializeBlocks(offset = 0)(shortSeries)
      serializeBlocks(offset = 1)(shortSeries)
    }

    it should "serialize Int data" in {
      serializeBlocks(offset = 0)(intSeries)
      serializeBlocks(offset = 3)(intSeries)
    }

    it should "serialize Long data" in {
      serializeBlocks(offset = 0)(longSeries)
      serializeBlocks(offset = 7)(longSeries)
    }

    def concat(a: Int, b: Int, alias: Boolean): Unit = {
      var framer = Data.Framer
      var i = 0
      while (i < a) {
        framer.writeByte((~i).toByte)
        i += 1
      }
      val xs = framer.state

      framer = Data.Framer
      val n = a + b
      while (i < n) {
        framer.writeByte((~i).toByte)
        i += 1
      }
      val ys = framer.state

      framer = Data.Framer
      framer.writeData(xs)
      if (alias) framer.state
      framer.writeData(ys)
      val data = framer.state

      i = 0
      try while (i < n) {
        if (data.loadByte(i) != (~i).toByte) fail(s"incorrect value at address $i of $n")
        i += 1
      }
      catch {
        case e: Exception =>
          info(s"address $i of $n")
          throw e
      }
    }

    def concatBlocks(alias: Boolean): Unit = {
      var k = 3
      while (k <= 23) {
        val n = 1 << k
        concat(1, n, alias)
        concat(n, 1, alias)
        concat(n, n, alias)
        k += 5
      }
    }

    it should "concatenate data" in {
      concatBlocks(alias = false)
    }

    it should "concatenate data with intermediate aliasing" in {
      concatBlocks(alias = true)
    }
  }
}
