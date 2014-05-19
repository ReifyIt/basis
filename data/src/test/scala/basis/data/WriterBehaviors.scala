//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._
import org.scalatest._

trait WriterBehaviors { this: FlatSpec =>
  import Matchers._
  import HexMatchers._

  def BigEndianWriter(Data: ByteFactory[Loader with ByteOrder[BigEndian]]): Unit = {
    it should "write Byte values" in {
      val framer = Data.Framer
      framer.writeByte(0x69.toByte)

      val data = framer.state
      data.loadByte(0L) should equalByte (0x69.toByte)
    }

    it should "write Short values" in {
      val framer = Data.Framer
      var i = 0L
      while (i < 6L) {
        framer.writeShort(0x6996.toShort)
        framer.writeByte(0.toByte)
        i += 3L
      }

      val data = framer.state
      i = 0L
      while (i < 6L) withClue(s"offset $i:") {
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0x69.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0.toByte))
        i += 3L
      }
    }

    it should "write Int values" in {
      val framer = Data.Framer
      var i = 0L
      while (i < 20L) {
        framer.writeInt(0x69965AA5)
        framer.writeByte(0.toByte)
        i += 5L
      }

      val data = framer.state
      i = 0L
      while (i < 20L) withClue(s"offset $i:") {
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0x69.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0x5A.toByte))
        withClue(s"byte ${i + 3}:") (data.loadByte(i + 3L) should equalByte (0xA5.toByte))
        withClue(s"byte ${i + 4}:") (data.loadByte(i + 4L) should equalByte (0.toByte))
        i += 5L
      }
    }

    it should "write Long values" in {
      val framer = Data.Framer
      var i = 0L
      while (i < 72L) {
        framer.writeLong(0x69965AA53CC30FF0L)
        framer.writeByte(0.toByte)
        i += 9L
      }

      val data = framer.state
      i = 0L
      while (i < 72L) withClue(s"offset $i:") {
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0x69.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0x5A.toByte))
        withClue(s"byte ${i + 3}:") (data.loadByte(i + 3L) should equalByte (0xA5.toByte))
        withClue(s"byte ${i + 4}:") (data.loadByte(i + 4L) should equalByte (0x3C.toByte))
        withClue(s"byte ${i + 5}:") (data.loadByte(i + 5L) should equalByte (0xC3.toByte))
        withClue(s"byte ${i + 6}:") (data.loadByte(i + 6L) should equalByte (0x0F.toByte))
        withClue(s"byte ${i + 7}:") (data.loadByte(i + 7L) should equalByte (0xF0.toByte))
        withClue(s"byte ${i + 8}:") (data.loadByte(i + 8L) should equalByte (0.toByte))
        i += 9L
      }
    }

    it should "write Float values" in {
      val framer = Data.Framer
      var i = 0L
      while (i < 20L) {
        framer.writeFloat(0x69965AA5.toFloatBits)
        framer.writeByte(0.toByte)
        i += 5L
      }

      val data = framer.state
      i = 0L
      while (i < 20L) withClue(s"offset $i:") {
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0x69.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0x5A.toByte))
        withClue(s"byte ${i + 3}:") (data.loadByte(i + 3L) should equalByte (0xA5.toByte))
        withClue(s"byte ${i + 4}:") (data.loadByte(i + 4L) should equalByte (0.toByte))
        i += 5L
      }
    }

    it should "write Double values" in {
      val framer = Data.Framer
      var i = 0L
      while (i < 72L) {
        framer.writeDouble(0x69965AA53CC30FF0L.toDoubleBits)
        framer.writeByte(0.toByte)
        i += 9L
      }

      val data = framer.state
      i = 0L
      while (i < 72L) withClue(s"offset $i:") {
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0x69.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0x5A.toByte))
        withClue(s"byte ${i + 3}:") (data.loadByte(i + 3L) should equalByte (0xA5.toByte))
        withClue(s"byte ${i + 4}:") (data.loadByte(i + 4L) should equalByte (0x3C.toByte))
        withClue(s"byte ${i + 5}:") (data.loadByte(i + 5L) should equalByte (0xC3.toByte))
        withClue(s"byte ${i + 6}:") (data.loadByte(i + 6L) should equalByte (0x0F.toByte))
        withClue(s"byte ${i + 7}:") (data.loadByte(i + 7L) should equalByte (0xF0.toByte))
        withClue(s"byte ${i + 8}:") (data.loadByte(i + 8L) should equalByte (0.toByte))
        i += 9L
      }
    }
  }


  def LittleEndianWriter(Data: ByteFactory[Loader with ByteOrder[LittleEndian]]): Unit = {
    it should "write Byte values" in {
      val framer = Data.Framer
      framer.writeByte(0x69.toByte)
      val data = framer.state
      data.loadByte(0L) should equalByte (0x69.toByte)
    }

    it should "write Short values" in {
      val framer = Data.Framer
      var i = 0L
      while (i < 6L) {
        framer.writeShort(0x6996.toShort)
        framer.writeByte(0.toByte)
        i += 3L
      }

      val data = framer.state
      i = 0L
      while (i < 6L) withClue(s"offset $i:") {
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0x96.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x69.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0.toByte))
        i += 3L
      }
    }

    it should "write Int values" in {
      val framer = Data.Framer
      var i = 0L
      while (i < 20L) {
        framer.writeInt(0x69965AA5)
        framer.writeByte(0.toByte)
        i += 5L
      }

      val data = framer.state
      i = 0L
      while (i < 20L) withClue(s"offset $i:") {
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0xA5.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x5A.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 3}:") (data.loadByte(i + 3L) should equalByte (0x69.toByte))
        withClue(s"byte ${i + 4}:") (data.loadByte(i + 4L) should equalByte (0.toByte))
        i += 5L
      }
    }

    it should "write Long values" in {
      val framer = Data.Framer
      var i = 0L
      while (i < 72L) {
        framer.writeLong(0x69965AA53CC30FF0L)
        framer.writeByte(0.toByte)
        i += 9L
      }

      val data = framer.state
      i = 0L
      while (i < 72L) withClue(s"offset $i:") {
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0xF0.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x0F.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0xC3.toByte))
        withClue(s"byte ${i + 3}:") (data.loadByte(i + 3L) should equalByte (0x3C.toByte))
        withClue(s"byte ${i + 4}:") (data.loadByte(i + 4L) should equalByte (0xA5.toByte))
        withClue(s"byte ${i + 5}:") (data.loadByte(i + 5L) should equalByte (0x5A.toByte))
        withClue(s"byte ${i + 6}:") (data.loadByte(i + 6L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 7}:") (data.loadByte(i + 7L) should equalByte (0x69.toByte))
        withClue(s"byte ${i + 8}:") (data.loadByte(i + 8L) should equalByte (0.toByte))
        i += 9L
      }
    }

    it should "write Float values" in {
      val framer = Data.Framer
      var i = 0L
      while (i < 20L) {
        framer.writeFloat(0x69965AA5.toFloatBits)
        framer.writeByte(0.toByte)
        i += 5L
      }

      val data = framer.state
      i = 0L
      while (i < 20L) withClue(s"offset $i:") {
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0xA5.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x5A.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 3}:") (data.loadByte(i + 3L) should equalByte (0x69.toByte))
        withClue(s"byte ${i + 4}:") (data.loadByte(i + 4L) should equalByte (0.toByte))
        i += 5L
      }
    }

    it should "write Double values" in {
      val framer = Data.Framer
      var i = 0L
      while (i < 72L) {
        framer.writeDouble(0x69965AA53CC30FF0L.toDoubleBits)
        framer.writeByte(0.toByte)
        i += 9L
      }

      val data = framer.state
      i = 0L
      while (i < 72L) withClue(s"offset $i:") {
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0xF0.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x0F.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0xC3.toByte))
        withClue(s"byte ${i + 3}:") (data.loadByte(i + 3L) should equalByte (0x3C.toByte))
        withClue(s"byte ${i + 4}:") (data.loadByte(i + 4L) should equalByte (0xA5.toByte))
        withClue(s"byte ${i + 5}:") (data.loadByte(i + 5L) should equalByte (0x5A.toByte))
        withClue(s"byte ${i + 6}:") (data.loadByte(i + 6L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 7}:") (data.loadByte(i + 7L) should equalByte (0x69.toByte))
        withClue(s"byte ${i + 8}:") (data.loadByte(i + 8L) should equalByte (0.toByte))
        i += 9L
      }
    }
  }
}