//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._
import org.scalatest._

trait StorerBehaviors { this: FlatSpec =>
  import Matchers._
  import HexMatchers._

  def PrimitiveSerializer[Data <: Loader with Storer](Data: Allocator[Data]): Unit = {
    it should "serialize Byte values" in {
      val data = Data(1L)
      data.storeByte(0L, 0x69.toByte)
      data.loadByte(0L) should equalByte (0x69.toByte)
    }

    it should "serialize aligned Short values" in {
      val data = Data(2L)
      data.storeAlignedShort(0L, 0x6996.toShort)
      data.loadAlignedShort(0L) should equalShort (0x6996.toShort)
    }

    it should "serialize aligned Int values" in {
      val data = Data(4L)
      data.storeAlignedInt(0L, 0x69965AA5)
      data.loadAlignedInt(0L) should equalInt (0x69965AA5)
    }

    it should "serialize aligned Long values" in {
      val data = Data(8L)
      data.storeAlignedLong(0L, 0x69965AA53CC30FF0L)
      data.loadAlignedLong(0L) should equalLong (0x69965AA53CC30FF0L)
    }

    it should "serialize aligned Float values" in {
      val data = Data(4L)
      data.storeAlignedFloat(0L, 0x69965AA5.toFloatBits)
      data.loadAlignedFloat(0L) should equalFloat (0x69965AA5.toFloatBits)
    }

    it should "serialize aligned Double values" in {
      val data = Data(8L)
      data.storeAlignedDouble(0L, 0x69965AA53CC30FF0L.toDoubleBits)
      data.loadAlignedDouble(0L) should equalDouble (0x69965AA53CC30FF0L.toDoubleBits)
    }

    it should "serialize unaligned Short values" in {
      val data = Data(3L)
      data.storeShort(1L, 0x6996.toShort)
      data.loadShort(1L) should equalShort (0x6996.toShort)
    }

    it should "serialize unaligned Int values" in {
      val data = Data(15L)
      var i = 1L
      while (i < 15L) withClue(s"offset $i:") {
        data.storeInt(i, 0x69965AA5)
        data.loadInt(i) should equalInt (0x69965AA5)
        i += 5L
      }
    }

    it should "serialize unaligned Long values" in {
      val data = Data(63L)
      var i = 1L
      while (i < 63L) withClue(s"offset $i:") {
        data.storeLong(i, 0x69965AA53CC30FF0L)
        data.loadLong(i) should equalLong (0x69965AA53CC30FF0L)
        i += 9L
      }
    }

    it should "serialize unaligned Float values" in {
      val data = Data(15L)
      var i = 1L
      while (i < 15L) withClue(s"offset $i:") {
        data.storeFloat(i, 0x69965AA5.toFloatBits)
        data.loadFloat(i).toRawIntBits should equalInt (0x69965AA5)
        i += 5L
      }
    }

    it should "serialize unaligned Double values" in {
      val data = Data(63L)
      var i = 1L
      while (i < 63L) withClue(s"offset $i:") {
        data.storeDouble(i, 0x69965AA53CC30FF0L.toDoubleBits)
        data.loadDouble(i).toRawLongBits should equalLong (0x69965AA53CC30FF0L)
        i += 9L
      }
    }

    it should "truncate unaligned addresses when serializing aligned Short values" in {
      val data = Data(2L)
      data.storeAlignedShort(1L, 0x6996.toShort)
      withClue("unaligned address:") (data.loadAlignedShort(1L) should equalShort(0x6996.toShort))
      withClue("truncated address:") (data.loadAlignedShort(0L) should equalShort(0x6996.toShort))
    }

    it should "truncate unaligned addresses when serializing aligned Int values" in {
      var i = 0L
      while (i < 4L) withClue(s"offset $i:") {
        val data = Data(4L)
        data.storeAlignedInt(i, 0x69965AA5)
        withClue("unaligned address:") (data.loadAlignedInt(i)       should equalInt (0x69965AA5))
        withClue("truncated address:") (data.loadAlignedInt(i & -4L) should equalInt (0x69965AA5))
        i += 1L
      }
    }

    it should "truncate unaligned addresses when serializing aligned Long values" in {
      var i = 0L
      while (i < 8L) withClue(s"offset $i:") {
        val data = Data(8L)
        data.storeAlignedLong(i, 0x69965AA53CC30FF0L)
        withClue("unaligned address:") (data.loadAlignedLong(i)       should equalLong (0x69965AA53CC30FF0L))
        withClue("truncated address:") (data.loadAlignedLong(i & -8L) should equalLong (0x69965AA53CC30FF0L))
        i += 1L
      }
    }

    it should "truncate unaligned addresses when serializing aligned Float values" in {
      var i = 0L
      while (i < 4L) withClue(s"offset $i:") {
        val data = Data(4L)
        data.storeAlignedFloat(i, 0x69965AA5.toFloatBits)
        withClue("unaligned address:") (data.loadAlignedFloat(i).toRawIntBits       should equalInt (0x69965AA5))
        withClue("truncated address:") (data.loadAlignedFloat(i & -4L).toRawIntBits should equalInt (0x69965AA5))
        i += 1L
      }
    }

    it should "truncate unaligned addresses when serializing aligned Double values" in {
      var i = 0L
      while (i < 8L) withClue(s"offset $i:") {
        val data = Data(8L)
        data.storeAlignedDouble(i, 0x69965AA53CC30FF0L.toDoubleBits)
        withClue("unaligned address:") (data.loadAlignedDouble(i).toRawLongBits       should equalLong (0x69965AA53CC30FF0L))
        withClue("truncated address:") (data.loadAlignedDouble(i & -8L).toRawLongBits should equalLong (0x69965AA53CC30FF0L))
        i += 1L
      }
    }
  }


  def BigEndianStorer[Data <: Loader with Storer with ByteOrder[BigEndian]](Data: Allocator[Data]): Unit = {
    it should "store aligned Short values" in {
      val data = Data(2L)
      data.storeAlignedShort(0L, 0x6996.toShort)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0x69.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0x96.toByte))
    }

    it should "store aligned Int values" in {
      val data = Data(4L)
      data.storeAlignedInt(0L, 0x69965AA5)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0x69.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0x96.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0x5A.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xA5.toByte))
    }

    it should "store aligned Long values" in {
      val data = Data(8L)
      data.storeAlignedLong(0L, 0x69965AA53CC30FF0L)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0x69.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0x96.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0x5A.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xA5.toByte))
      withClue("byte 4:") (data.loadByte(4L) should equalByte (0x3C.toByte))
      withClue("byte 5:") (data.loadByte(5L) should equalByte (0xC3.toByte))
      withClue("byte 6:") (data.loadByte(6L) should equalByte (0x0F.toByte))
      withClue("byte 7:") (data.loadByte(7L) should equalByte (0xF0.toByte))
    }

    it should "store aligned Float values" in {
      val data = Data(4L)
      data.storeAlignedFloat(0L, 0x69965AA5.toFloatBits)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0x69.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0x96.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0x5A.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xA5.toByte))
    }

    it should "store aligned Double values" in {
      val data = Data(8L)
      data.storeAlignedDouble(0L, 0x69965AA53CC30FF0L.toDoubleBits)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0x69.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0x96.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0x5A.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0xA5.toByte))
      withClue("byte 4:") (data.loadByte(4L) should equalByte (0x3C.toByte))
      withClue("byte 5:") (data.loadByte(5L) should equalByte (0xC3.toByte))
      withClue("byte 6:") (data.loadByte(6L) should equalByte (0x0F.toByte))
      withClue("byte 7:") (data.loadByte(7L) should equalByte (0xF0.toByte))
    }

    it should "store unaligned Short values" in {
      val data = Data(5L)
      var i = 0L
      while (i < 5L) withClue(s"offset $i:") {
        data.storeShort(i, 0x6996.toShort)
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0x69.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x96.toByte))
        i += 3L
      }
    }

    it should "store unaligned Int values" in {
      val data = Data(19L)
      var i = 0L
      while (i < 15L) withClue(s"offset $i:") {
        data.storeInt(i, 0x69965AA5)
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0x69.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0x5A.toByte))
        withClue(s"byte ${i + 3}:") (data.loadByte(i + 3L) should equalByte (0xA5.toByte))
        i += 5L
      }
    }

    it should "store unaligned Long values" in {
      val data = Data(71L)
      var i = 0L
      while (i < 71L) withClue(s"offset $i:") {
        data.storeLong(i, 0x69965AA53CC30FF0L)
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0x69.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0x5A.toByte))
        withClue(s"byte ${i + 3}:") (data.loadByte(i + 3L) should equalByte (0xA5.toByte))
        withClue(s"byte ${i + 4}:") (data.loadByte(i + 4L) should equalByte (0x3C.toByte))
        withClue(s"byte ${i + 5}:") (data.loadByte(i + 5L) should equalByte (0xC3.toByte))
        withClue(s"byte ${i + 6}:") (data.loadByte(i + 6L) should equalByte (0x0F.toByte))
        withClue(s"byte ${i + 7}:") (data.loadByte(i + 7L) should equalByte (0xF0.toByte))
        i += 9L
      }
    }

    it should "store unaligned Float values" in {
      val data = Data(19L)
      var i = 0L
      while (i < 15L) withClue(s"offset $i:") {
        data.storeFloat(i, 0x69965AA5.toFloatBits)
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0x69.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0x5A.toByte))
        withClue(s"byte ${i + 3}:") (data.loadByte(i + 3L) should equalByte (0xA5.toByte))
        i += 5L
      }
    }

    it should "store unaligned Double values" in {
      val data = Data(71L)
      var i = 0L
      while (i < 71L) withClue(s"offset $i:") {
        data.storeDouble(i, 0x69965AA53CC30FF0L.toDoubleBits)
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0x69.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0x5A.toByte))
        withClue(s"byte ${i + 3}:") (data.loadByte(i + 3L) should equalByte (0xA5.toByte))
        withClue(s"byte ${i + 4}:") (data.loadByte(i + 4L) should equalByte (0x3C.toByte))
        withClue(s"byte ${i + 5}:") (data.loadByte(i + 5L) should equalByte (0xC3.toByte))
        withClue(s"byte ${i + 6}:") (data.loadByte(i + 6L) should equalByte (0x0F.toByte))
        withClue(s"byte ${i + 7}:") (data.loadByte(i + 7L) should equalByte (0xF0.toByte))
        i += 9L
      }
    }
  }


  def LittleEndianStorer[Data <: Loader with Storer with ByteOrder[LittleEndian]](Data: Allocator[Data]): Unit = {
    it should "store aligned Short values" in {
      val data = Data(2L)
      data.storeAlignedShort(0L, 0x6996.toShort)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0x96.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0x69.toByte))
    }

    it should "store aligned Int values" in {
      val data = Data(4L)
      data.storeAlignedInt(0L, 0x69965AA5)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xA5.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0x5A.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0x96.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0x69.toByte))
    }

    it should "store aligned Long values" in {
      val data = Data(8L)
      data.storeAlignedLong(0L, 0x69965AA53CC30FF0L)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF0.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0x0F.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xC3.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0x3C.toByte))
      withClue("byte 4:") (data.loadByte(4L) should equalByte (0xA5.toByte))
      withClue("byte 5:") (data.loadByte(5L) should equalByte (0x5A.toByte))
      withClue("byte 6:") (data.loadByte(6L) should equalByte (0x96.toByte))
      withClue("byte 7:") (data.loadByte(7L) should equalByte (0x69.toByte))
    }

    it should "store aligned Float values" in {
      val data = Data(4L)
      data.storeAlignedFloat(0L, 0x69965AA5.toFloatBits)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xA5.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0x5A.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0x96.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0x69.toByte))
    }

    it should "store aligned Double values" in {
      val data = Data(8L)
      data.storeAlignedDouble(0L, 0x69965AA53CC30FF0L.toDoubleBits)
      withClue("byte 0:") (data.loadByte(0L) should equalByte (0xF0.toByte))
      withClue("byte 1:") (data.loadByte(1L) should equalByte (0x0F.toByte))
      withClue("byte 2:") (data.loadByte(2L) should equalByte (0xC3.toByte))
      withClue("byte 3:") (data.loadByte(3L) should equalByte (0x3C.toByte))
      withClue("byte 4:") (data.loadByte(4L) should equalByte (0xA5.toByte))
      withClue("byte 5:") (data.loadByte(5L) should equalByte (0x5A.toByte))
      withClue("byte 6:") (data.loadByte(6L) should equalByte (0x96.toByte))
      withClue("byte 7:") (data.loadByte(7L) should equalByte (0x69.toByte))
    }

    it should "store unaligned Short values" in {
      val data = Data(5L)
      var i = 0L
      while (i < 5L) withClue(s"offset $i:") {
        data.storeShort(i, 0x6996.toShort)
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0x96.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x69.toByte))
        i += 3L
      }
    }

    it should "store unaligned Int values" in {
      val data = Data(19L)
      var i = 0L
      while (i < 15L) withClue(s"offset $i:") {
        data.storeInt(i, 0x69965AA5)
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0xA5.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x5A.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 3}:") (data.loadByte(i + 3L) should equalByte (0x69.toByte))
        i += 5L
      }
    }

    it should "store unaligned Long values" in {
      val data = Data(71L)
      var i = 0L
      while (i < 71L) withClue(s"offset $i:") {
        data.storeLong(i, 0x69965AA53CC30FF0L)
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0xF0.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x0F.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0xC3.toByte))
        withClue(s"byte ${i + 3}:") (data.loadByte(i + 3L) should equalByte (0x3C.toByte))
        withClue(s"byte ${i + 4}:") (data.loadByte(i + 4L) should equalByte (0xA5.toByte))
        withClue(s"byte ${i + 5}:") (data.loadByte(i + 5L) should equalByte (0x5A.toByte))
        withClue(s"byte ${i + 6}:") (data.loadByte(i + 6L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 7}:") (data.loadByte(i + 7L) should equalByte (0x69.toByte))
        i += 9L
      }
    }

    it should "store unaligned Float values" in {
      val data = Data(19L)
      var i = 0L
      while (i < 15L) withClue(s"offset $i:") {
        data.storeFloat(i, 0x69965AA5.toFloatBits)
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0xA5.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x5A.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 3}:") (data.loadByte(i + 3L) should equalByte (0x69.toByte))
        i += 5L
      }
    }

    it should "store unaligned Double values" in {
      val data = Data(71L)
      var i = 0L
      while (i < 71L) withClue(s"offset $i:") {
        data.storeDouble(i, 0x69965AA53CC30FF0L.toDoubleBits)
        withClue(s"byte $i:")       (data.loadByte(i)      should equalByte (0xF0.toByte))
        withClue(s"byte ${i + 1}:") (data.loadByte(i + 1L) should equalByte (0x0F.toByte))
        withClue(s"byte ${i + 2}:") (data.loadByte(i + 2L) should equalByte (0xC3.toByte))
        withClue(s"byte ${i + 3}:") (data.loadByte(i + 3L) should equalByte (0x3C.toByte))
        withClue(s"byte ${i + 4}:") (data.loadByte(i + 4L) should equalByte (0xA5.toByte))
        withClue(s"byte ${i + 5}:") (data.loadByte(i + 5L) should equalByte (0x5A.toByte))
        withClue(s"byte ${i + 6}:") (data.loadByte(i + 6L) should equalByte (0x96.toByte))
        withClue(s"byte ${i + 7}:") (data.loadByte(i + 7L) should equalByte (0x69.toByte))
        i += 9L
      }
    }
  }
}
