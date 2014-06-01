//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._
import org.scalatest._

trait LoaderBehaviors { this: FlatSpec =>
  import Matchers._
  import HexMatchers._

  def BigEndianLoader[Data <: Loader with ByteOrder[BigEndian]](Data: DataFactory[Data]): Unit = {
    def Base16(base16: String): Data = Data.fromBase16(base16)

    it should "load Byte values" in {
      val data = Base16("69")
      data.loadByte(0L) should equalByte (0x69.toByte)
    }

    it should "load aligned Short values" in {
      val data = Base16("6996")
      data.loadAlignedShort(0L) should equalShort (0x6996.toShort)
    }

    it should "load aligned Int values" in {
      val data = Base16("69965AA5")
      data.loadAlignedInt(0L) should equalInt (0x69965AA5)
    }

    it should "load aligned Long values" in {
      val data = Base16("69965AA53CC30FF0")
      data.loadAlignedLong(0L) should equalLong (0x69965AA53CC30FF0L)
    }

    it should "load aligned Float values" in {
      val data = Base16("69965AA5")
      data.loadAlignedFloat(0L).toRawIntBits should equalInt (0x69965AA5)
    }

    it should "load aligned Double values" in {
      val data = Base16("69965AA53CC30FF0")
      data.loadAlignedDouble(0L).toRawLongBits should equalLong (0x69965AA53CC30FF0L)
    }

    it should "load unaligned Short values" in {
      val data = Base16("6996006996")
      var i = 0L
      while (i < 5L) withClue(s"offset $i:") {
        data.loadShort(i) should equalShort (0x6996.toShort)
        i += 3L
      }
    }

    it should "load unaligned Int values" in {
      val data = Base16("69965AA50069965AA50069965AA50069965AA5")
      var i = 0L
      while (i < 19L) withClue(s"offset $i:") {
        data.loadInt(i) should equalInt (0x69965AA5)
        i += 5L
      }
    }

    it should "load unaligned Long values" in {
      val data = Base16("69965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF0")
      var i = 0L
      while (i < 71L) withClue(s"offset $i:") {
        data.loadLong(i) should equalLong (0x69965AA53CC30FF0L)
        i += 9L
      }
    }

    it should "load unaligned Float values" in {
      val data = Base16("69965AA50069965AA50069965AA50069965AA5")
      var i = 0L
      while (i < 19L) withClue(s"offset $i:") {
        data.loadFloat(i).toRawIntBits should equalInt (0x69965AA5)
        i += 5L
      }
    }

    it should "load unaligned Double values" in {
      val data = Base16("69965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF0")
      var i = 0L
      while (i < 71L) withClue(s"offset $i:") {
        data.loadDouble(i).toRawLongBits should equalLong (0x69965AA53CC30FF0L)
        i += 9L
      }
    }

    it should "truncate unaligned addresses when loading aligned Short values" in {
      val data = Base16("6996")
      var i = 0L
      while (i < 2L) withClue(s"offset $i:") {
        data.loadAlignedShort(i) should equalShort(0x6996.toShort)
        i += 1L
      }
    }

    it should "truncate unaligned addresses when loading aligned Int values" in {
      val data = Base16("69965AA5")
      var i = 0L
      while (i < 4L) withClue(s"offset $i:") {
        data.loadAlignedInt(i) should equalInt (0x69965AA5)
        i += 1L
      }
    }

    it should "truncate unaligned addresses when loading aligned Long values" in {
      val data = Base16("69965AA53CC30FF0")
      var i = 0L
      while (i < 8L) withClue(s"offset $i:") {
        data.loadAlignedLong(i) should equalLong (0x69965AA53CC30FF0L)
        i += 1L
      }
    }

    it should "truncate unaligned addresses when loading aligned Float values" in {
      val data = Base16("69965AA5")
      var i = 0L
      while (i < 4L) withClue(s"offset $i:") {
        data.loadAlignedFloat(i).toRawIntBits should equalInt (0x69965AA5)
        i += 1L
      }
    }

    it should "truncate unaligned addresses when loading aligned Double values" in {
      val data = Base16("69965AA53CC30FF0")
      var i = 0L
      while (i < 8L) withClue(s"offset $i:") {
        data.loadAlignedDouble(i).toRawLongBits should equalLong (0x69965AA53CC30FF0L)
        i += 1L
      }
    }
  }


  def LittleEndianLoader[Data <: Loader with ByteOrder[LittleEndian]](Data: DataFactory[Data]): Unit = {
    def Base16(base16: String): Data = Data.fromBase16(base16)

    it should "load Byte values" in {
      val data = Base16("69")
      data.loadByte(0L) should equalByte (0x69.toByte)
    }

    it should "load aligned Short values" in {
      val data = Base16("6996")
      data.loadAlignedShort(0L) should equalShort (0x9669.toShort)
    }

    it should "load aligned Int values" in {
      val data = Base16("69965AA5")
      data.loadAlignedInt(0L) should equalInt (0xA55A9669)
    }

    it should "load aligned Long values" in {
      val data = Base16("69965AA53CC30FF0")
      data.loadAlignedLong(0L) should equalLong (0xF00FC33CA55A9669L)
    }

    it should "load aligned Float values" in {
      val data = Base16("69965AA5")
      data.loadAlignedFloat(0L).toRawIntBits should equalInt (0xA55A9669)
    }

    it should "load aligned Double values" in {
      val data = Base16("69965AA53CC30FF0")
      data.loadAlignedDouble(0L).toRawLongBits should equalLong (0xF00FC33CA55A9669L)
    }

    it should "load unaligned Short values" in {
      val data = Base16("6996006996")
      var i = 0L
      while (i < 5L) withClue(s"offset $i:") {
        data.loadShort(i) should equalShort (0x9669.toShort)
        i += 3L
      }
    }

    it should "load unaligned Int values" in {
      val data = Base16("69965AA50069965AA50069965AA50069965AA5")
      var i = 0L
      while (i < 19L) withClue(s"offset $i:") {
        data.loadInt(i) should equalInt (0xA55A9669)
        i += 5L
      }
    }

    it should "load unaligned Long values" in {
      val data = Base16("69965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF0")
      var i = 0L
      while (i < 71L) withClue(s"offset $i:") {
        data.loadLong(i) should equalLong (0xF00FC33CA55A9669L)
        i += 9L
      }
    }

    it should "load unaligned Float values" in {
      val data = Base16("69965AA50069965AA50069965AA50069965AA5")
      var i = 0L
      while (i < 19L) withClue(s"offset $i:") {
        data.loadFloat(i).toRawIntBits should equalInt (0xA55A9669)
        i += 5L
      }
    }

    it should "load unaligned Double values" in {
      val data = Base16("69965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF0")
      var i = 0L
      while (i < 71L) withClue(s"offset $i:") {
        data.loadDouble(i).toRawLongBits should equalLong (0xF00FC33CA55A9669L)
        i += 9L
      }
    }

    it should "truncate unaligned addresses when loading aligned Short values" in {
      val data = Base16("6996")
      var i = 0L
      while (i < 2L) withClue(s"offset $i:") {
        data.loadAlignedShort(1L) should equalShort(0x9669.toShort)
        i += 1L
      }
    }

    it should "truncate unaligned addresses when loading aligned Int values" in {
      val data = Base16("69965AA5")
      var i = 0L
      while (i < 4L) withClue(s"offset $i:") {
        data.loadAlignedInt(i) should equalInt (0xA55A9669)
        i += 1L
      }
    }

    it should "truncate unaligned addresses when loading aligned Long values" in {
      val data = Base16("69965AA53CC30FF0")
      var i = 0L
      while (i < 8L) withClue(s"offset $i:") {
        data.loadAlignedLong(i) should equalLong (0xF00FC33CA55A9669L)
        i += 1L
      }
    }

    it should "truncate unaligned addresses when loading aligned Float values" in {
      val data = Base16("69965AA5")
      var i = 0L
      while (i < 4L) withClue(s"offset $i:") {
        data.loadAlignedFloat(i).toRawIntBits should equalInt (0xA55A9669)
        i += 1L
      }
    }

    it should "truncate unaligned addresses when loading aligned Double values" in {
      val data = Base16("69965AA53CC30FF0")
      var i = 0L
      while (i < 8L) withClue(s"offset $i:") {
        data.loadAlignedDouble(i).toRawLongBits should equalLong (0xF00FC33CA55A9669L)
        i += 1L
      }
    }
  }
}
