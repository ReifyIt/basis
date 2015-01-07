//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._
import org.scalatest._

trait ReaderBehaviors { this: FlatSpec =>
  import Matchers._
  import HexMatchers._

  def BigEndianReader(Data: DataFactory[Loader with ByteOrder[BigEndian]]): Unit = {
    def Base16(base16: String): Reader with ByteOrder[BigEndian] = Data.fromBase16(base16).reader(0L)

    it should "declare itself big-endian" in {
      Data.empty.reader(0L).endian should equal (BigEndian)
    }

    it should "read Byte values" in {
      val data = Base16("69")
      data.readByte() should equalByte (0x69.toByte)
    }

    it should "read Short values" in {
      val data = Base16("699600699600")
      var i = 0L
      while (i < 6L) withClue(s"offset $i:") {
        data.readShort() should equalShort (0x6996.toShort)
        data.readByte() should equalByte (0.toByte)
        i += 3L
      }
    }

    it should "read Int values" in {
      val data = Base16("69965AA50069965AA50069965AA50069965AA500")
      var i = 0L
      while (i < 20L) withClue(s"offset $i:") {
        data.readInt() should equalInt (0x69965AA5)
        data.readByte() should equalByte (0.toByte)
        i += 5L
      }
    }

    it should "read Long values" in {
      val data = Base16("69965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF000")
      var i = 0L
      while (i < 72L) withClue(s"offset $i:") {
        data.readLong() should equalLong (0x69965AA53CC30FF0L)
        data.readByte() should equalByte (0.toByte)
        i += 9L
      }
    }

    it should "read Float values" in {
      val data = Base16("69965AA50069965AA50069965AA50069965AA500")
      var i = 0L
      while (i < 20L) withClue(s"offset $i:") {
        data.readFloat().toRawIntBits should equalInt (0x69965AA5)
        data.readByte() should equalByte (0.toByte)
        i += 5L
      }
    }

    it should "read Double values" in {
      val data = Base16("69965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF000")
      var i = 0L
      while (i < 72L) withClue(s"offset $i:") {
        data.readDouble().toRawLongBits should equalLong (0x69965AA53CC30FF0L)
        data.readByte() should equalByte (0.toByte)
        i += 9L
      }
    }
  }


  def LittleEndianReader(Data: DataFactory[Loader with ByteOrder[LittleEndian]]): Unit = {
    def Base16(base16: String): Reader with ByteOrder[LittleEndian] = Data.fromBase16(base16).reader(0L)

    it should "declare itself little-endian" in {
      Data.empty.reader(0L).endian should equal (LittleEndian)
    }

    it should "read Byte values" in {
      val data = Base16("69")
      data.readByte() should equalByte (0x69.toByte)
    }

    it should "read Short values" in {
      val data = Base16("699600699600")
      var i = 0L
      while (i < 6L) withClue(s"offset $i:") {
        data.readShort() should equalShort (0x9669.toShort)
        data.readByte() should equalByte (0.toByte)
        i += 3L
      }
    }

    it should "read Int values" in {
      val data = Base16("69965AA50069965AA50069965AA50069965AA500")
      var i = 0L
      while (i < 20L) withClue(s"offset $i:") {
        data.readInt() should equalInt (0xA55A9669)
        data.readByte() should equalByte (0.toByte)
        i += 5L
      }
    }

    it should "read Long values" in {
      val data = Base16("69965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF000")
      var i = 0L
      while (i < 72L) withClue(s"offset $i:") {
        data.readLong() should equalLong (0xF00FC33CA55A9669L)
        data.readByte() should equalByte (0.toByte)
        i += 9L
      }
    }

    it should "read Float values" in {
      val data = Base16("69965AA50069965AA50069965AA50069965AA500")
      var i = 0L
      while (i < 20L) withClue(s"offset $i:") {
        data.readFloat().toRawIntBits should equalInt (0xA55A9669)
        data.readByte() should equalByte (0.toByte)
        i += 5L
      }
    }

    it should "read Double values" in {
      val data = Base16("69965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF00069965AA53CC30FF000")
      var i = 0L
      while (i < 72L) withClue(s"offset $i:") {
        data.readDouble().toRawLongBits should equalLong (0xF00FC33CA55A9669L)
        data.readByte() should equalByte (0.toByte)
        i += 9L
      }
    }
  }
}
