//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._

trait Writer extends ByteOrder[Endianness] {
  def isEOF: Boolean

  def writeByte(value: Byte): Unit

  def writeShort(value: Short): Unit = {
    if (endian.isBig) {
      writeByte((value >> 8).toByte)
      writeByte((value     ).toByte)
    }
    else if (endian.isLittle) {
      writeByte((value     ).toByte)
      writeByte((value >> 8).toByte)
    }
    else throw new MatchError(endian)
  }

  def writeInt(value: Int): Unit = {
    if (endian.isBig) {
      writeByte((value >> 24).toByte)
      writeByte((value >> 16).toByte)
      writeByte((value >>  8).toByte)
      writeByte((value      ).toByte)
    }
    else if (endian.isLittle) {
      writeByte((value      ).toByte)
      writeByte((value >>  8).toByte)
      writeByte((value >> 16).toByte)
      writeByte((value >> 24).toByte)
    }
    else throw new MatchError(endian)
  }

  def writeLong(value: Long): Unit = {
    if (endian.isBig) {
      writeByte((value >> 56).toByte)
      writeByte((value >> 48).toByte)
      writeByte((value >> 40).toByte)
      writeByte((value >> 32).toByte)
      writeByte((value >> 24).toByte)
      writeByte((value >> 16).toByte)
      writeByte((value >>  8).toByte)
      writeByte((value      ).toByte)
    }
    else if (endian.isLittle) {
      writeByte((value      ).toByte)
      writeByte((value >>  8).toByte)
      writeByte((value >> 16).toByte)
      writeByte((value >> 24).toByte)
      writeByte((value >> 32).toByte)
      writeByte((value >> 40).toByte)
      writeByte((value >> 48).toByte)
      writeByte((value >> 56).toByte)
    }
    else throw new MatchError(endian)
  }

  def writeFloat(value: Float): Unit = writeInt(value.toRawIntBits)

  def writeDouble(value: Double): Unit = writeLong(value.toRawLongBits)

  def writeData(data: Loader): Unit = {
    var p = 0L
    val n = data.size
    while (p < n) {
      writeByte(data.loadByte(p))
      p += 1L
    }
  }
}
