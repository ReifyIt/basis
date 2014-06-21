//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._

trait Reader extends ByteOrder[Endianness] {
  def isEOF: Boolean

  def readByte(): Byte

  def readShort(): Short = {
    if (endian.isBig)
      ((readByte()       ) << 8) |
      ((readByte() & 0xFF)     )
    else if (endian.isLittle)
      ((readByte() & 0xFF)     ) |
      ((readByte()       ) << 8)
    else throw new MatchError(endian)
  }.toShort

  def readInt(): Int = {
    if (endian.isBig)
      ((readByte()       ) << 24) |
      ((readByte() & 0xFF) << 16) |
      ((readByte() & 0xFF) <<  8) |
      ((readByte() & 0xFF)      )
    else if (endian.isLittle)
      ((readByte() & 0xFF)      ) |
      ((readByte() & 0xFF) <<  8) |
      ((readByte() & 0xFF) << 16) |
      ((readByte()       ) << 24)
    else throw new MatchError(endian)
  }

  def readLong(): Long = {
    if (endian.isBig)
      ((readByte()       ).toLong << 56) |
      ((readByte() & 0xFF).toLong << 48) |
      ((readByte() & 0xFF).toLong << 40) |
      ((readByte() & 0xFF).toLong << 32) |
      ((readByte() & 0xFF).toLong << 24) |
      ((readByte() & 0xFF).toLong << 16) |
      ((readByte() & 0xFF).toLong <<  8) |
      ((readByte() & 0xFF).toLong      )
    else if (endian.isLittle)
      ((readByte() & 0xFF).toLong      ) |
      ((readByte() & 0xFF).toLong <<  8) |
      ((readByte() & 0xFF).toLong << 16) |
      ((readByte() & 0xFF).toLong << 24) |
      ((readByte() & 0xFF).toLong << 32) |
      ((readByte() & 0xFF).toLong << 40) |
      ((readByte() & 0xFF).toLong << 48) |
      ((readByte()       ).toLong << 56)
    else throw new MatchError(endian)
  }

  def readFloat(): Float = readInt().toFloatBits

  def readDouble(): Double = readLong().toDoubleBits

  def drop(lower: Long): Reader with ByteOrder[Endian] = {
    var i = 0L
    while (i < lower && !isEOF) {
      readByte()
      i += 1L
    }
    this
  }

  def take(upper: Long): Reader with ByteOrder[Endian] =
    new Reader.Limited(this, 0L, 0L max upper).asInstanceOf[Reader with ByteOrder[Endian]]
}

private[data] object Reader {
  private[data] final class Limited(
      private[this] val self: Reader,
      private[this] var index: Long,
      private[this] val limit: Long)
    extends Reader {

    override def endian: Endianness = self.endian

    override def isEOF: Boolean = self.isEOF || index >= limit

    override def readByte(): Byte = {
      if (index + 1L > limit) throw new IndexOutOfBoundsException
      val value = self.readByte()
      index += 1L
      value
    }

    override def readShort(): Short = {
      if (index + 2L > limit) throw new IndexOutOfBoundsException
      val value = self.readShort()
      index += 2L
      value
    }

    override def readInt(): Int = {
      if (index + 4L > limit) throw new IndexOutOfBoundsException
      val value = self.readInt()
      index += 4L
      value
    }

    override def readLong(): Long = {
      if (index + 8L > limit) throw new IndexOutOfBoundsException
      val value = self.readLong()
      index += 8L
      value
    }

    override def readFloat(): Float = {
      if (index + 4L > limit) throw new IndexOutOfBoundsException
      val value = self.readFloat()
      index += 4L
      value
    }

    override def readDouble(): Double = {
      if (index + 8L > limit) throw new IndexOutOfBoundsException
      val value = self.readDouble()
      index += 8L
      value
    }

    override def drop(lower: Long): Reader with ByteOrder[Endian] = {
      index = (index + lower) min limit
      this
    }

    override def take(upper: Long): Reader with ByteOrder[Endian] =
      new Limited(self, index, (index + upper) min limit)
  }
}
