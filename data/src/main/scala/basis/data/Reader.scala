//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._

trait Reader extends ByteOrder[Endianness] {
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
}
