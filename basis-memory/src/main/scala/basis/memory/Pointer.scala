/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.memory

import basis.util._

/** An untyped pointer into a memory region.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  */
abstract class Pointer {
  def align(alignment: Long): Unit
  
  def address: Long
  
  def address_= (newAddress: Long): Unit
  
  def limit: Long
  
  def limit_= (newLimit: Long): Unit
  
  def size: Long
  
  def remaining: Long = 0L max (limit - address)
  
  def flip() {
    limit = address
    address = 0L
  }
  
  def rewind() {
    address = 0L
  }
  
  def clear() {
    address = 0L
    limit = size
  }
  
  def endian: Endianness
  
  def readByte(): Byte
  
  def writeByte(value: Byte): Unit
  
  def readShort(): Short = {
    address &= -2L
    readUnalignedShort()
  }
  
  def writeShort(value: Short) {
    address &= -2L
    writeUnalignedShort(value)
  }
  
  def readInt(): Int = {
    address &= -4L
    readUnalignedInt()
  }
  
  def writeInt(value: Int) {
    address &= -4L
    writeUnalignedInt(value)
  }
  
  def readLong(): Long = {
    address &= -8L
    readUnalignedLong()
  }
  
  def writeLong(value: Long) {
    address &= -8L
    writeUnalignedLong(value)
  }
  
  def readFloat(): Float = readInt().toFloatBits
  
  def writeFloat(value: Float): Unit = writeInt(value.toIntBits)
  
  def readDouble(): Double = readLong().toDoubleBits
  
  def writeDouble(value: Double): Unit = writeLong(value.toLongBits)
  
  def readUnalignedShort(): Short = {
    if (endian eq BigEndian) {
      ((readByte()         << 8) |
       (readByte() & 0xFF)).toShort
    }
    else if (endian eq LittleEndian) {
      ((readByte() & 0xFF)       |
       (readByte()         << 8)).toShort
    }
    else throw new MatchError(endian)
  }
  
  def writeUnalignedShort(value: Short) {
    if (endian eq BigEndian) {
      writeByte((value >> 8).toByte)
      writeByte(value.toByte)
    }
    else if (endian eq LittleEndian) {
      writeByte(value.toByte)
      writeByte((value >> 8).toByte)
    }
    else throw new MatchError(endian)
  }
  
  def readUnalignedInt(): Int = {
    if (endian eq BigEndian) {
       (readByte()         << 24) |
      ((readByte() & 0xFF) << 16) |
      ((readByte() & 0xFF) <<  8) |
       (readByte() & 0xFF)
    }
    else if (endian eq LittleEndian) {
       (readByte() & 0xFF)        |
      ((readByte() & 0xFF) <<  8) |
      ((readByte() & 0xFF) << 16) |
       (readByte()         << 24)
    }
    else throw new MatchError(endian)
  }
  
  def writeUnalignedInt(value: Int) {
    if (endian eq BigEndian) {
      writeByte((value >> 24).toByte)
      writeByte((value >> 16).toByte)
      writeByte((value >>  8).toByte)
      writeByte(value.toByte)
    }
    else if (endian eq LittleEndian) {
      writeByte(value.toByte)
      writeByte((value >>  8).toByte)
      writeByte((value >> 16).toByte)
      writeByte((value >> 24).toByte)
    }
    else throw new MatchError(endian)
  }
  
  def readUnalignedLong(): Long = {
    if (endian eq BigEndian) {
       (readByte().toLong         << 56) |
      ((readByte() & 0xFF).toLong << 48) |
      ((readByte() & 0xFF).toLong << 40) |
      ((readByte() & 0xFF).toLong << 32) |
      ((readByte() & 0xFF).toLong << 24) |
      ((readByte() & 0xFF).toLong << 16) |
      ((readByte() & 0xFF).toLong <<  8) |
       (readByte() & 0xFF).toLong
    }
    else if (endian eq LittleEndian) {
       (readByte() & 0xFF).toLong        |
      ((readByte() & 0xFF).toLong <<  8) |
      ((readByte() & 0xFF).toLong << 16) |
      ((readByte() & 0xFF).toLong << 24) |
      ((readByte() & 0xFF).toLong << 32) |
      ((readByte() & 0xFF).toLong << 40) |
      ((readByte() & 0xFF).toLong << 48) |
       (readByte().toLong         << 56)
    }
    else throw new MatchError(endian)
  }
  
  def writeUnalignedLong(value: Long) {
    if (endian eq BigEndian) {
      writeByte((value >> 56).toByte)
      writeByte((value >> 48).toByte)
      writeByte((value >> 40).toByte)
      writeByte((value >> 32).toByte)
      writeByte((value >> 24).toByte)
      writeByte((value >> 16).toByte)
      writeByte((value >>  8).toByte)
      writeByte(value.toByte)
    }
    else if (endian eq LittleEndian) {
      writeByte(value.toByte)
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
  
  def readUnalignedFloat(): Float =
    readUnalignedInt().toFloatBits
  
  def writeUnalignedFloat(value: Float): Unit =
    writeUnalignedInt(value.toIntBits)
  
  def readUnalignedDouble(): Double =
    readUnalignedLong().toDoubleBits
  
  def writeUnalignedDouble(value: Double): Unit =
    writeUnalignedLong(value.toLongBits)
  
  def read[T](implicit T: Struct[T]): T = T.read(this)
  
  def write[T](value: T)(implicit T: Struct[T]): Unit = T.write(this, value)
}
