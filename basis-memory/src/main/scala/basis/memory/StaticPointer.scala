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
abstract class StaticPointer {
  def endian: Endianness
  
  def loadByte(offset: Long): Byte
  
  def storeByte(offset: Long, value: Byte): Unit
  
  def loadShort(offset: Long): Short =
    loadUnalignedShort(offset & -2L)
  
  def storeShort(offset: Long, value: Short): Unit =
    storeUnalignedShort(offset & -2L, value)
  
  def loadInt(offset: Long): Int =
    loadUnalignedInt(offset & -4L)
  
  def storeInt(offset: Long, value: Int): Unit =
    storeUnalignedInt(offset & -4L, value)
  
  def loadLong(offset: Long): Long =
    loadUnalignedLong(offset & -8L)
  
  def storeLong(offset: Long, value: Long): Unit =
    storeUnalignedLong(offset & -8L, value)
  
  def loadFloat(offset: Long): Float =
    loadUnalignedFloat(offset & -4L)
  
  def storeFloat(offset: Long, value: Float): Unit =
    storeUnalignedFloat(offset & -4L, value)
  
  def loadDouble(offset: Long): Double =
    loadUnalignedDouble(offset & -8L)
  
  def storeDouble(offset: Long, value: Double): Unit =
    storeUnalignedDouble(offset & -8L, value)
  
  def loadUnalignedShort(offset: Long): Short = {
    if (endian eq BigEndian) {
      ((loadByte(offset)              << 8) |
       (loadByte(offset + 1L) & 0xFF)).toShort
    }
    else if (endian eq LittleEndian) {
      ((loadByte(offset)      & 0xFF)       |
       (loadByte(offset + 1L)         << 8)).toShort
    }
    else throw new MatchError(endian)
  }
  
  def storeUnalignedShort(offset: Long, value: Short) {
    if (endian eq BigEndian) {
      storeByte(offset,      (value >> 8).toByte)
      storeByte(offset + 1L,  value.toByte)
    }
    else if (endian eq LittleEndian) {
      storeByte(offset,       value.toByte)
      storeByte(offset + 1L, (value >> 8).toByte)
    }
    else throw new MatchError(endian)
  }
  
  def loadUnalignedInt(offset: Long): Int = {
    if (endian eq BigEndian) {
       (loadByte(offset)              << 24) |
      ((loadByte(offset + 1L) & 0xFF) << 16) |
      ((loadByte(offset + 2L) & 0xFF) <<  8) |
       (loadByte(offset + 3L) & 0xFF)
    }
    else if (endian eq LittleEndian) {
       (loadByte(offset)      & 0xFF)        |
      ((loadByte(offset + 1L) & 0xFF) <<  8) |
      ((loadByte(offset + 2L) & 0xFF) << 16) |
       (loadByte(offset + 3L)         << 24)
    }
    else throw new MatchError(endian)
  }
  
  def storeUnalignedInt(offset: Long, value: Int) {
    if (endian eq BigEndian) {
      storeByte(offset,      (value >> 24).toByte)
      storeByte(offset + 1L, (value >> 16).toByte)
      storeByte(offset + 2L, (value >>  8).toByte)
      storeByte(offset + 3L,  value.toByte)
    }
    else if (endian eq LittleEndian) {
      storeByte(offset,       value.toByte)
      storeByte(offset + 1L, (value >>  8).toByte)
      storeByte(offset + 2L, (value >> 16).toByte)
      storeByte(offset + 3L, (value >> 24).toByte)
    }
    else throw new MatchError(endian)
  }
  
  def loadUnalignedLong(offset: Long): Long = {
    if (endian eq BigEndian) {
       (loadByte(offset).toLong              << 56) |
      ((loadByte(offset + 1L) & 0xFF).toLong << 48) |
      ((loadByte(offset + 2L) & 0xFF).toLong << 40) |
      ((loadByte(offset + 3L) & 0xFF).toLong << 32) |
      ((loadByte(offset + 4L) & 0xFF).toLong << 24) |
      ((loadByte(offset + 5L) & 0xFF).toLong << 16) |
      ((loadByte(offset + 6L) & 0xFF).toLong <<  8) |
       (loadByte(offset + 7L) & 0xFF).toLong
    }
    else if (endian eq LittleEndian) {
       (loadByte(offset)      & 0xFF).toLong        |
      ((loadByte(offset + 1L) & 0xFF).toLong <<  8) |
      ((loadByte(offset + 2L) & 0xFF).toLong << 16) |
      ((loadByte(offset + 3L) & 0xFF).toLong << 24) |
      ((loadByte(offset + 4L) & 0xFF).toLong << 32) |
      ((loadByte(offset + 5L) & 0xFF).toLong << 40) |
      ((loadByte(offset + 6L) & 0xFF).toLong << 48) |
       (loadByte(offset + 7L).toLong         << 56)
    }
    else throw new MatchError(endian)
  }
  
  def storeUnalignedLong(offset: Long, value: Long) {
    if (endian eq BigEndian) {
      storeByte(offset,      (value >> 56).toByte)
      storeByte(offset + 1L, (value >> 48).toByte)
      storeByte(offset + 2L, (value >> 40).toByte)
      storeByte(offset + 3L, (value >> 32).toByte)
      storeByte(offset + 4L, (value >> 24).toByte)
      storeByte(offset + 5L, (value >> 16).toByte)
      storeByte(offset + 6L, (value >>  8).toByte)
      storeByte(offset + 7L,  value.toByte)
    }
    else if (endian eq LittleEndian) {
      storeByte(offset,       value.toByte)
      storeByte(offset + 1L, (value >>  8).toByte)
      storeByte(offset + 2L, (value >> 16).toByte)
      storeByte(offset + 3L, (value >> 24).toByte)
      storeByte(offset + 4L, (value >> 32).toByte)
      storeByte(offset + 5L, (value >> 40).toByte)
      storeByte(offset + 6L, (value >> 48).toByte)
      storeByte(offset + 7L, (value >> 56).toByte)
    }
    else throw new MatchError(endian)
  }
  
  def loadUnalignedFloat(offset: Long): Float =
    loadUnalignedInt(offset).toFloatBits
  
  def storeUnalignedFloat(offset: Long, value: Float): Unit =
    storeUnalignedInt(offset, value.toIntBits)
  
  def loadUnalignedDouble(offset: Long): Double =
    loadUnalignedLong(offset).toDoubleBits
  
  def storeUnalignedDouble(offset: Long, value: Double): Unit =
    storeUnalignedLong(offset, value.toLongBits)
  
  def load[T](offset: Long)(implicit T: Struct[T]): T =
    T.load(this, offset)
  
  def store[T](offset: Long, value: T)(implicit T: Struct[T]): Unit =
    T.store(this, offset, value)
  
  def loadArray[T]
      (offset: Long, count: Int)
      (implicit T: Struct[T])
    : Array[T] = {
    val array = T.newArray(count)
    loadToArray[T](offset, array, 0, count)
    array
  }
  
  def loadToArray[T]
      (offset: Long, array: Array[T], start: Int, count: Int)
      (implicit T: Struct[T]) {
    val end = start + count
    var p = offset
    var i = start
    while (i < end) {
      array(i) = T.load(this, p)
      p += T.size
      i += 1
    }
  }
  
  def storeArray[T]
      (offset: Long, array: Array[T], start: Int, count: Int)
      (implicit T: Struct[T]) {
    val end = start + count
    var p = offset
    var i = start
    while (i < end) {
      T.store(this, p, array(i))
      p += T.size
      i += 1
    }
  }
}
