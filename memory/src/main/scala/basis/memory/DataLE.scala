//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

import basis.collections._

/** Little-endian data.
  *
  * @author Chris Sachs
  * @since  0.0
  */
trait DataLE extends Data {
  override def endian: LittleEndian.type = LittleEndian

  override def copy(size: Long): DataLE

  override def loadUnalignedShort(address: Long): Short = {
    ((loadByte(address)      & 0xFF)       |
     (loadByte(address + 1L)         << 8)).toShort
  }

  override def storeUnalignedShort(address: Long, value: Short): Unit = {
    storeByte(address,       value.toByte)
    storeByte(address + 1L, (value >> 8).toByte)
  }

  override def loadUnalignedInt(address: Long): Int = {
     (loadByte(address)      & 0xFF)        |
    ((loadByte(address + 1L) & 0xFF) <<  8) |
    ((loadByte(address + 2L) & 0xFF) << 16) |
     (loadByte(address + 3L)         << 24)
  }

  override def storeUnalignedInt(address: Long, value: Int): Unit = {
    storeByte(address,       value.toByte)
    storeByte(address + 1L, (value >>  8).toByte)
    storeByte(address + 2L, (value >> 16).toByte)
    storeByte(address + 3L, (value >> 24).toByte)
  }

  override def loadUnalignedLong(address: Long): Long = {
     (loadByte(address)      & 0xFF).toLong        |
    ((loadByte(address + 1L) & 0xFF).toLong <<  8) |
    ((loadByte(address + 2L) & 0xFF).toLong << 16) |
    ((loadByte(address + 3L) & 0xFF).toLong << 24) |
    ((loadByte(address + 4L) & 0xFF).toLong << 32) |
    ((loadByte(address + 5L) & 0xFF).toLong << 40) |
    ((loadByte(address + 6L) & 0xFF).toLong << 48) |
     (loadByte(address + 7L).toLong         << 56)
  }

  override def storeUnalignedLong(address: Long, value: Long): Unit = {
    storeByte(address,       value.toByte)
    storeByte(address + 1L, (value >>  8).toByte)
    storeByte(address + 2L, (value >> 16).toByte)
    storeByte(address + 3L, (value >> 24).toByte)
    storeByte(address + 4L, (value >> 32).toByte)
    storeByte(address + 5L, (value >> 40).toByte)
    storeByte(address + 6L, (value >> 48).toByte)
    storeByte(address + 7L, (value >> 56).toByte)
  }
}

/** An allocator for little-endian data backed by a primitive array. */
private[memory] object DataLE extends Allocator[DataLE] {
  override def MaxSize: Long = Int.MaxValue << 3

  override def Endian: Endianness = LittleEndian

  override def alloc[T](count: Long)(implicit T: Struct[T]): DataLE = {
    val size = T.size * count
    if (size <= Int.MaxValue.toLong) T.alignment match {
      case 1L => Data1LE(size)
      case 2L => Data2LE(size)
      case 4L => Data4LE(size)
      case _  => Data8LE(size)
    }
    else if (size <= (Int.MaxValue.toLong << 1)) T.alignment match {
      case 1L | 2L => Data2LE(size)
      case 4L      => Data4LE(size)
      case _       => Data8LE(size)
    }
    else if (size <= (Int.MaxValue.toLong << 2)) T.alignment match {
      case 1L | 2L | 4L => Data4LE(size)
      case _            => Data8LE(size)
    }
    else Data8LE(size)
  }

  override def apply(size: Long): DataLE = alloc[Byte](size)

  override def realloc(data: Loader, size: Long): DataLE = {
    if (data.isInstanceOf[DataLE]) data.asInstanceOf[DataLE].copy(size)
    else super.realloc(data, size)
  }

  override def Framer(): Framer with State[DataLE] = new DataFramer(this)

  override def toString: String = "DataLE"
}
