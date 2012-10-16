/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

/** Little-endian memory. */
trait MemLE extends Any with Mem {
  override def endian: LittleEndian.type = LittleEndian
  
  override def copy(size: Long): MemLE
  
  override def loadUnalignedShort(address: Long): Short = {
    ((loadByte(address)      & 0xFF)       |
     (loadByte(address + 1L)         << 8)).toShort
  }
  
  override def storeUnalignedShort(address: Long, value: Short) {
    storeByte(address,       value.toByte)
    storeByte(address + 1L, (value >> 8).toByte)
  }
  
  override def loadUnalignedInt(address: Long): Int = {
     (loadByte(address)      & 0xFF)        |
    ((loadByte(address + 1L) & 0xFF) <<  8) |
    ((loadByte(address + 2L) & 0xFF) << 16) |
     (loadByte(address + 3L)         << 24)
  }
  
  override def storeUnalignedInt(address: Long, value: Int) {
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
  
  override def storeUnalignedLong(address: Long, value: Long) {
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

/** An allocator for little-endian memory backed by a primitive array. */
object MemLE extends Allocator with (Long => MemLE) {
  override def MaxSize: Long = scala.Int.MaxValue << 3
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): MemLE = {
    val size = unit.size * count
    if (size <= scala.Int.MaxValue.toLong) unit.alignment match {
      case 1L => Mem1LE(size)
      case 2L => Mem2LE(size)
      case 4L => Mem4LE(size)
      case _  => Mem8LE(size)
    }
    else if (size <= (scala.Int.MaxValue.toLong << 1)) unit.alignment match {
      case 1L | 2L => Mem2LE(size)
      case 4L      => Mem4LE(size)
      case _       => Mem8LE(size)
    }
    else if (size <= (scala.Int.MaxValue.toLong << 2)) unit.alignment match {
      case 1L | 2L | 4L => Mem4LE(size)
      case _            => Mem8LE(size)
    }
    else Mem8LE(size)
  }
  
  override def apply(size: Long): MemLE = alloc[Byte](size)
  
  override def toString: java.lang.String = "MemLE"
}
