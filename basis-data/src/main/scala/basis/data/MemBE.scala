/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

import scala._

/** Big-endian memory. */
trait MemBE extends Any with Mem {
  override def endian: BigEndian.type = BigEndian
  
  override def copy(size: Long): MemBE
  
  override def loadUnalignedShort(address: Long): Short = {
    ((loadByte(address)              << 8) |
     (loadByte(address + 1L) & 0xFF)).toShort
  }
  
  override def storeUnalignedShort(address: Long, value: Short) {
    storeByte(address,      (value >> 8).toByte)
    storeByte(address + 1L,  value.toByte)
  }
  
  override def loadUnalignedInt(address: Long): Int = {
     (loadByte(address)              << 24) |
    ((loadByte(address + 1L) & 0xFF) << 16) |
    ((loadByte(address + 2L) & 0xFF) <<  8) |
     (loadByte(address + 3L) & 0xFF)
  }
  
  override def storeUnalignedInt(address: Long, value: Int) {
    storeByte(address,      (value >> 24).toByte)
    storeByte(address + 1L, (value >> 16).toByte)
    storeByte(address + 2L, (value >>  8).toByte)
    storeByte(address + 3L,  value.toByte)
  }
  
  override def loadUnalignedLong(address: Long): Long = {
     (loadByte(address).toLong              << 56) |
    ((loadByte(address + 1L) & 0xFF).toLong << 48) |
    ((loadByte(address + 2L) & 0xFF).toLong << 40) |
    ((loadByte(address + 3L) & 0xFF).toLong << 32) |
    ((loadByte(address + 4L) & 0xFF).toLong << 24) |
    ((loadByte(address + 5L) & 0xFF).toLong << 16) |
    ((loadByte(address + 6L) & 0xFF).toLong <<  8) |
     (loadByte(address + 7L) & 0xFF).toLong
  }
  
  override def storeUnalignedLong(address: Long, value: Long) {
    storeByte(address,      (value >> 56).toByte)
    storeByte(address + 1L, (value >> 48).toByte)
    storeByte(address + 2L, (value >> 40).toByte)
    storeByte(address + 3L, (value >> 32).toByte)
    storeByte(address + 4L, (value >> 24).toByte)
    storeByte(address + 5L, (value >> 16).toByte)
    storeByte(address + 6L, (value >>  8).toByte)
    storeByte(address + 7L,  value.toByte)
  }
}

/** An allocator for big-endian memory backed by a primitive array. */
object MemBE extends Allocator with (Long => MemBE) {
  override def MaxSize: Long = scala.Int.MaxValue.toLong << 3
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): MemBE = {
    val size = unit.size * count
    if (size <= scala.Int.MaxValue.toLong) unit.alignment match {
      case 1L => Mem1BE(size)
      case 2L => Mem2BE(size)
      case 4L => Mem4BE(size)
      case _  => Mem8BE(size)
    }
    else if (size <= (scala.Int.MaxValue.toLong << 1)) unit.alignment match {
      case 1L | 2L => Mem2BE(size)
      case 4L      => Mem4BE(size)
      case _       => Mem8BE(size)
    }
    else if (size <= (scala.Int.MaxValue.toLong << 2)) unit.alignment match {
      case 1L | 2L | 4L => Mem4BE(size)
      case _            => Mem8BE(size)
    }
    else Mem8BE(size)
  }
  
  override def apply(size: Long): MemBE = alloc[Byte](size)
  
  override def toString: java.lang.String = "MemBE"
}
