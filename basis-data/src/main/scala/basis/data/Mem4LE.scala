/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

import scala._

/** Little-endian memory backed by an `Int` array. */
final class Mem4LE(val words: scala.Array[Int]) extends AnyVal with MemLE {
  import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
  import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
  
  @inline override def size: Long = words.length.toLong << 2
  
  override def unit: Int = 4
  
  override def endian: LittleEndian.type = LittleEndian
  
  override def copy(size: Long = this.size): Mem4LE = {
    scala.Predef.require(0L <= size && size <= (scala.Int.MaxValue.toLong << 2))
    val words = new scala.Array[Int]((align(size, 4L) >> 2).toInt)
    java.lang.System.arraycopy(this.words, 0, words, 0, scala.math.min(this.words.length, words.length))
    new Mem4LE(words)
  }
  
  override def loadByte(address: Long): Byte = {
    val i = (address >> 2).toInt
    val j = (address.toInt & 3) << 3
    (words(i) >>> j).toByte
  }
  
  override def storeByte(address: Long, value: Byte) {
    val i = (address >> 2).toInt
    val j = (address.toInt & 3) << 3
    words(i) = (words(i) & ~(0xFF << j)) | ((value & 0xFF) << j)
  }
  
  override def loadShort(address: Long): Short = {
    val i = (address >> 2).toInt
    val j = (address.toInt & 2) << 3
    (words(i) >>> j).toShort
  }
  
  override def storeShort(address: Long, value: Short) {
    val i = (address >> 2).toInt
    val j = (address.toInt & 2) << 3
    words(i) = (words(i) & ~(0xFFFF << j)) | ((value & 0xFFFF) << j)
  }
  
  override def loadInt(address: Long): Int =
    words((address >> 2).toInt)
  
  override def storeInt(address: Long, value: Int): Unit =
    words((address >> 2).toInt) = value
  
  override def loadLong(address: Long): Long = {
    val i = (address >> 2).toInt & ~1
    (words(i).toLong     & 0xFFFFFFFFL)        |
    (words(i + 1).toLong                << 32)
  }
  
  override def storeLong(address: Long, value: Long) {
    val i = (address >> 2).toInt & ~1
    words(i) = value.toInt
    words(i + 1) = (value >>> 32).toInt
  }
  
  override def loadFloat(address: Long): Float =
    intBitsToFloat(loadInt(address))
  
  override def storeFloat(address: Long, value: Float): Unit =
    storeInt(address, floatToRawIntBits(value))
  
  override def loadDouble(address: Long): Double =
    longBitsToDouble(loadLong(address))
  
  override def storeDouble(address: Long, value: Double): Unit =
    storeLong(address, doubleToRawLongBits(value))
  
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
  
  override def loadUnalignedFloat(address: Long): Float =
    intBitsToFloat(loadUnalignedInt(address))
  
  override def storeUnalignedFloat(address: Long, value: Float): Unit =
    storeUnalignedInt(address, floatToRawIntBits(value))
  
  override def loadUnalignedDouble(address: Long): Double =
    longBitsToDouble(loadUnalignedLong(address))
  
  override def storeUnalignedDouble(address: Long, value: Double): Unit =
    storeUnalignedLong(address, doubleToRawLongBits(value))
  
  override def move(fromAddress: Long, toAddress: Long, size: Long) {
    val fromLimit = fromAddress + size
    val toLimit = toAddress + size
    if (fromAddress == toAddress) ()
    else if ((size & 3L) == 0L && (fromAddress & 3L) == 0L && (toAddress & 3L) == 0L)
      java.lang.System.arraycopy(words, (fromAddress >> 2).toInt,
                                 words, (toAddress   >> 2).toInt, (size >> 2).toInt)
    else if (fromAddress >= toAddress || fromLimit <= toAddress) {
      var p = fromAddress
      var q = toAddress
      while (q < toLimit) {
        storeByte(q, loadByte(p))
        p += 1L
        q += 1L
      }
    }
    else {
      var p = fromLimit - 1L
      var q = toLimit - 1L
      while (q >= toAddress) {
        storeByte(q, loadByte(p))
        p -= 1L
        q -= 1L
      }
    }
  }
  
  override def clear(fromAddress: Long, untilAddress: Long) {
    if (fromAddress > untilAddress)
      throw new java.lang.IllegalArgumentException("fromAddress > untilAddress")
    else if (fromAddress == untilAddress) ()
    else if ((fromAddress & 3L) != 0L) {
      storeByte(fromAddress, 0.toByte)
      clear(fromAddress + 1L, untilAddress)
    }
    else {
      java.util.Arrays.fill(words, (fromAddress  >> 2).toInt,
                                   (untilAddress >> 2).toInt, 0)
      clear(untilAddress & -4L, untilAddress)
    }
  }
  
  @inline def toBE: Mem4BE = new Mem4BE(words)
  
  override def toString: java.lang.String = "Mem4LE"+"("+ size +")"
}

/** An allocator for little-endian memory backed by an `Int` array. */
object Mem4LE extends Allocator with (Long => Mem4LE) {
  override def MaxSize: Long = scala.Int.MaxValue.toLong << 2
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Mem4LE =
    apply(unit.size * count)
  
  override def apply(size: Long): Mem4LE = {
    scala.Predef.require(0L <= size && size <= MaxSize)
    val words = new scala.Array[Int]((align(size, 4L) >> 2).toInt)
    new Mem4LE(words)
  }
  
  def unapply(mem: Mem4LE): Some[scala.Array[Int]] = Some(mem.words)
  
  override def toString: java.lang.String = "Mem4LE"
}