/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

import scala._

/** Little-endian memory backed by a `Short` array. */
final class Mem2LE(val words: scala.Array[Short]) extends AnyVal with MemLE {
  import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
  import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
  
  @inline override def size: Long = words.length.toLong << 1
  
  override def unit: Int = 2
  
  override def endian: LittleEndian.type = LittleEndian
  
  override def copy(size: Long = this.size): Mem2LE = {
    scala.Predef.require(0L <= size && size <= (scala.Int.MaxValue.toLong << 1))
    val words = new scala.Array[Short]((align(size, 2L) >> 1).toInt)
    java.lang.System.arraycopy(this.words, 0, words, 0, scala.math.min(this.words.length, words.length))
    new Mem2LE(words)
  }
  
  override def loadByte(address: Long): Byte = {
    val i = (address >> 1).toInt
    val j = (address.toInt & 1) << 3
    (words(i) >>> j).toByte
  }
  
  override def storeByte(address: Long, value: Byte) {
    val i = (address >> 1).toInt
    val j = (address.toInt & 1) << 3
    words(i) = ((words(i) & ~(0xFF << j)) | ((value & 0xFF) << j)).toShort
  }
  
  override def loadShort(address: Long): Short =
    words((address >> 1).toInt)
  
  override def storeShort(address: Long, value: Short): Unit =
    words((address >> 1).toInt) = value
  
  override def loadInt(address: Long): Int = {
    val i = (address >> 1).toInt & -2
    (words(i)     & 0xFFFF)        |
    (words(i + 1)           << 16)
  }
  
  override def storeInt(address: Long, value: Int) {
    val i = (address >> 1).toInt & -2
    words(i)     =  value.toShort
    words(i + 1) = (value >>> 16).toShort
  }
  
  override def loadLong(address: Long): Long = {
    val i = (address >> 1).toInt & -4
     (words(i)     & 0xFFFF).toLong        |
    ((words(i + 1) & 0xFFFF).toLong << 16) |
    ((words(i + 2) & 0xFFFF).toLong << 32) |
     (words(i + 3).toLong           << 48)
  }
  
  override def storeLong(address: Long, value: Long) {
    val i = (address >> 1).toInt & -4
    words(i)     =  value.toShort
    words(i + 1) = (value >>> 16).toShort
    words(i + 2) = (value >>> 32).toShort
    words(i + 3) = (value >>> 48).toShort
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
    else if ((size & 1L) == 0L && (fromAddress & 1L) == 0L && (toAddress & 1L) == 0L)
      java.lang.System.arraycopy(words, (fromAddress >> 1).toInt,
                                 words, (toAddress   >> 1).toInt, (size >> 1).toInt)
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
    else if ((fromAddress & 1L) != 0L) {
      storeByte(fromAddress, 0.toByte)
      clear(fromAddress + 1L, untilAddress)
    }
    else {
      java.util.Arrays.fill(words, (fromAddress  >> 1).toInt,
                                   (untilAddress >> 1).toInt, 0.toShort)
      clear(untilAddress & -2L, untilAddress)
    }
  }
  
  @inline def toBE: Mem2BE = new Mem2BE(words)
  
  override def toString: java.lang.String = "Mem2LE"+"("+ size +")"
}

/** An allocator for little-endian memory backed by a `Short` array. */
object Mem2LE extends Allocator with (Long => Mem2LE) {
  override def MaxSize: Long = scala.Int.MaxValue.toLong << 1
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Mem2LE =
    apply(unit.size * count)
  
  override def apply(size: Long): Mem2LE = {
    scala.Predef.require(0L <= size && size <= MaxSize)
    val words = new scala.Array[Short]((align(size, 2L) >> 1).toInt)
    new Mem2LE(words)
  }
  
  def unapply(mem: Mem2LE): Some[scala.Array[Short]] = Some(mem.words)
  
  override def toString: java.lang.String = "Mem2LE"
}
