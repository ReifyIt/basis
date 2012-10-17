/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

/** Big-endian memory backed by a `Long` array. */
class Mem8BE(val words: Array[Long]) extends AnyVal with MemBE {
  import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
  import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
  
  override def size: Long = words.length.toLong << 3
  
  override def unit: Int = 8
  
  override def endian: BigEndian.type = BigEndian
  
  override def copy(size: Long = this.size): Mem8BE = {
    Predef.require(0L <= size && size <= (Int.MaxValue.toLong << 3))
    val words = new Array[Long]((align(size, 8L) >> 3).toInt)
    java.lang.System.arraycopy(this.words, 0, words, 0, java.lang.Math.min(this.words.length, words.length))
    new Mem8BE(words)
  }
  
  override def loadByte(address: Long): Byte = {
    val i = (address >> 3).toInt
    val j = ((address.toInt & 7) ^ 7) << 3
    (words(i) >>> j).toByte
  }
  
  override def storeByte(address: Long, value: Byte) {
    val i = (address >> 3).toInt
    val j = ((address.toInt & 7) ^ 7) << 3
    words(i) = (words(i) & ~(0xFFL << j)) | ((value & 0xFF).toLong << j)
  }
  
  override def loadShort(address: Long): Short = {
    val i = (address >> 3).toInt
    val j = ((address.toInt & 6) ^ 6) << 3
    (words(i) >>> j).toShort
  }
  
  override def storeShort(address: Long, value: Short) {
    val i = (address >> 3).toInt
    val j = ((address.toInt & 6) ^ 6) << 3
    words(i) = (words(i) & ~(0xFFFFL << j)) | ((value & 0xFFFF).toLong << j)
  }
  
  override def loadInt(address: Long): Int = {
    val i = (address >> 3).toInt
    val j = ((address.toInt & 4) ^ 4) << 3
    (words(i) >> j).toInt
  }
  
  override def storeInt(address: Long, value: Int) {
    val i = (address >> 3).toInt
    val j = ((address.toInt & 4) ^ 4) << 3
    words(i) = (words(i) & ~(0xFFFFFFFFL << j)) | ((value & 0xFFFFFFFFL) << j)
  }
  
  override def loadLong(address: Long): Long =
    words((address >> 3).toInt)
  
  override def storeLong(address: Long, value: Long): Unit =
    words((address >> 3).toInt) = value
  
  override def loadFloat(address: Long): Float =
    intBitsToFloat(loadInt(address))
  
  override def storeFloat(address: Long, value: Float): Unit =
    storeInt(address, floatToRawIntBits(value))
  
  override def loadDouble(address: Long): Double =
    longBitsToDouble(loadLong(address))
  
  override def storeDouble(address: Long, value: Double): Unit =
    storeLong(address, doubleToRawLongBits(value))
  
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
    else if ((size & 7L) == 0L && (fromAddress & 7L) == 0L && (toAddress & 7L) == 0L)
      java.lang.System.arraycopy(words, (fromAddress >> 3).toInt,
                                 words, (toAddress   >> 3).toInt, (size >> 3).toInt)
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
    else if ((fromAddress & 7L) != 0L) {
      storeByte(fromAddress, 0.toByte)
      clear(fromAddress + 1L, untilAddress)
    }
    else {
      java.util.Arrays.fill(words, (fromAddress  >> 3).toInt,
                                   (untilAddress >> 3).toInt, 0L)
      clear(untilAddress & -8L, untilAddress)
    }
  }
  
  def toLE: Mem8LE = new Mem8LE(words)
  
  override def toString: String = "Mem8BE"+"("+ size +")"
}

/** An allocator for big-endian memory backed by a `Long` array. */
object Mem8BE extends Allocator with (Long => Mem8BE) {
  override def MaxSize: Long = Int.MaxValue.toLong << 3
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Mem8BE =
    apply(unit.size * count)
  
  override def apply(size: Long): Mem8BE = {
    Predef.require(0L <= size && size <= MaxSize)
    val words = new Array[Long]((align(size, 8L) >> 3).toInt)
    new Mem8BE(words)
  }
  
  def unapply(mem: Mem8BE): Some[Array[Long]] = Some(mem.words)
  
  override def toString: String = "Mem8BE"
}
