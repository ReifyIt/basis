/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** Big-endian `Long` array backed data. */
final class Data8BE(final val words: Array[Long]) extends AnyVal with DataBE {
  import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
  import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
  
  @inline final override def size: Long = words.length.toLong << 3
  
  final override def unit: Int = 8
  
  final override def endian: BigEndian.type = BigEndian
  
  final override def copy(size: Long = this.size): Data8BE = {
    require(0L <= size && size <= (Int.MaxValue.toLong << 3))
    val words = new Array[Long]((align(size, 8L) >> 3).toInt)
    System.arraycopy(this.words, 0, words, 0, math.min(this.words.length, words.length))
    new Data8BE(words)
  }
  
  final override def loadByte(address: Long): Byte = {
    val i = (address >> 3).toInt
    val j = ((address.toInt & 7) ^ 7) << 3
    (words(i) >>> j).toByte
  }
  
  final override def storeByte(address: Long, value: Byte) {
    val i = (address >> 3).toInt
    val j = ((address.toInt & 7) ^ 7) << 3
    words(i) = (words(i) & ~(0xFFL << j)) | ((value & 0xFF).toLong << j)
  }
  
  final override def loadShort(address: Long): Short = {
    val i = (address >> 3).toInt
    val j = ((address.toInt & 6) ^ 6) << 3
    (words(i) >>> j).toShort
  }
  
  final override def storeShort(address: Long, value: Short) {
    val i = (address >> 3).toInt
    val j = ((address.toInt & 6) ^ 6) << 3
    words(i) = (words(i) & ~(0xFFFFL << j)) | ((value & 0xFFFF).toLong << j)
  }
  
  final override def loadInt(address: Long): Int = {
    val i = (address >> 3).toInt
    val j = ((address.toInt & 4) ^ 4) << 3
    (words(i) >> j).toInt
  }
  
  final override def storeInt(address: Long, value: Int) {
    val i = (address >> 3).toInt
    val j = ((address.toInt & 4) ^ 4) << 3
    words(i) = (words(i) & ~(0xFFFFFFFFL << j)) | ((value & 0xFFFFFFFFL) << j)
  }
  
  final override def loadLong(address: Long): Long =
    words((address >> 3).toInt)
  
  final override def storeLong(address: Long, value: Long): Unit =
    words((address >> 3).toInt) = value
  
  final override def loadChar(address: Long): Char =
    loadShort(address).toChar
  
  final override def storeChar(address: Long, value: Char): Unit =
    storeShort(address, value.toShort)
  
  final override def loadFloat(address: Long): Float =
    intBitsToFloat(loadInt(address))
  
  final override def storeFloat(address: Long, value: Float): Unit =
    storeInt(address, floatToRawIntBits(value))
  
  final override def loadDouble(address: Long): Double =
    longBitsToDouble(loadLong(address))
  
  final override def storeDouble(address: Long, value: Double): Unit =
    storeLong(address, doubleToRawLongBits(value))
  
  final override def loadUnalignedShort(address: Long): Short = {
    ((loadByte(address)              << 8) |
     (loadByte(address + 1L) & 0xFF)).toShort
  }
  
  final override def storeUnalignedShort(address: Long, value: Short) {
    storeByte(address,      (value >> 8).toByte)
    storeByte(address + 1L,  value.toByte)
  }
  
  final override def loadUnalignedInt(address: Long): Int = {
     (loadByte(address)              << 24) |
    ((loadByte(address + 1L) & 0xFF) << 16) |
    ((loadByte(address + 2L) & 0xFF) <<  8) |
     (loadByte(address + 3L) & 0xFF)
  }
  
  final override def storeUnalignedInt(address: Long, value: Int) {
    storeByte(address,      (value >> 24).toByte)
    storeByte(address + 1L, (value >> 16).toByte)
    storeByte(address + 2L, (value >>  8).toByte)
    storeByte(address + 3L,  value.toByte)
  }
  
  final override def loadUnalignedLong(address: Long): Long = {
     (loadByte(address).toLong              << 56) |
    ((loadByte(address + 1L) & 0xFF).toLong << 48) |
    ((loadByte(address + 2L) & 0xFF).toLong << 40) |
    ((loadByte(address + 3L) & 0xFF).toLong << 32) |
    ((loadByte(address + 4L) & 0xFF).toLong << 24) |
    ((loadByte(address + 5L) & 0xFF).toLong << 16) |
    ((loadByte(address + 6L) & 0xFF).toLong <<  8) |
     (loadByte(address + 7L) & 0xFF).toLong
  }
  
  final override def storeUnalignedLong(address: Long, value: Long) {
    storeByte(address,      (value >> 56).toByte)
    storeByte(address + 1L, (value >> 48).toByte)
    storeByte(address + 2L, (value >> 40).toByte)
    storeByte(address + 3L, (value >> 32).toByte)
    storeByte(address + 4L, (value >> 24).toByte)
    storeByte(address + 5L, (value >> 16).toByte)
    storeByte(address + 6L, (value >>  8).toByte)
    storeByte(address + 7L,  value.toByte)
  }
  
  final override def loadUnalignedChar(address: Long): Char =
    loadUnalignedShort(address).toChar
  
  final override def storeUnalignedChar(address: Long, value: Char): Unit =
    storeUnalignedShort(address, value.toShort)
  
  final override def loadUnalignedFloat(address: Long): Float =
    intBitsToFloat(loadUnalignedInt(address))
  
  final override def storeUnalignedFloat(address: Long, value: Float): Unit =
    storeUnalignedInt(address, floatToRawIntBits(value))
  
  final override def loadUnalignedDouble(address: Long): Double =
    longBitsToDouble(loadUnalignedLong(address))
  
  final override def storeUnalignedDouble(address: Long, value: Double): Unit =
    storeUnalignedLong(address, doubleToRawLongBits(value))
  
  final override def move(fromAddress: Long, toAddress: Long, size: Long) {
    val fromLimit = fromAddress + size
    val toLimit = toAddress + size
    if (fromAddress == toAddress) ()
    else if ((size & 7L) == 0L && (fromAddress & 7L) == 0L && (toAddress & 7L) == 0L)
      System.arraycopy(words, (fromAddress >> 3).toInt,
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
  
  final override def clear(fromAddress: Long, untilAddress: Long) {
    if (fromAddress > untilAddress)
      throw new IllegalArgumentException("fromAddress > untilAddress")
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
  
  @inline final def toLE: Data8LE = new Data8LE(words)
  
  override def toString: String = "Data8BE"+"("+ size +")"
}

/** An allocator for big-endian data backed by a `Long` array. */
object Data8BE extends Allocator with (Long => Data8BE) {
  override def MaxSize: Long = Int.MaxValue.toLong << 3
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Data8BE =
    apply(unit.size * count)
  
  override def apply(size: Long): Data8BE = {
    require(0L <= size && size <= MaxSize)
    val words = new Array[Long]((align(size, 8L) >> 3).toInt)
    new Data8BE(words)
  }
  
  def unapply(data: Data8BE): Some[Array[Long]] = Some(data.words)
  
  override def toString: String = "Data8BE"
}
