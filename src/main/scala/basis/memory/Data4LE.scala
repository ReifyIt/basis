/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** Little-endian `Int` array backed data. */
final class Data4LE(final val words: Array[Int]) extends AnyVal with DataLE {
  import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
  import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
  
  @inline final override def size: Long = words.length.toLong << 2
  
  final override def unit: Int = 4
  
  final override def endian: LittleEndian.type = LittleEndian
  
  final override def copy(size: Long = this.size): Data4LE = {
    require(0L <= size && size <= (Int.MaxValue.toLong << 2))
    val words = new Array[Int]((align(size, 4L) >> 2).toInt)
    System.arraycopy(this.words, 0, words, 0, math.min(this.words.length, words.length))
    new Data4LE(words)
  }
  
  final override def loadByte(address: Long): Byte = {
    val i = (address >> 2).toInt
    val j = (address.toInt & 3) << 3
    (words(i) >>> j).toByte
  }
  
  final override def storeByte(address: Long, value: Byte) {
    val i = (address >> 2).toInt
    val j = (address.toInt & 3) << 3
    words(i) = (words(i) & ~(0xFF << j)) | ((value & 0xFF) << j)
  }
  
  final override def loadShort(address: Long): Short = {
    val i = (address >> 2).toInt
    val j = (address.toInt & 2) << 3
    (words(i) >>> j).toShort
  }
  
  final override def storeShort(address: Long, value: Short) {
    val i = (address >> 2).toInt
    val j = (address.toInt & 2) << 3
    words(i) = (words(i) & ~(0xFFFF << j)) | ((value & 0xFFFF) << j)
  }
  
  final override def loadInt(address: Long): Int =
    words((address >> 2).toInt)
  
  final override def storeInt(address: Long, value: Int): Unit =
    words((address >> 2).toInt) = value
  
  final override def loadLong(address: Long): Long = {
    val i = (address >> 2).toInt & ~1
    (words(i).toLong     & 0xFFFFFFFFL)        |
    (words(i + 1).toLong                << 32)
  }
  
  final override def storeLong(address: Long, value: Long) {
    val i = (address >> 2).toInt & ~1
    words(i) = value.toInt
    words(i + 1) = (value >>> 32).toInt
  }
  
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
    ((loadByte(address)      & 0xFF)       |
     (loadByte(address + 1L)         << 8)).toShort
  }
  
  final override def storeUnalignedShort(address: Long, value: Short) {
    storeByte(address,       value.toByte)
    storeByte(address + 1L, (value >> 8).toByte)
  }
  
  final override def loadUnalignedInt(address: Long): Int = {
     (loadByte(address)      & 0xFF)        |
    ((loadByte(address + 1L) & 0xFF) <<  8) |
    ((loadByte(address + 2L) & 0xFF) << 16) |
     (loadByte(address + 3L)         << 24)
  }
  
  final override def storeUnalignedInt(address: Long, value: Int) {
    storeByte(address,       value.toByte)
    storeByte(address + 1L, (value >>  8).toByte)
    storeByte(address + 2L, (value >> 16).toByte)
    storeByte(address + 3L, (value >> 24).toByte)
  }
  
  final override def loadUnalignedLong(address: Long): Long = {
     (loadByte(address)      & 0xFF).toLong        |
    ((loadByte(address + 1L) & 0xFF).toLong <<  8) |
    ((loadByte(address + 2L) & 0xFF).toLong << 16) |
    ((loadByte(address + 3L) & 0xFF).toLong << 24) |
    ((loadByte(address + 4L) & 0xFF).toLong << 32) |
    ((loadByte(address + 5L) & 0xFF).toLong << 40) |
    ((loadByte(address + 6L) & 0xFF).toLong << 48) |
     (loadByte(address + 7L).toLong         << 56)
  }
  
  final override def storeUnalignedLong(address: Long, value: Long) {
    storeByte(address,       value.toByte)
    storeByte(address + 1L, (value >>  8).toByte)
    storeByte(address + 2L, (value >> 16).toByte)
    storeByte(address + 3L, (value >> 24).toByte)
    storeByte(address + 4L, (value >> 32).toByte)
    storeByte(address + 5L, (value >> 40).toByte)
    storeByte(address + 6L, (value >> 48).toByte)
    storeByte(address + 7L, (value >> 56).toByte)
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
    else if ((size & 3L) == 0L && (fromAddress & 3L) == 0L && (toAddress & 3L) == 0L)
      System.arraycopy(words, (fromAddress >> 2).toInt,
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
  
  final override def clear(fromAddress: Long, untilAddress: Long) {
    if (fromAddress > untilAddress)
      throw new IllegalArgumentException("fromAddress > untilAddress")
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
  
  @inline final def toBE: Data4BE = new Data4BE(words)
  
  override def toString: String = "Data4LE"+"("+ size +")"
}

/** An allocator for little-endian data backed by an `Int` array. */
object Data4LE extends Allocator with (Long => Data4LE) {
  override def MaxSize: Long = Int.MaxValue.toLong << 2
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Data4LE =
    apply(unit.size * count)
  
  override def apply(size: Long): Data4LE = {
    require(0L <= size && size <= MaxSize)
    val words = new Array[Int]((align(size, 4L) >> 2).toInt)
    new Data4LE(words)
  }
  
  def unapply(data: Data4LE): Some[Array[Int]] = Some(data.words)
  
  override def toString: String = "Data4LE"
}
