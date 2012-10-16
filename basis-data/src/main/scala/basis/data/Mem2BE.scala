/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

/** Big-endian memory backed by a `Short` array. */
class Mem2BE(val words: scala.Array[Short]) extends AnyVal with MemBE {
  import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
  import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
  
  override def size: Long = words.length.toLong << 1
  
  override def unit: Int = 2
  
  override def endian: BigEndian.type = BigEndian
  
  override def copy(size: Long = this.size): Mem2BE = {
    scala.Predef.require(0L <= size && size <= (scala.Int.MaxValue.toLong << 1))
    val words = new scala.Array[Short]((align(size, 2L) >> 1).toInt)
    java.lang.System.arraycopy(this.words, 0, words, 0, scala.math.min(this.words.length, words.length))
    new Mem2BE(words)
  }
  
  override def loadByte(address: Long): Byte = {
    val i = (address >> 1).toInt
    val j = ((address.toInt & 1) ^ 1) << 3
    (words(i) >>> j).toByte
  }
  
  override def storeByte(address: Long, value: Byte) {
    val i = (address >> 1).toInt
    val j = ((address.toInt & 1) ^ 1) << 3
    words(i) = ((words(i) & ~(0xFF << j)) | ((value & 0xFF) << j)).toShort
  }
  
  override def loadShort(address: Long): Short =
    words((address >> 1).toInt)
  
  override def storeShort(address: Long, value: Short): Unit =
    words((address >> 1).toInt) = value
  
  override def loadInt(address: Long): Int = {
    val i = (address >> 1).toInt & -2
    (words(i)               << 16) |
    (words(i + 1) & 0xFFFF)
  }
  
  override def storeInt(address: Long, value: Int) {
    val i = (address >> 1).toInt & -2
    words(i)     = (value >>> 16).toShort
    words(i + 1) =  value.toShort
  }
  
  override def loadLong(address: Long): Long = {
    val i = (address >> 1).toInt & -4
     (words(i).toLong               << 48) |
    ((words(i + 1) & 0xFFFF).toLong << 32) |
    ((words(i + 2) & 0xFFFF).toLong << 16) |
     (words(i + 3) & 0xFFFF).toLong
  }
  
  override def storeLong(address: Long, value: Long) {
    val i = (address >> 1).toInt & -4
    words(i)     = (value >>> 48).toShort
    words(i + 1) = (value >>> 32).toShort
    words(i + 2) = (value >>> 16).toShort
    words(i + 3) =  value.toShort
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
  
  def toLE: Mem2LE = new Mem2LE(words)
  
  override def toString: java.lang.String = "Mem2BE"+"("+ size +")"
}

/** An allocator for big-endian memory backed by a `Short` array. */
object Mem2BE extends Allocator with (Long => Mem2BE) {
  override def MaxSize: Long = scala.Int.MaxValue.toLong << 1
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Mem2BE =
    apply(unit.size * count)
  
  override def apply(size: Long): Mem2BE = {
    scala.Predef.require(0L <= size && size <= MaxSize)
    val words = new scala.Array[Short]((align(size, 2L) >> 1).toInt)
    new Mem2BE(words)
  }
  
  def unapply(mem: Mem2BE): Some[scala.Array[Short]] = Some(mem.words)
  
  override def toString: java.lang.String = "Mem2BE"
}
