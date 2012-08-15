/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** Native-endian `Short` array backed data. */
final class Data2(final val words: Array[Short]) extends AnyVal with Data {
  import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
  import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
  
  @inline final override def size: Long = words.length.toLong << 1
  
  final override def unit: Int = 2
  
  final override def endian: NativeEndian.type = NativeEndian
  
  final override def copy(size: Long = this.size): Data2 = {
    require(0L <= size && size <= (Int.MaxValue.toLong << 1))
    val words = new Array[Short]((align(size, 2L) >> 1).toInt)
    System.arraycopy(this.words, 0, words, 0, math.min(this.words.length, words.length))
    new Data2(words)
  }
  
  final override def loadByte(address: Long): Byte = {
    if (BigEndian.isNative)    return toBE.loadByte(address)
    if (LittleEndian.isNative) return toLE.loadByte(address)
    throw new AssertionError
  }
  
  final override def storeByte(address: Long, value: Byte) {
    if (BigEndian.isNative)    return toBE.storeByte(address, value)
    if (LittleEndian.isNative) return toLE.storeByte(address, value)
    throw new AssertionError
  }
  
  final override def loadShort(address: Long): Short =
    words((address >> 1).toInt)
  
  final override def storeShort(address: Long, value: Short): Unit =
    words((address >> 1).toInt) = value
  
  final override def loadInt(address: Long): Int = {
    if (BigEndian.isNative)    return toBE.loadInt(address)
    if (LittleEndian.isNative) return toLE.loadInt(address)
    throw new AssertionError
  }
  
  final override def storeInt(address: Long, value: Int) {
    if (BigEndian.isNative)    return toBE.storeInt(address, value)
    if (LittleEndian.isNative) return toLE.storeInt(address, value)
    throw new AssertionError
  }
  
  final override def loadLong(address: Long): Long = {
    if (BigEndian.isNative)    return toBE.loadLong(address)
    if (LittleEndian.isNative) return toLE.loadLong(address)
    throw new AssertionError
  }
  
  final override def storeLong(address: Long, value: Long) {
    if (BigEndian.isNative)    return toBE.storeLong(address, value)
    if (LittleEndian.isNative) return toLE.storeLong(address, value)
    throw new AssertionError
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
    if (BigEndian.isNative)    return toBE.loadUnalignedShort(address)
    if (LittleEndian.isNative) return toLE.loadUnalignedShort(address)
    throw new AssertionError
  }
  
  final override def storeUnalignedShort(address: Long, value: Short) {
    if (BigEndian.isNative)    return toBE.storeUnalignedShort(address, value)
    if (LittleEndian.isNative) return toLE.storeUnalignedShort(address, value)
    throw new AssertionError
  }
  
  final override def loadUnalignedInt(address: Long): Int = {
    if (BigEndian.isNative)    return toBE.loadUnalignedInt(address)
    if (LittleEndian.isNative) return toLE.loadUnalignedInt(address)
    throw new AssertionError
  }
  
  final override def storeUnalignedInt(address: Long, value: Int) {
    if (BigEndian.isNative)    return toBE.storeUnalignedInt(address, value)
    if (LittleEndian.isNative) return toLE.storeUnalignedInt(address, value)
    throw new AssertionError
  }
  
  final override def loadUnalignedLong(address: Long): Long = {
    if (BigEndian.isNative)    return toBE.loadUnalignedLong(address)
    if (LittleEndian.isNative) return toLE.loadUnalignedLong(address)
    throw new AssertionError
  }
  
  final override def storeUnalignedLong(address: Long, value: Long) {
    if (BigEndian.isNative)    return toBE.storeUnalignedLong(address, value)
    if (LittleEndian.isNative) return toLE.storeUnalignedLong(address, value)
    throw new AssertionError
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
    else if ((size & 1L) == 0L && (fromAddress & 1L) == 0L && (toAddress & 1L) == 0L)
      System.arraycopy(words, (fromAddress >> 1).toInt,
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
  
  final override def clear(fromAddress: Long, untilAddress: Long) {
    if (fromAddress > untilAddress)
      throw new IllegalArgumentException("fromAddress > untilAddress")
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
  
  @inline final def toBE: Data2BE = new Data2BE(words)
  
  @inline final def toLE: Data2LE = new Data2LE(words)
  
  override def toString: String = "Data2"+"("+ size +")"
}

/** An allocator for native-endian data backed by a `Short` array. */
object Data2 extends Allocator with (Long => Data2) {
  override def MaxSize: Long = Int.MaxValue.toLong << 1
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Data2 =
    apply(unit.size * count)
  
  override def apply(size: Long): Data2 = {
    require(0L <= size && size <= MaxSize)
    val words = new Array[Short]((align(size, 2L) >> 1).toInt)
    new Data2(words)
  }
  
  def unapply(data: Data2): Some[Array[Short]] = Some(data.words)
  
  override def toString: String = "Data2"
}
