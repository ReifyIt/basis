/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** Native-endian data backed by an `Int` array. */
class Data4(val words: Array[Int]) extends AnyVal with Data {
  import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
  import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
  
  override def size: Long = words.length.toLong << 2
  
  override def unit: Int = 4
  
  override def endian: NativeEndian.type = NativeEndian
  
  override def copy(size: Long = this.size): Data4 = {
    Predef.require(0L <= size && size <= (Int.MaxValue.toLong << 2))
    val words = new Array[Int]((align(size, 4L) >> 2).toInt)
    java.lang.System.arraycopy(this.words, 0, words, 0, java.lang.Math.min(this.words.length, words.length))
    new Data4(words)
  }
  
  override def loadByte(address: Long): Byte = {
    if (BigEndian.isNative)    return toBE.loadByte(address)
    if (LittleEndian.isNative) return toLE.loadByte(address)
    throw new MatchError(NativeEndian)
  }
  
  override def storeByte(address: Long, value: Byte) {
    if (BigEndian.isNative)    return toBE.storeByte(address, value)
    if (LittleEndian.isNative) return toLE.storeByte(address, value)
    throw new MatchError(NativeEndian)
  }
  
  override def loadShort(address: Long): Short = {
    if (BigEndian.isNative)    return toBE.loadShort(address)
    if (LittleEndian.isNative) return toLE.loadShort(address)
    throw new MatchError(NativeEndian)
  }
  
  override def storeShort(address: Long, value: Short) {
    if (BigEndian.isNative)    return toBE.storeShort(address, value)
    if (LittleEndian.isNative) return toLE.storeShort(address, value)
    throw new MatchError(NativeEndian)
  }
  
  override def loadInt(address: Long): Int =
    words((address >> 2).toInt)
  
  override def storeInt(address: Long, value: Int): Unit =
    words((address >> 2).toInt) = value
  
  override def loadLong(address: Long): Long = {
    if (BigEndian.isNative)    return toBE.loadLong(address)
    if (LittleEndian.isNative) return toLE.loadLong(address)
    throw new MatchError(NativeEndian)
  }
  
  override def storeLong(address: Long, value: Long) {
    if (BigEndian.isNative)    return toBE.storeLong(address, value)
    if (LittleEndian.isNative) return toLE.storeLong(address, value)
    throw new MatchError(NativeEndian)
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
    if (BigEndian.isNative)    return toBE.loadUnalignedShort(address)
    if (LittleEndian.isNative) return toLE.loadUnalignedShort(address)
    throw new MatchError(NativeEndian)
  }
  
  override def storeUnalignedShort(address: Long, value: Short) {
    if (BigEndian.isNative)    return toBE.storeUnalignedShort(address, value)
    if (LittleEndian.isNative) return toLE.storeUnalignedShort(address, value)
    throw new MatchError(NativeEndian)
  }
  
  override def loadUnalignedInt(address: Long): Int = {
    if (BigEndian.isNative)    return toBE.loadUnalignedInt(address)
    if (LittleEndian.isNative) return toLE.loadUnalignedInt(address)
    throw new MatchError(NativeEndian)
  }
  
  override def storeUnalignedInt(address: Long, value: Int) {
    if (BigEndian.isNative)    return toBE.storeUnalignedInt(address, value)
    if (LittleEndian.isNative) return toLE.storeUnalignedInt(address, value)
    throw new MatchError(NativeEndian)
  }
  
  override def loadUnalignedLong(address: Long): Long = {
    if (BigEndian.isNative)    return toBE.loadUnalignedLong(address)
    if (LittleEndian.isNative) return toLE.loadUnalignedLong(address)
    throw new MatchError(NativeEndian)
  }
  
  override def storeUnalignedLong(address: Long, value: Long) {
    if (BigEndian.isNative)    return toBE.storeUnalignedLong(address, value)
    if (LittleEndian.isNative) return toLE.storeUnalignedLong(address, value)
    throw new MatchError(NativeEndian)
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
  
  def toBE: Data4BE = new Data4BE(words)
  
  def toLE: Data4LE = new Data4LE(words)
  
  override def toString: String = "Data4"+"("+ size +")"
}

/** An allocator for native-endian data backed by an `Int` array. */
object Data4 extends Allocator with (Long => Data4) {
  override def MaxSize: Long = Int.MaxValue.toLong << 2
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Data4 =
    apply(unit.size * count)
  
  override def apply(size: Long): Data4 = {
    Predef.require(0L <= size && size <= MaxSize)
    val words = new Array[Int]((align(size, 4L) >> 2).toInt)
    new Data4(words)
  }
  
  def unapply(data: Data4): Some[Array[Int]] = Some(data.words)
  
  override def toString: String = "Data4"
}
