/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

/** Native-endian memory backed by a `Long` array. */
class Mem8(val words: scala.Array[Long]) extends AnyVal with Mem {
  import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
  import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
  
  override def size: Long = words.length.toLong << 3
  
  override def unit: Int = 8
  
  override def endian: NativeEndian.type = NativeEndian
  
  override def copy(size: Long = this.size): Mem8 = {
    scala.Predef.require(0L <= size && size <= (scala.Int.MaxValue.toLong << 3))
    val words = new scala.Array[Long]((align(size, 8L) >> 3).toInt)
    java.lang.System.arraycopy(this.words, 0, words, 0, scala.math.min(this.words.length, words.length))
    new Mem8(words)
  }
  
  override def loadByte(address: Long): Byte = {
    if (BigEndian.isNative)    return toBE.loadByte(address)
    if (LittleEndian.isNative) return toLE.loadByte(address)
    throw new scala.MatchError(NativeEndian)
  }
  
  override def storeByte(address: Long, value: Byte) {
    if (BigEndian.isNative)    return toBE.storeByte(address, value)
    if (LittleEndian.isNative) return toLE.storeByte(address, value)
    throw new scala.MatchError(NativeEndian)
  }
  
  override def loadShort(address: Long): Short = {
    if (BigEndian.isNative)    return toBE.loadShort(address)
    if (LittleEndian.isNative) return toLE.loadShort(address)
    throw new scala.MatchError(NativeEndian)
  }
  
  override def storeShort(address: Long, value: Short) {
    if (BigEndian.isNative)    return toBE.storeShort(address, value)
    if (LittleEndian.isNative) return toLE.storeShort(address, value)
    throw new scala.MatchError(NativeEndian)
  }
  
  override def loadInt(address: Long): Int = {
    if (BigEndian.isNative)    return toBE.loadInt(address)
    if (LittleEndian.isNative) return toLE.loadInt(address)
    throw new scala.MatchError(NativeEndian)
  }
  
  override def storeInt(address: Long, value: Int) {
    if (BigEndian.isNative)    return toBE.storeInt(address, value)
    if (LittleEndian.isNative) return toLE.storeInt(address, value)
    throw new scala.MatchError(NativeEndian)
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
    if (BigEndian.isNative)    return toBE.loadUnalignedShort(address)
    if (LittleEndian.isNative) return toLE.loadUnalignedShort(address)
    throw new scala.MatchError(NativeEndian)
  }
  
  override def storeUnalignedShort(address: Long, value: Short) {
    if (BigEndian.isNative)    return toBE.storeUnalignedShort(address, value)
    if (LittleEndian.isNative) return toLE.storeUnalignedShort(address, value)
    throw new scala.MatchError(NativeEndian)
  }
  
  override def loadUnalignedInt(address: Long): Int = {
    if (BigEndian.isNative)    return toBE.loadUnalignedInt(address)
    if (LittleEndian.isNative) return toLE.loadUnalignedInt(address)
    throw new scala.MatchError(NativeEndian)
  }
  
  override def storeUnalignedInt(address: Long, value: Int) {
    if (BigEndian.isNative)    return toBE.storeUnalignedInt(address, value)
    if (LittleEndian.isNative) return toLE.storeUnalignedInt(address, value)
    throw new scala.MatchError(NativeEndian)
  }
  
  override def loadUnalignedLong(address: Long): Long = {
    if (BigEndian.isNative)    return toBE.loadUnalignedLong(address)
    if (LittleEndian.isNative) return toLE.loadUnalignedLong(address)
    throw new scala.MatchError(NativeEndian)
  }
  
  override def storeUnalignedLong(address: Long, value: Long) {
    if (BigEndian.isNative)    return toBE.storeUnalignedLong(address, value)
    if (LittleEndian.isNative) return toLE.storeUnalignedLong(address, value)
    throw new scala.MatchError(NativeEndian)
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
  
  def toBE: Mem8BE = new Mem8BE(words)
  
  def toLE: Mem8LE = new Mem8LE(words)
  
  override def toString: java.lang.String = "Mem8"+"("+ size +")"
}

/** An allocator for native-endian memory backed by a `Long` array. */
object Mem8 extends Allocator with (Long => Mem8) {
  override def MaxSize: Long = scala.Int.MaxValue.toLong << 3
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Mem8 =
    apply(unit.size * count)
  
  override def apply(size: Long): Mem8 = {
    scala.Predef.require(0L <= size && size <= MaxSize)
    val words = new scala.Array[Long]((align(size, 8L) >> 3).toInt)
    new Mem8(words)
  }
  
  def unapply(mem: Mem8): Some[scala.Array[Long]] = Some(mem.words)
  
  override def toString: java.lang.String = "Mem8"
}
