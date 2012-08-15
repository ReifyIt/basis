/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** Native-endian `Byte` array backed data. */
final class Data1(final val words: Array[Byte]) extends AnyVal with Data {
  import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
  import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
  
  @inline final override def size: Long = words.length.toLong
  
  final override def unit: Int = 1
  
  final override def endian: NativeEndian.type = NativeEndian
  
  final override def copy(size: Long = this.size): Data1 = {
    require(0L <= size && size <= Int.MaxValue.toLong)
    val words = new Array[Byte](size.toInt)
    System.arraycopy(this.words, 0, words, 0, math.min(this.words.length, words.length))
    new Data1(words)
  }
  
  @inline final override def loadByte(address: Long): Byte =
    words(address.toInt)
  
  @inline final override def storeByte(address: Long, value: Byte): Unit =
    words(address.toInt) = value
  
  final override def loadShort(address: Long): Short =
    loadUnalignedShort(address & -2L)
  
  final override def storeShort(address: Long, value: Short): Unit =
    storeUnalignedShort(address & -2L, value)
  
  final override def loadInt(address: Long): Int =
    loadUnalignedInt(address & -4L)
  
  final override def storeInt(address: Long, value: Int): Unit =
    storeUnalignedInt(address & -4L, value)
  
  final override def loadLong(address: Long): Long =
    loadUnalignedLong(address & -8L)
  
  final override def storeLong(address: Long, value: Long): Unit =
    storeUnalignedLong(address & -8L, value)
  
  final override def loadChar(address: Long): Char =
    loadUnalignedShort(address & -2L).toChar
  
  final override def storeChar(address: Long, value: Char): Unit =
    storeUnalignedShort(address & -2L, value.toShort)
  
  final override def loadFloat(address: Long): Float =
    intBitsToFloat(loadUnalignedInt(address & -4L))
  
  final override def storeFloat(address: Long, value: Float): Unit =
    storeUnalignedInt(address & -4L, floatToRawIntBits(value))
  
  final override def loadDouble(address: Long): Double =
    longBitsToDouble(loadUnalignedLong(address & -8L))
  
  final override def storeDouble(address: Long, value: Double): Unit =
    storeUnalignedLong(address & -8L, doubleToRawLongBits(value))
  
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
  
  final override def move(fromAddress: Long, toAddress: Long, size: Long): Unit =
    System.arraycopy(words, fromAddress.toInt, words, toAddress.toInt, size.toInt)
  
  final override def clear(fromAddress: Long, untilAddress: Long): Unit =
    java.util.Arrays.fill(words, fromAddress.toInt, untilAddress.toInt, 0.toByte)
  
  @inline final def toBE: Data1BE = new Data1BE(words)
  
  @inline final def toLE: Data1LE = new Data1LE(words)
  
  override def toString: String = "Data1"+"("+ size +")"
}

/** An allocator for native-endian data backed by a `Byte` array. */
object Data1 extends Allocator with (Long => Data1) {
  override def MaxSize: Long = Int.MaxValue.toLong
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Data1 =
    apply(unit.size * count)
  
  override def apply(size: Long): Data1 = {
    require(0L <= size && size <= MaxSize)
    val words = new Array[Byte](size.toInt)
    new Data1(words)
  }
  
  def unapply(data: Data1): Some[Array[Byte]] = Some(data.words)
  
  override def toString: String = "Data1"
}
