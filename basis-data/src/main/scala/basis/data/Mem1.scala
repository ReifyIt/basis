/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

/** Native-endian memory backed by a `Byte` array. */
class Mem1(val words: scala.Array[Byte]) extends AnyVal with Mem {
  import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
  import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
  
  override def size: Long = words.length.toLong
  
  override def unit: Int = 1
  
  override def endian: NativeEndian.type = NativeEndian
  
  override def copy(size: Long = this.size): Mem1 = {
    scala.Predef.require(0L <= size && size <= scala.Int.MaxValue.toLong)
    val words = new scala.Array[Byte](size.toInt)
    java.lang.System.arraycopy(this.words, 0, words, 0, scala.math.min(this.words.length, words.length))
    new Mem1(words)
  }
  
  override def loadByte(address: Long): Byte =
    words(address.toInt)
  
  override def storeByte(address: Long, value: Byte): Unit =
    words(address.toInt) = value
  
  override def loadShort(address: Long): Short =
    loadUnalignedShort(address & -2L)
  
  override def storeShort(address: Long, value: Short): Unit =
    storeUnalignedShort(address & -2L, value)
  
  override def loadInt(address: Long): Int =
    loadUnalignedInt(address & -4L)
  
  override def storeInt(address: Long, value: Int): Unit =
    storeUnalignedInt(address & -4L, value)
  
  override def loadLong(address: Long): Long =
    loadUnalignedLong(address & -8L)
  
  override def storeLong(address: Long, value: Long): Unit =
    storeUnalignedLong(address & -8L, value)
  
  override def loadFloat(address: Long): Float =
    intBitsToFloat(loadUnalignedInt(address & -4L))
  
  override def storeFloat(address: Long, value: Float): Unit =
    storeUnalignedInt(address & -4L, floatToRawIntBits(value))
  
  override def loadDouble(address: Long): Double =
    longBitsToDouble(loadUnalignedLong(address & -8L))
  
  override def storeDouble(address: Long, value: Double): Unit =
    storeUnalignedLong(address & -8L, doubleToRawLongBits(value))
  
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
  
  override def move(fromAddress: Long, toAddress: Long, size: Long): Unit =
    java.lang.System.arraycopy(words, fromAddress.toInt, words, toAddress.toInt, size.toInt)
  
  override def clear(fromAddress: Long, untilAddress: Long): Unit =
    java.util.Arrays.fill(words, fromAddress.toInt, untilAddress.toInt, 0.toByte)
  
  def toBE: Mem1BE = new Mem1BE(words)
  
  def toLE: Mem1LE = new Mem1LE(words)
  
  override def toString: java.lang.String = "Mem1"+"("+ size +")"
}

/** An allocator for native-endian memory backed by a `Byte` array. */
object Mem1 extends Allocator with (Long => Mem1) {
  override def MaxSize: Long = scala.Int.MaxValue.toLong
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Mem1 =
    apply(unit.size * count)
  
  override def apply(size: Long): Mem1 = {
    scala.Predef.require(0L <= size && size <= MaxSize)
    val words = new scala.Array[Byte](size.toInt)
    new Mem1(words)
  }
  
  def unapply(mem: Mem1): Some[scala.Array[Byte]] = Some(mem.words)
  
  override def toString: java.lang.String = "Mem1"
}
