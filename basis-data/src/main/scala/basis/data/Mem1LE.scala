/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

/** Little-endian memory backed by a `Byte` array. */
class Mem1LE(val words: scala.Array[Byte]) extends AnyVal with MemLE {
  import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
  import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
  
  override def size: Long = words.length.toLong
  
  override def unit: Int = 1
  
  override def endian: LittleEndian.type = LittleEndian
  
  override def copy(size: Long = this.size): Mem1LE = {
    scala.Predef.require(0L <= size && size <= scala.Int.MaxValue.toLong)
    val words = new scala.Array[Byte](size.toInt)
    java.lang.System.arraycopy(this.words, 0, words, 0, scala.math.min(this.words.length, words.length))
    new Mem1LE(words)
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
    val i = address.toInt
    ((words(i)     & 0xFF)       |
     (words(i + 1)         << 8)).toShort
  }
  
  override def storeUnalignedShort(address: Long, value: Short) {
    val i = address.toInt
    words(i)     =  value.toByte
    words(i + 1) = (value >> 8).toByte
  }
  
  override def loadUnalignedInt(address: Long): Int = {
    val i = address.toInt
     (words(i)     & 0xFF)        |
    ((words(i + 1) & 0xFF) <<  8) |
    ((words(i + 2) & 0xFF) << 16) |
     (words(i + 3)         << 24)
  }
  
  override def storeUnalignedInt(address: Long, value: Int) {
    val i = address.toInt
    words(i)     =  value.toByte
    words(i + 1) = (value >>  8).toByte
    words(i + 2) = (value >> 16).toByte
    words(i + 3) = (value >> 24).toByte
  }
  
  override def loadUnalignedLong(address: Long): Long = {
    val i = address.toInt
     (words(i)     & 0xFF).toLong        |
    ((words(i + 1) & 0xFF).toLong <<  8) |
    ((words(i + 2) & 0xFF).toLong << 16) |
    ((words(i + 3) & 0xFF).toLong << 24) |
    ((words(i + 4) & 0xFF).toLong << 32) |
    ((words(i + 5) & 0xFF).toLong << 40) |
    ((words(i + 6) & 0xFF).toLong << 48) |
     (words(i + 7).toLong         << 56)
  }
  
  override def storeUnalignedLong(address: Long, value: Long) {
    val i = address.toInt
    words(i)     =  value.toByte
    words(i + 1) = (value >>  8).toByte
    words(i + 2) = (value >> 16).toByte
    words(i + 3) = (value >> 24).toByte
    words(i + 4) = (value >> 32).toByte
    words(i + 5) = (value >> 40).toByte
    words(i + 6) = (value >> 48).toByte
    words(i + 7) = (value >> 56).toByte
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
  
  override def toString: java.lang.String = "Mem1LE"+"("+ size +")"
}

/** An allocator for little-endian memory backed by a `Byte` array. */
object Mem1LE extends Allocator with (Long => Mem1LE) {
  override def MaxSize: Long = scala.Int.MaxValue.toLong
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Mem1LE =
    apply(unit.size * count)
  
  override def apply(size: Long): Mem1LE = {
    scala.Predef.require(0L <= size && size <= MaxSize)
    val words = new scala.Array[Byte](size.toInt)
    new Mem1LE(words)
  }
  
  def unapply(mem: Mem1LE): Some[scala.Array[Byte]] = Some(mem.words)
  
  override def toString: java.lang.String = "Mem1LE"
}
