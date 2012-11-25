/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** Big-endian data backed by a `Long` array. */
private[memory] final class Data8BE(override val words: Array[Long]) extends Data8 with DataBE {
  override def endian: BigEndian.type = BigEndian
  
  override def copy(size: Long): Data8BE = {
    Predef.require(0L <= size && size <= (Int.MaxValue.toLong << 3))
    val words = new Array[Long]((align(size, 8L) >> 3).toInt)
    java.lang.System.arraycopy(this.words, 0, words, 0, java.lang.Math.min(this.words.length, words.length))
    new Data8BE(words)
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
  
  override def toString: String = "Data8BE"+"("+ size +")"
}

/** An allocator for big-endian data backed by a `Long` array. */
private[memory] object Data8BE extends Allocator with (Long => Data8BE) {
  override def MaxSize: Long = Int.MaxValue.toLong << 3
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Data8BE =
    apply(unit.size * count)
  
  override def apply(size: Long): Data8BE = {
    Predef.require(0L <= size && size <= MaxSize)
    val words = new Array[Long]((align(size, 8L) >> 3).toInt)
    new Data8BE(words)
  }
  
  override def toString: String = "Data8BE"
}
