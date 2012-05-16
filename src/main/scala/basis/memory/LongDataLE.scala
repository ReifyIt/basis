/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** Little-endian `Long` array backed data. */
final class LongDataLE(val array: Array[Long]) extends AnyVal with ArrayData[Long] {
  @inline override def size: Long = array.length.toLong << 3
  
  @inline override def unit: Int = 8
  
  @inline override def endian = Endianness.LittleEndian
  
  override def copy(size: Long): LongDataLE = {
    require(0L <= size && size <= (Int.MaxValue.toLong << 3))
    val array = new Array[Long]((align(8L)(size) >> 3).toInt)
    Array.copy(this.array, 0, array, 0, math.min(this.array.length, array.length))
    new LongDataLE(array)
  }
  
  override def loadByte(address: Long): Byte = {
    val i = (address >> 3).toInt
    val j = (address.toInt & 7) << 3
    (array(i) >>> j).toByte
  }
  
  override def storeByte(address: Long, value: Byte) {
    val i = (address >> 3).toInt
    val j = (address.toInt & 7) << 3
    array(i) = (array(i) & ~(0xFFL << j)) | ((value & 0xFF).toLong << j)
  }
  
  override def loadShort(address: Long): Short = {
    val i = (address >> 3).toInt
    val j = (address.toInt & 6) << 3
    (array(i) >>> j).toShort
  }
  
  override def storeShort(address: Long, value: Short) {
    val i = (address >> 3).toInt
    val j = (address.toInt & 6) << 3
    array(i) = (array(i) & ~(0xFFFFL << j)) | ((value & 0xFFFF).toLong << j)
  }
  
  override def loadInt(address: Long): Int = {
    val i = (address >> 3).toInt
    val j = (address.toInt & 4) << 3
    (array(i) >>> j).toInt
  }
  
  override def storeInt(address: Long, value: Int) {
    val i = (address >> 3).toInt
    val j = (address.toInt & 4) << 3
    array(i) = (array(i) & ~(0xFFFFFFFFL << j)) | ((value & 0xFFFFFFFFL) << j)
  }
  
  @inline override def loadLong(address: Long): Long =
    array((address >> 3).toInt)
  
  @inline override def storeLong(address: Long, value: Long): Unit =
    array((address >> 3).toInt) = value
  
  override def toString: String = "LongDataLE"+"("+ size +")"
}

/** An allocator for little-endian data backed by a `Long` array. */
object LongDataLE extends ArrayAllocator[Long] {
  override def MaxSize: Long = Int.MaxValue.toLong << 3
  
  override def alloc[T](count: Long)(implicit unit: Struct[T]): LongDataLE =
    apply(unit.size * count)
  
  override def apply(size: Long): LongDataLE = {
    require(0L <= size && size <= MaxSize)
    val array = new Array[Long]((align(8L)(size) >> 3).toInt)
    new LongDataLE(array)
  }
  
  override def wrap(array: Array[Long]): LongDataLE = new LongDataLE(array)
  
  def unapply(data: LongDataLE): Some[Array[Long]] = Some(data.array)
  
  override def toString: String = "LongDataLE"
}
