/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** Big-endian `Int` array backed data. */
final class IntDataBE(val array: Array[Int]) extends AnyVal with ArrayData[Int] {
  @inline override def size: Long = array.length.toLong << 2
  
  @inline override def unit: Int = 4
  
  @inline override def endian = Endianness.BigEndian
  
  override def copy(size: Long): IntDataBE = {
    require(0L <= size && size <= (Int.MaxValue.toLong << 2))
    val array = new Array[Int]((align(4L)(size) >> 2).toInt)
    Array.copy(this.array, 0, array, 0, math.min(this.array.length, array.length))
    new IntDataBE(array)
  }
  
  override def loadByte(address: Long): Byte = {
    val i = (address >> 2).toInt
    val j = ((address.toInt & 3) ^ 3) << 3
    (array(i) >>> j).toByte
  }
  
  override def storeByte(address: Long, value: Byte) {
    val i = (address >> 2).toInt
    val j = ((address.toInt & 3) ^ 3) << 3
    array(i) = (array(i) & ~(0xFF << j)) | ((value & 0xFF) << j)
  }
  
  override def loadShort(address: Long): Short = {
    val i = (address >> 2).toInt
    val j = ((address.toInt & 2) ^ 2) << 3
    (array(i) >>> j).toShort
  }
  
  override def storeShort(address: Long, value: Short) {
    val i = (address >> 2).toInt
    val j = ((address.toInt & 2) ^ 2) << 3
    array(i) = (array(i) & ~(0xFFFF << j)) | ((value & 0xFFFF) << j)
  }
  
  @inline override def loadInt(address: Long): Int =
    array((address >> 2).toInt)
  
  @inline override def storeInt(address: Long, value: Int): Unit =
    array((address >> 2).toInt) = value
  
  override def loadLong(address: Long): Long = {
    val i = (address >> 2).toInt & ~1
    (array(i).toLong                    << 32) |
    (array(i + 1).toLong & 0xFFFFFFFFL)
  }
  
  override def storeLong(address: Long, value: Long) {
    val i = (address >> 2).toInt & ~1
    array(i)     = (value >>> 32).toInt
    array(i + 1) = value.toInt
  }
  
  override def toString: String = "IntDataBE"+"("+ size +")"
}

/** An allocator for big-endian data backed by an `Int` array. */
object IntDataBE extends ArrayAllocator[Int] {
  override def MaxSize: Long = Int.MaxValue.toLong << 2
  
  override def alloc[T](count: Long)(implicit unit: Struct[T]): IntDataBE =
    apply(unit.size * count)
  
  override def apply(size: Long): IntDataBE = {
    require(0L <= size && size <= MaxSize)
    val array = new Array[Int]((align(4L)(size) >> 2).toInt)
    new IntDataBE(array)
  }
  
  override def wrap(array: Array[Int]): IntDataBE = new IntDataBE(array)
  
  def unapply(data: IntDataBE): Some[Array[Int]] = Some(data.array)
  
  override def toString: String = "IntDataBE"
}
