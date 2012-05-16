/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** Little-endian `Int` array backed data. */
final class IntDataLE(val array: Array[Int]) extends AnyVal with ArrayData[Int] {
  @inline override def size: Long = array.length.toLong << 2
  
  @inline override def unit: Int = 4
  
  @inline override def endian = Endianness.LittleEndian
  
  override def copy(size: Long): IntDataLE = {
    require(0L <= size && size <= (Int.MaxValue.toLong << 2))
    val array = new Array[Int]((align(4L)(size) >> 2).toInt)
    Array.copy(this.array, 0, array, 0, math.min(this.array.length, array.length))
    new IntDataLE(array)
  }
  
  override def loadByte(address: Long): Byte = {
    val i = (address >> 2).toInt
    val j = (address.toInt & 3) << 3
    (array(i) >>> j).toByte
  }
  
  override def storeByte(address: Long, value: Byte) {
    val i = (address >> 2).toInt
    val j = (address.toInt & 3) << 3
    array(i) = (array(i) & ~(0xFF << j)) | ((value & 0xFF) << j)
  }
  
  override def loadShort(address: Long): Short = {
    val i = (address >> 2).toInt
    val j = (address.toInt & 2) << 3
    (array(i) >>> j).toShort
  }
  
  override def storeShort(address: Long, value: Short) {
    val i = (address >> 2).toInt
    val j = (address.toInt & 2) << 3
    array(i) = (array(i) & ~(0xFFFF << j)) | ((value & 0xFFFF) << j)
  }
  
  @inline override def loadInt(address: Long): Int =
    array((address >> 2).toInt)
  
  @inline override def storeInt(address: Long, value: Int): Unit =
    array((address >> 2).toInt) = value
  
  override def loadLong(address: Long): Long = {
    val i = (address >> 2).toInt & ~1
    (array(i).toLong     & 0xFFFFFFFFL)        |
    (array(i + 1).toLong                << 32)
  }
  
  override def storeLong(address: Long, value: Long) {
    val i = (address >> 2).toInt & ~1
    array(i) = value.toInt
    array(i + 1) = (value >>> 32).toInt
  }
  
  override def toString: String = "IntDataLE"+"("+ size +")"
}

/** An allocator for little-endian data backed by an `Int` array. */
object IntDataLE extends ArrayAllocator[Int] {
  override def MaxSize: Long = Int.MaxValue.toLong << 2
  
  override def alloc[T](count: Long)(implicit unit: Struct[T]): IntDataLE =
    apply(unit.size * count)
  
  override def apply(size: Long): IntDataLE = {
    require(0L <= size && size <= MaxSize)
    val array = new Array[Int]((align(4L)(size) >> 2).toInt)
    new IntDataLE(array)
  }
  
  override def wrap(array: Array[Int]): IntDataLE = new IntDataLE(array)
  
  def unapply(data: IntDataLE): Some[Array[Int]] = Some(data.array)
  
  override def toString: String = "IntDataLE"
}
