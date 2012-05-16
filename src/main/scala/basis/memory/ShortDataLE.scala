/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** Little-endian `Short` array backed data. */
final class ShortDataLE(val array: Array[Short]) extends AnyVal with ArrayData[Short] {
  @inline override def size: Long = array.length.toLong << 1
  
  @inline override def unit: Int = 2
  
  @inline override def endian = Endianness.LittleEndian
  
  override def copy(size: Long): ShortDataLE = {
    require(0L <= size && size <= (Int.MaxValue.toLong << 1))
    val array = new Array[Short]((align(2L)(size) >> 1).toInt)
    Array.copy(this.array, 0, array, 0, math.min(this.array.length, array.length))
    new ShortDataLE(array)
  }
  
  override def loadByte(address: Long): Byte = {
    val i = (address >> 1).toInt
    val j = (address.toInt & 1) << 3
    (array(i) >>> j).toByte
  }
  
  override def storeByte(address: Long, value: Byte) {
    val i = (address >> 1).toInt
    val j = (address.toInt & 1) << 3
    array(i) = ((array(i) & ~(0xFF << j)) | ((value & 0xFF) << j)).toShort
  }
  
  @inline override def loadShort(address: Long): Short =
    array((address >> 1).toInt)
  
  @inline override def storeShort(address: Long, value: Short): Unit =
    array((address >> 1).toInt) = value
  
  override def loadInt(address: Long): Int = {
    val i = (address >> 1).toInt & ~1
    (array(i)     & 0xFFFF)        |
    (array(i + 1)           << 16)
  }
  
  override def storeInt(address: Long, value: Int) {
    val i = (address >> 1).toInt & ~1
    array(i)     = value.toShort
    array(i + 1) = (value >>> 16).toShort
  }
  
  override def loadLong(address: Long): Long = {
    val i = (address >> 1).toInt & ~3
     (array(i)     & 0xFFFF).toLong        |
    ((array(i + 1) & 0xFFFF).toLong << 16) |
    ((array(i + 2) & 0xFFFF).toLong << 32) |
     (array(i + 3).toLong           << 48)
  }
  
  override def storeLong(address: Long, value: Long) {
    val i = (address >> 1).toInt & ~3
    array(i) =     value.toShort
    array(i + 1) = (value >>> 16).toShort
    array(i + 2) = (value >>> 32).toShort
    array(i + 3) = (value >>> 48).toShort
  }
  
  override def toString: String = "ShortDataLE"+"("+ size +")"
}

/** An allocator for little-endian data backed by a `Short` array. */
object ShortDataLE extends ArrayAllocator[Short] {
  override def MaxSize: Long = Int.MaxValue.toLong << 1
  
  override def alloc[T](count: Long)(implicit unit: Struct[T]): ShortDataLE =
    apply(unit.size * count)
  
  override def apply(size: Long): ShortDataLE = {
    require(0L <= size && size <= MaxSize)
    val array = new Array[Short]((align(2L)(size) >> 1).toInt)
    new ShortDataLE(array)
  }
  
  override def wrap(array: Array[Short]): ShortDataLE = new ShortDataLE(array)
  
  def unapply(data: ShortDataLE): Some[Array[Short]] = Some(data.array)
  
  override def toString: String = "ShortDataLE"
}
