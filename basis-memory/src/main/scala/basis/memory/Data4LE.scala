/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.memory

/** Little-endian data backed by an `Int` array.
  * 
  * @author Chris Sachs
  * @since  0.0
  */
private[memory] final class Data4LE(override val words: Array[Int]) extends Data4 with DataLE {
  override def endian: LittleEndian.type = LittleEndian
  
  override def copy(size: Long): Data4LE = {
    Predef.require(0L <= size && size <= (Int.MaxValue.toLong << 2))
    val words = new Array[Int]((align(size, 4L) >> 2).toInt)
    java.lang.System.arraycopy(this.words, 0, words, 0, java.lang.Math.min(this.words.length, words.length))
    new Data4LE(words)
  }
  
  override def loadByte(address: Long): Byte = {
    val i = (address >> 2).toInt
    val j = (address.toInt & 3) << 3
    (words(i) >>> j).toByte
  }
  
  override def storeByte(address: Long, value: Byte) {
    val i = (address >> 2).toInt
    val j = (address.toInt & 3) << 3
    words(i) = (words(i) & ~(0xFF << j)) | ((value & 0xFF) << j)
  }
  
  override def loadShort(address: Long): Short = {
    val i = (address >> 2).toInt
    val j = (address.toInt & 2) << 3
    (words(i) >>> j).toShort
  }
  
  override def storeShort(address: Long, value: Short) {
    val i = (address >> 2).toInt
    val j = (address.toInt & 2) << 3
    words(i) = (words(i) & ~(0xFFFF << j)) | ((value & 0xFFFF) << j)
  }
  
  override def loadInt(address: Long): Int =
    words((address >> 2).toInt)
  
  override def storeInt(address: Long, value: Int): Unit =
    words((address >> 2).toInt) = value
  
  override def loadLong(address: Long): Long = {
    val i = (address >> 2).toInt & ~1
    (words(i).toLong     & 0xFFFFFFFFL)        |
    (words(i + 1).toLong                << 32)
  }
  
  override def storeLong(address: Long, value: Long) {
    val i = (address >> 2).toInt & ~1
    words(i) = value.toInt
    words(i + 1) = (value >>> 32).toInt
  }
  
  override def toString: String = "Data4LE"+"("+ size +")"
}

/** An allocator for little-endian data backed by an `Int` array. */
private[memory] object Data4LE extends Allocator with (Long => Data4LE) {
  override def MaxSize: Long = Int.MaxValue.toLong << 2
  
  override def alloc[T](count: Long)(implicit T: Struct[T]): Data4LE = apply(T.size * count)
  
  override def apply(size: Long): Data4LE = {
    Predef.require(0L <= size && size <= MaxSize)
    val words = new Array[Int]((align(size, 4L) >> 2).toInt)
    new Data4LE(words)
  }
  
  override def toString: String = "Data4LE"
}
