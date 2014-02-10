//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

import basis.util._

/** Little-endian data backed by a `Long` array.
  *
  * @author Chris Sachs
  * @since  0.0
  */
private[memory] final class Data8LE(override val words: Array[Long]) extends Data8 with DataLE {
  override def endian: LittleEndian.type = LittleEndian

  override def copy(size: Long): Data8LE = {
    if (size < 0L || size > Data8LE.MaxSize) throw new DataSizeException(size.toString)
    val words = new Array[Long]((align(size, 8L) >> 3).toInt)
    java.lang.System.arraycopy(this.words, 0, words, 0, this.words.length min words.length)
    new Data8LE(words)
  }

  override def loadByte(address: Long): Byte = {
    val i = (address >> 3).toInt
    val j = (address.toInt & 7) << 3
    (words(i) >>> j).toByte
  }

  override def storeByte(address: Long, value: Byte): Unit = {
    val i = (address >> 3).toInt
    val j = (address.toInt & 7) << 3
    words(i) = (words(i) & ~(0xFFL << j)) | ((value & 0xFF).toLong << j)
  }

  override def loadShort(address: Long): Short = {
    val i = (address >> 3).toInt
    val j = (address.toInt & 6) << 3
    (words(i) >>> j).toShort
  }

  override def storeShort(address: Long, value: Short): Unit = {
    val i = (address >> 3).toInt
    val j = (address.toInt & 6) << 3
    words(i) = (words(i) & ~(0xFFFFL << j)) | ((value & 0xFFFF).toLong << j)
  }

  override def loadInt(address: Long): Int = {
    val i = (address >> 3).toInt
    val j = (address.toInt & 4) << 3
    (words(i) >>> j).toInt
  }

  override def storeInt(address: Long, value: Int): Unit = {
    val i = (address >> 3).toInt
    val j = (address.toInt & 4) << 3
    words(i) = (words(i) & ~(0xFFFFFFFFL << j)) | ((value & 0xFFFFFFFFL) << j)
  }

  override def loadLong(address: Long): Long =
    words((address >> 3).toInt)

  override def storeLong(address: Long, value: Long): Unit =
    words((address >> 3).toInt) = value

  override def toString: String = "Data8LE"+"("+ size +")"
}

/** An allocator for little-endian data backed by a `Long` array. */
private[memory] object Data8LE extends Allocator with (Long => Data8LE) {
  override def MaxSize: Long = Int.MaxValue.toLong << 3

  override def Endian: Endianness = LittleEndian

  override def alloc[T](count: Long)(implicit T: Struct[T]): Data8LE = apply(T.size * count)

  override def apply(size: Long): Data8LE = {
    if (size < 0L || size > MaxSize) throw new DataSizeException(size.toString)
    val words = new Array[Long]((align(size, 8L) >> 3).toInt)
    new Data8LE(words)
  }

  override def toString: String = "Data8LE"
}
