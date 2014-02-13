//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

import basis.collections._
import basis.util._

/** Big-endian data backed by an `Int` array.
  *
  * @author Chris Sachs
  * @since  0.0
  */
private[memory] final class Data4BE(override val words: Array[Int]) extends Data4 with DataBE {
  override def endian: BigEndian.type = BigEndian

  override def copy(size: Long): Data4BE = {
    if (size < 0L || size > Data4BE.MaxSize) throw new DataSizeException(size.toString)
    val words = new Array[Int]((align(size, 4L) >> 2).toInt)
    java.lang.System.arraycopy(this.words, 0, words, 0, this.words.length min words.length)
    new Data4BE(words)
  }

  override def loadByte(address: Long): Byte = {
    val i = (address >> 2).toInt
    val j = ((address.toInt & 3) ^ 3) << 3
    (words(i) >>> j).toByte
  }

  override def storeByte(address: Long, value: Byte): Unit = {
    val i = (address >> 2).toInt
    val j = ((address.toInt & 3) ^ 3) << 3
    words(i) = (words(i) & ~(0xFF << j)) | ((value & 0xFF) << j)
  }

  override def loadShort(address: Long): Short = {
    val i = (address >> 2).toInt
    val j = ((address.toInt & 2) ^ 2) << 3
    (words(i) >>> j).toShort
  }

  override def storeShort(address: Long, value: Short): Unit = {
    val i = (address >> 2).toInt
    val j = ((address.toInt & 2) ^ 2) << 3
    words(i) = (words(i) & ~(0xFFFF << j)) | ((value & 0xFFFF) << j)
  }

  override def loadInt(address: Long): Int =
    words((address >> 2).toInt)

  override def storeInt(address: Long, value: Int): Unit =
    words((address >> 2).toInt) = value

  override def loadLong(address: Long): Long = {
    val i = (address >> 2).toInt & ~1
    (words(i).toLong                    << 32) |
    (words(i + 1).toLong & 0xFFFFFFFFL)
  }

  override def storeLong(address: Long, value: Long): Unit = {
    val i = (address >> 2).toInt & ~1
    words(i)     = (value >>> 32).toInt
    words(i + 1) = value.toInt
  }

  override def toString: String = "Data4BE"+"("+ size +")"
}

/** An allocator for big-endian data backed by an `Int` array. */
private[memory] object Data4BE extends Allocator[Data4BE] {
  override def MaxSize: Long = Int.MaxValue.toLong << 2

  override def Endian: Endianness = BigEndian

  override def alloc[T](count: Long)(implicit T: Struct[T]): Data4BE = apply(T.size * count)

  override def apply(size: Long): Data4BE = {
    if (size < 0L || size > MaxSize) throw new DataSizeException(size.toString)
    val words = new Array[Int]((align(size, 4L) >> 2).toInt)
    new Data4BE(words)
  }

  override def realloc(data: Loader, size: Long): Data4BE = {
    if (data.isInstanceOf[Data4BE]) data.asInstanceOf[Data4BE].copy(size)
    else super.realloc(data, size)
  }

  override def Framer(): Framer with State[Data4BE] = new DataFramer(this)

  override def toString: String = "Data4BE"
}
