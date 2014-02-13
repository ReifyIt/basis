//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

import basis.collections._
import basis.util._

/** Big-endian data backed by a `Short` array.
  *
  * @author Chris Sachs
  * @since  0.0
  */
private[memory] final class Data2BE(override val words: Array[Short]) extends Data2 with DataBE {
  override def endian: BigEndian.type = BigEndian

  override def copy(size: Long): Data2BE = {
    if (size < 0L || size > Data2BE.MaxSize) throw new DataSizeException(size.toString)
    val words = new Array[Short]((align(size, 2L) >> 1).toInt)
    java.lang.System.arraycopy(this.words, 0, words, 0, this.words.length min words.length)
    new Data2BE(words)
  }

  override def loadByte(address: Long): Byte = {
    val i = (address >> 1).toInt
    val j = ((address.toInt & 1) ^ 1) << 3
    (words(i) >>> j).toByte
  }

  override def storeByte(address: Long, value: Byte): Unit = {
    val i = (address >> 1).toInt
    val j = ((address.toInt & 1) ^ 1) << 3
    words(i) = ((words(i) & ~(0xFF << j)) | ((value & 0xFF) << j)).toShort
  }

  override def loadShort(address: Long): Short =
    words((address >> 1).toInt)

  override def storeShort(address: Long, value: Short): Unit =
    words((address >> 1).toInt) = value

  override def loadInt(address: Long): Int = {
    val i = (address >> 1).toInt & -2
    (words(i)               << 16) |
    (words(i + 1) & 0xFFFF)
  }

  override def storeInt(address: Long, value: Int): Unit = {
    val i = (address >> 1).toInt & -2
    words(i)     = (value >>> 16).toShort
    words(i + 1) =  value.toShort
  }

  override def loadLong(address: Long): Long = {
    val i = (address >> 1).toInt & -4
     (words(i).toLong               << 48) |
    ((words(i + 1) & 0xFFFF).toLong << 32) |
    ((words(i + 2) & 0xFFFF).toLong << 16) |
     (words(i + 3) & 0xFFFF).toLong
  }

  override def storeLong(address: Long, value: Long): Unit = {
    val i = (address >> 1).toInt & -4
    words(i)     = (value >>> 48).toShort
    words(i + 1) = (value >>> 32).toShort
    words(i + 2) = (value >>> 16).toShort
    words(i + 3) =  value.toShort
  }

  override def toString: String = "Data2BE"+"("+ size +")"
}

/** An allocator for big-endian data backed by a `Short` array. */
private[memory] object Data2BE extends Allocator[Data2BE] {
  override def MaxSize: Long = Int.MaxValue.toLong << 1

  override def Endian: Endianness = BigEndian

  override def alloc[T](count: Long)(implicit T: Struct[T]): Data2BE = apply(T.size * count)

  override def apply(size: Long): Data2BE = {
    if (size < 0L || size > MaxSize) throw new DataSizeException(size.toString)
    val words = new Array[Short]((align(size, 2L) >> 1).toInt)
    new Data2BE(words)
  }

  override def realloc(data: Loader, size: Long): Data2BE = {
    if (data.isInstanceOf[Data2BE]) data.asInstanceOf[Data2BE].copy(size)
    else super.realloc(data, size)
  }

  override def Framer(): Framer with State[Data2BE] = new DataFramer(this)

  override def toString: String = "Data2BE"
}
