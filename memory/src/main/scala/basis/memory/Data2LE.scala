//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

import basis.collections._
import basis.util._

/** Little-endian data backed by a `Short` array.
  *
  * @author Chris Sachs
  * @since  0.0
  */
private[memory] final class Data2LE(override val words: Array[Short]) extends Data2 with DataLE {
  override def endian: LittleEndian.type = LittleEndian

  override def copy(size: Long): Data2LE = {
    if (size < 0L || size > Data2LE.MaxSize) throw new DataSizeException(size.toString)
    val words = new Array[Short]((align(size, 2L) >> 1).toInt)
    java.lang.System.arraycopy(this.words, 0, words, 0, this.words.length min words.length)
    new Data2LE(words)
  }

  override def loadByte(address: Long): Byte = {
    val i = (address >> 1).toInt
    val j = (address.toInt & 1) << 3
    (words(i) >>> j).toByte
  }

  override def storeByte(address: Long, value: Byte): Unit = {
    val i = (address >> 1).toInt
    val j = (address.toInt & 1) << 3
    words(i) = ((words(i) & ~(0xFF << j)) | ((value & 0xFF) << j)).toShort
  }

  override def loadShort(address: Long): Short =
    words((address >> 1).toInt)

  override def storeShort(address: Long, value: Short): Unit =
    words((address >> 1).toInt) = value

  override def loadInt(address: Long): Int = {
    val i = (address >> 1).toInt & -2
    (words(i)     & 0xFFFF)        |
    (words(i + 1)           << 16)
  }

  override def storeInt(address: Long, value: Int): Unit = {
    val i = (address >> 1).toInt & -2
    words(i)     =  value.toShort
    words(i + 1) = (value >>> 16).toShort
  }

  override def loadLong(address: Long): Long = {
    val i = (address >> 1).toInt & -4
     (words(i)     & 0xFFFF).toLong        |
    ((words(i + 1) & 0xFFFF).toLong << 16) |
    ((words(i + 2) & 0xFFFF).toLong << 32) |
     (words(i + 3).toLong           << 48)
  }

  override def storeLong(address: Long, value: Long): Unit = {
    val i = (address >> 1).toInt & -4
    words(i)     =  value.toShort
    words(i + 1) = (value >>> 16).toShort
    words(i + 2) = (value >>> 32).toShort
    words(i + 3) = (value >>> 48).toShort
  }

  override def toString: String = "Data2LE"+"("+ size +")"
}

/** An allocator for little-endian data backed by a `Short` array. */
private[memory] object Data2LE extends Allocator[Data2LE] {
  override def MaxSize: Long = Int.MaxValue.toLong << 1

  override def Endian: Endianness = LittleEndian

  override def alloc[T](count: Long)(implicit T: Struct[T]): Data2LE = apply(T.size * count)

  override def apply(size: Long): Data2LE = {
    if (size < 0L || size > MaxSize) throw new DataSizeException(size.toString)
    val words = new Array[Short]((align(size, 2L) >> 1).toInt)
    new Data2LE(words)
  }

  override def realloc(data: Loader, size: Long): Data2LE = {
    if (data.isInstanceOf[Data2LE]) data.asInstanceOf[Data2LE].copy(size)
    else super.realloc(data, size)
  }

  override def Framer(): Framer with State[Data2LE] = new DataFramer(this)

  override def toString: String = "Data2LE"
}
