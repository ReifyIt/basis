//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

import basis.collections._

/** Native-endian data backed by a `Byte` array.
  *
  * @author Chris Sachs
  * @since  0.0
  */
abstract class Data1 extends Data {
  def words: Array[Byte]

  override def size: Long = words.length.toLong

  override def unit: Int = 1

  override def copy(size: Long): Data1

  override def move(fromAddress: Long, toAddress: Long, size: Long): Unit =
    java.lang.System.arraycopy(words, fromAddress.toInt, words, toAddress.toInt, size.toInt)

  override def clear(fromAddress: Long, untilAddress: Long): Unit =
    java.util.Arrays.fill(words, fromAddress.toInt, untilAddress.toInt, 0.toByte)
}

/** An allocator for native-endian data backed by a `Byte` array. */
object Data1 extends Allocator[Data1] {
  override def MaxSize: Long = Int.MaxValue.toLong

  override def Endian: Endianness = NativeEndian

  override def alloc[T](count: Long)(implicit T: Struct[T]): Data1 = NativeEndian match {
    case BigEndian    => Data1BE.alloc[T](count)(T)
    case LittleEndian => Data1LE.alloc[T](count)(T)
  }

  override def apply(size: Long): Data1 = NativeEndian match {
    case BigEndian    => Data1BE(size)
    case LittleEndian => Data1LE(size)
  }

  override def realloc(data: Loader, size: Long): Data1 = {
    if (data.isInstanceOf[Data1]) data.asInstanceOf[Data1].copy(size)
    else super.realloc(data, size)
  }

  override def Framer(): Framer with State[Data1] = new DataFramer(this)

  def BE: Allocator[Data1] = Data1BE

  def LE: Allocator[Data1] = Data1LE

  override def toString: String = "Data1"
}
