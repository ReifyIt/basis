//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

import basis.util._

trait Writer extends Storer {
  def += (offset: Long): Unit

  def -= (offset: Long): Unit

  def align(alignment: Long): Unit

  def truncate(alignment: Long): Unit

  def writeByte(value: Byte): Unit = {
    storeByte(0L, value)
    this += 1L
  }

  def writeShort(value: Short): Unit = {
    truncate(2L)
    storeUnalignedShort(0L, value)
    this += 2L
  }

  def writeInt(value: Int): Unit = {
    truncate(4L)
    storeUnalignedInt(0L, value)
    this += 4L
  }

  def writeLong(value: Long): Unit = {
    truncate(8L)
    storeUnalignedLong(0L, value)
    this += 8L
  }

  def writeFloat(value: Float): Unit = {
    truncate(4L)
    storeUnalignedFloat(0L, value)
    this += 4L
  }

  def writeDouble(value: Double): Unit = {
    truncate(8L)
    storeUnalignedDouble(0L, value)
    this += 8L
  }

  def writeUnalignedShort(value: Short): Unit = {
    storeUnalignedShort(0L, value)
    this += 2L
  }

  def writeUnalignedInt(value: Int): Unit = {
    storeUnalignedInt(0L, value)
    this += 4L
  }

  def writeUnalignedLong(value: Long): Unit = {
    storeUnalignedLong(0L, value)
    this += 8L
  }

  def writeUnalignedFloat(value: Float): Unit =
    writeUnalignedInt(value.toIntBits)

  def writeUnalignedDouble(value: Double): Unit =
    writeUnalignedLong(value.toLongBits)

  def write[T](value: T)(implicit T: Struct[T]): Unit = {
    T.store(this, 0L, value)
    this += T.size
  }

  def writeArray[T](array: Array[T], start: Int, count: Int)(implicit T: Struct[T]): Unit = {
    storeArray[T](0L, array, start, count)
    this += T.size * count
  }
}
