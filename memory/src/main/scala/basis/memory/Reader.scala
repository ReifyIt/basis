//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

import basis.util._

trait Reader extends Loader {
  def += (offset: Long): Unit

  def -= (offset: Long): Unit

  def align(alignment: Long): Unit

  def truncate(alignment: Long): Unit

  def readByte(): Byte = {
    val value = loadByte(0L)
    this += 1L
    value
  }

  def readShort(): Short = {
    truncate(2L)
    val value = loadUnalignedShort(0L)
    this += 2L
    value
  }

  def readInt(): Int = {
    truncate(4L)
    val value = loadUnalignedInt(0L)
    this += 4L
    value
  }

  def readLong(): Long = {
    truncate(8L)
    val value = loadUnalignedLong(0L)
    this += 8L
    value
  }

  def readFloat(): Float = {
    truncate(4L)
    val value = loadUnalignedFloat(0L)
    this += 4L
    value
  }

  def readDouble(): Double = {
    truncate(8L)
    val value = loadUnalignedDouble(0L)
    this += 8L
    value
  }

  def readUnalignedShort(): Short = {
    val value = loadUnalignedShort(0L)
    this += 2L
    value
  }

  def readUnalignedInt(): Int = {
    val value = loadUnalignedInt(0L)
    this += 4L
    value
  }

  def readUnalignedLong(): Long = {
    val value = loadUnalignedLong(0L)
    this += 8L
    value
  }

  def readUnalignedFloat(): Float =
    readUnalignedInt().toFloatBits

  def readUnalignedDouble(): Double =
    readUnalignedLong().toDoubleBits

  def read[T](implicit T: Struct[T]): T = {
    val value = T.load(this, 0L)
    this += T.size
    value
  }

  def readArray[T](count: Int)(implicit T: Struct[T]): Array[T] = {
    val array = loadArray[T](0L, count)
    this += T.size * count
    array
  }

  def readToArray[T](array: Array[T], start: Int, count: Int)(implicit T: Struct[T]): Unit = {
    loadToArray[T](0L, array, start, count)
    this += T.size * count
  }
}
