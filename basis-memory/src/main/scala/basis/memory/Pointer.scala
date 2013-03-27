/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.memory

import basis.util._

/** An untyped, mutable pointer into a memory region.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  */
abstract class Pointer extends StaticPointer {
  def += (offset: Long): Unit
  
  def -= (offset: Long): Unit
  
  def align(alignment: Long): Unit
  
  def truncate(alignment: Long): Unit
  
  def isValid(offset: Long): Boolean
  
  def readByte(): Byte = {
    val value = loadByte(0L)
    this += 1L
    value
  }
  
  def writeByte(value: Byte) {
    storeByte(0L, value)
    this += 1L
  }
  
  def readShort(): Short = {
    truncate(2L)
    val value = loadUnalignedShort(0L)
    this += 2L
    value
  }
  
  def writeShort(value: Short) {
    truncate(2L)
    storeUnalignedShort(0L, value)
    this += 2L
  }
  
  def readInt(): Int = {
    truncate(4L)
    val value = loadUnalignedInt(0L)
    this += 4L
    value
  }
  
  def writeInt(value: Int) {
    truncate(4L)
    storeUnalignedInt(0L, value)
    this += 4L
  }
  
  def readLong(): Long = {
    truncate(8L)
    val value = loadUnalignedLong(0L)
    this += 8L
    value
  }
  
  def writeLong(value: Long) {
    truncate(8L)
    storeUnalignedLong(0L, value)
    this += 8L
  }
  
  def readFloat(): Float = {
    truncate(4L)
    val value = loadUnalignedFloat(0L)
    this += 4L
    value
  }
  
  def writeFloat(value: Float) {
    truncate(4L)
    storeUnalignedFloat(0L, value)
    this += 4L
  }
  
  def readDouble(): Double = {
    truncate(8L)
    val value = loadUnalignedDouble(0L)
    this += 8L
    value
  }
  
  def writeDouble(value: Double) {
    truncate(8L)
    storeUnalignedDouble(0L, value)
    this += 8L
  }
  
  def readUnalignedShort(): Short = {
    val value = loadUnalignedShort(0L)
    this += 2L
    value
  }
  
  def writeUnalignedShort(value: Short) {
    storeUnalignedShort(0L, value)
    this += 2L
  }
  
  def readUnalignedInt(): Int = {
    val value = loadUnalignedInt(0L)
    this += 4L
    value
  }
  
  def writeUnalignedInt(value: Int) {
    storeUnalignedInt(0L, value)
    this += 4L
  }
  
  def readUnalignedLong(): Long = {
    val value = loadUnalignedLong(0L)
    this += 8L
    value
  }
  
  def writeUnalignedLong(value: Long) {
    storeUnalignedLong(0L, value)
    this += 8L
  }
  
  def readUnalignedFloat(): Float =
    readUnalignedInt().toFloatBits
  
  def writeUnalignedFloat(value: Float): Unit =
    writeUnalignedInt(value.toIntBits)
  
  def readUnalignedDouble(): Double =
    readUnalignedLong().toDoubleBits
  
  def writeUnalignedDouble(value: Double): Unit =
    writeUnalignedLong(value.toLongBits)
  
  def read[T](implicit T: Struct[T]): T = {
    val value = T.load(this, 0L)
    this += T.size
    value
  }
  
  def write[T](value: T)(implicit T: Struct[T]) {
    T.store(this, 0L, value)
    this += T.size
  }
  
  def readArray[T](count: Int)(implicit T: Struct[T]): Array[T] = {
    val array = loadArray[T](0L, count)
    this += T.size * count
    array
  }
  
  def readToArray[T](array: Array[T], start: Int, count: Int)(implicit T: Struct[T]) {
    loadToArray[T](0L, array, start, count)
    this += T.size * count
  }
  
  def writeArray[T](array: Array[T], start: Int, count: Int)(implicit T: Struct[T]) {
    storeArray[T](0L, array, start, count)
    this += T.size * count
  }
}
