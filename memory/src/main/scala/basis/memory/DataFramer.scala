//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

import basis.collections._
import basis.util._

private[memory] final class DataFramer(Alloc: Allocator) extends Framer with State[Data] {
  private[this] var data: Data = _

  private[this] var address: Long = 0L

  private[this] var length: Long = 0L

  private[this] var aliased: Boolean = true

  private[this] def expand(base: Long, size: Long): Long = {
    var n = (base max size) - 1L
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16; n |= n >> 32
    n + 1L
  }

  private[this] def prepare(size: Long): Unit = {
    if (aliased || size > data.size) {
      val newSize = expand(128L, size)
      data = if (data != null) data.copy(newSize) else Alloc(newSize)
      aliased = false
    }
    if (size > length) length = size
  }

  override def size: Long = length

  override def endian: Endianness = Alloc.Endian

  override def += (offset: Long): Unit = address += offset

  override def -= (offset: Long): Unit = address -= offset

  override def align(alignment: Long): Unit =
    address = basis.memory.align(address, alignment)

  override def truncate(alignment: Long): Unit =
    address &= -alignment

  override def canStore(offset: Long): Boolean =
    0L <= address + offset

  override def storeByte(offset: Long, value: Byte): Unit = {
    prepare(address + offset + 1L)
    data.storeByte(address + offset, value)
  }

  override def writeByte(value: Byte): Unit = {
    prepare(address + 1L)
    data.storeByte(address, value)
    address += 1L
  }

  override def storeShort(offset: Long, value: Short): Unit = {
    prepare(address + offset + 2L)
    data.storeShort(address + offset, value)
  }

  override def writeShort(value: Short): Unit = {
    prepare(address + 2L)
    data.storeShort(address, value)
    address += 2L
  }

  override def storeInt(offset: Long, value: Int): Unit = {
    prepare(address + offset + 4L)
    data.storeInt(address + offset, value)
  }

  override def writeInt(value: Int): Unit = {
    prepare(address + 4L)
    data.storeInt(address, value)
    address += 4L
  }

  override def storeLong(offset: Long, value: Long): Unit = {
    prepare(address + offset + 8L)
    data.storeLong(address + offset, value)
  }

  override def writeLong(value: Long): Unit = {
    prepare(address + 8L)
    data.storeLong(address, value)
    address += 8L
  }

  override def storeFloat(offset: Long, value: Float): Unit = {
    prepare(address + offset + 4L)
    data.storeFloat(address + offset, value)
  }

  override def writeFloat(value: Float): Unit = {
    prepare(address + 4L)
    data.storeFloat(address, value)
    address += 4L
  }

  override def storeDouble(offset: Long, value: Double): Unit = {
    prepare(address + offset + 8L)
    data.storeDouble(address + offset, value)
  }

  override def writeDouble(value: Double): Unit = {
    prepare(address + 8L)
    data.storeDouble(address, value)
    address += 8L
  }

  override def storeUnalignedShort(offset: Long, value: Short): Unit = {
    prepare(address + offset + 2L)
    data.storeUnalignedShort(address + offset, value)
  }

  override def writeUnalignedShort(value: Short): Unit = {
    prepare(address + 2L)
    data.storeUnalignedShort(address, value)
    address += 2L
  }

  override def storeUnalignedInt(offset: Long, value: Int): Unit = {
    prepare(address + offset + 4L)
    data.storeUnalignedInt(address + offset, value)
  }

  override def writeUnalignedInt(value: Int): Unit = {
    prepare(address + 4L)
    data.storeUnalignedInt(address, value)
    address += 4L
  }

  override def storeUnalignedLong(offset: Long, value: Long): Unit = {
    prepare(address + offset + 8L)
    data.storeUnalignedLong(address + offset, value)
  }

  override def writeUnalignedLong(value: Long): Unit = {
    prepare(address + 8L)
    data.storeUnalignedLong(address, value)
    address += 8L
  }

  override def storeUnalignedFloat(offset: Long, value: Float): Unit = {
    prepare(address + offset + 4L)
    data.storeUnalignedFloat(address + offset, value)
  }

  override def writeUnalignedFloat(value: Float): Unit = {
    prepare(address + 4L)
    data.storeUnalignedFloat(address, value)
    address += 4L
  }

  override def storeUnalignedDouble(offset: Long, value: Double): Unit = {
    prepare(address + offset + 8L)
    data.storeUnalignedDouble(address + offset, value)
  }

  override def writeUnalignedDouble(value: Double): Unit = {
    prepare(address + 8L)
    data.storeUnalignedDouble(address, value)
    address += 8L
  }

  override def store[T](offset: Long, value: T)(implicit T: Struct[T]): Unit = {
    prepare(address + offset + T.size)
    data.store[T](address + offset, value)
  }

  override def write[T](value: T)(implicit T: Struct[T]): Unit = {
    prepare(address + T.size)
    data.store[T](address, value)
    address += T.size
  }

  override def storeArray[T](offset: Long, array: Array[T], start: Int, count: Int)(implicit T: Struct[T]): Unit = {
    prepare(address + offset + T.size * count)
    data.storeArray[T](address + offset, array, start, count)
  }

  override def writeArray[T](array: Array[T], start: Int, count: Int)(implicit T: Struct[T]): Unit = {
    prepare(address + T.size * count)
    data.storeArray[T](address, array, start, count)
    address += T.size * count
  }

  override def expect(count: Long): this.type = {
    if (data == null) {
      data = Alloc(length + count)
      aliased = false
    }
    else if (length + count > data.size) {
      data = data.copy(length + count)
      aliased = false
    }
    this
  }

  override def state: Data = {
    if (data == null) data = Alloc(0L)
    else if (length != data.size) data = data.copy(length)
    aliased = true
    data
  }

  override def clear(): Unit = {
    data = null
    aliased = true
    address = 0L
    length = 0L
  }

  override def toString: String = Alloc.toString +"."+"Buffer"+"()"
}
