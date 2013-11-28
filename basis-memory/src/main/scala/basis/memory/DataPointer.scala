//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

private[memory] final class DataPointer(val data: Data, var address: Long) extends Pointer {
  def this(data: Data) = this(data, 0L)

  override def endian: Endianness = data.endian

  override def += (offset: Long): Unit = address += offset

  override def -= (offset: Long): Unit = address -= offset

  override def align(alignment: Long): Unit =
    address = basis.memory.align(address, alignment)

  override def truncate(alignment: Long): Unit =
    address &= -alignment

  override def canLoad(offset: Long): Boolean =
    0L <= address + offset && address + offset < data.size

  override def canStore(offset: Long): Boolean =
    0L <= address + offset && address + offset < data.size

  override def loadByte(offset: Long): Byte =
    data.loadByte(address + offset)

  override def storeByte(offset: Long, value: Byte): Unit =
    data.storeByte(address + offset, value)

  override def readByte(): Byte = {
    val value = data.loadByte(address)
    address += 1L
    value
  }

  override def writeByte(value: Byte): Unit = {
    data.storeByte(address, value)
    address += 1L
  }

  override def loadShort(offset: Long): Short =
    data.loadShort(address + offset)

  override def storeShort(offset: Long, value: Short): Unit =
    data.storeShort(address + offset, value)

  override def readShort(): Short = {
    val value = data.loadShort(address)
    address += 2L
    value
  }

  override def writeShort(value: Short): Unit = {
    data.storeShort(address, value)
    address += 2L
  }

  override def loadInt(offset: Long): Int =
    data.loadInt(address + offset)

  override def storeInt(offset: Long, value: Int): Unit =
    data.storeInt(address + offset, value)

  override def readInt(): Int = {
    val value = data.loadInt(address)
    address += 4L
    value
  }

  override def writeInt(value: Int): Unit = {
    data.storeInt(address, value)
    address += 4L
  }

  override def loadLong(offset: Long): Long =
    data.loadLong(address + offset)

  override def storeLong(offset: Long, value: Long): Unit =
    data.storeLong(address + offset, value)

  override def readLong(): Long = {
    val value = data.loadLong(address)
    address += 8L
    value
  }

  override def writeLong(value: Long): Unit = {
    data.storeLong(address, value)
    address += 8L
  }

  override def loadFloat(offset: Long): Float =
    data.loadFloat(address + offset)

  override def storeFloat(offset: Long, value: Float): Unit =
    data.storeFloat(address + offset, value)

  override def readFloat(): Float = {
    val value = data.loadFloat(address)
    address += 4L
    value
  }

  override def writeFloat(value: Float): Unit = {
    data.storeFloat(address, value)
    address += 4L
  }

  override def loadDouble(offset: Long): Double =
    data.loadDouble(address + offset)

  override def storeDouble(offset: Long, value: Double): Unit =
    data.storeDouble(address + offset, value)

  override def readDouble(): Double = {
    val value = data.loadDouble(address)
    address += 8L
    value
  }

  override def writeDouble(value: Double): Unit = {
    data.storeDouble(address, value)
    address += 8L
  }

  override def loadUnalignedShort(offset: Long): Short =
    data.loadUnalignedShort(address + offset)

  override def storeUnalignedShort(offset: Long, value: Short): Unit =
    data.storeUnalignedShort(address + offset, value)

  override def readUnalignedShort(): Short = {
    val value = data.loadUnalignedShort(address)
    address += 2L
    value
  }

  override def writeUnalignedShort(value: Short): Unit = {
    data.storeUnalignedShort(address, value)
    address += 2L
  }

  override def loadUnalignedInt(offset: Long): Int =
    data.loadUnalignedInt(address + offset)

  override def storeUnalignedInt(offset: Long, value: Int): Unit =
    data.storeUnalignedInt(address + offset, value)

  override def readUnalignedInt(): Int = {
    val value = data.loadUnalignedInt(address)
    address += 4L
    value
  }

  override def writeUnalignedInt(value: Int): Unit = {
    data.storeUnalignedInt(address, value)
    address += 4L
  }

  override def loadUnalignedLong(offset: Long): Long =
    data.loadUnalignedLong(address + offset)

  override def storeUnalignedLong(offset: Long, value: Long): Unit =
    data.storeUnalignedLong(address + offset, value)

  override def readUnalignedLong(): Long = {
    val value = data.loadUnalignedLong(address)
    address += 8L
    value
  }

  override def writeUnalignedLong(value: Long): Unit = {
    data.storeUnalignedLong(address, value)
    address += 8L
  }

  override def loadUnalignedFloat(offset: Long): Float =
    data.loadUnalignedFloat(address + offset)

  override def storeUnalignedFloat(offset: Long, value: Float): Unit =
    data.storeUnalignedFloat(address + offset, value)

  override def readUnalignedFloat(): Float = {
    val value = data.loadUnalignedFloat(address)
    address += 4L
    value
  }

  override def writeUnalignedFloat(value: Float): Unit = {
    data.storeUnalignedFloat(address, value)
    address += 4L
  }

  override def loadUnalignedDouble(offset: Long): Double =
    data.loadUnalignedDouble(address + offset)

  override def storeUnalignedDouble(offset: Long, value: Double): Unit =
    data.storeUnalignedDouble(address + offset, value)

  override def readUnalignedDouble(): Double = {
    val value = data.loadUnalignedDouble(address)
    address += 8L
    value
  }

  override def writeUnalignedDouble(value: Double): Unit = {
    data.storeUnalignedDouble(address, value)
    address += 8L
  }

  override def load[T](offset: Long)(implicit T: Struct[T]): T =
    data.load[T](address + offset)

  override def store[T](offset: Long, value: T)(implicit T: Struct[T]): Unit =
    data.store[T](address + offset, value)

  override def read[T](implicit T: Struct[T]): T = {
    val value = data.load[T](address)
    address += T.size
    value
  }

  override def write[T](value: T)(implicit T: Struct[T]): Unit = {
    data.store[T](address, value)
    address += T.size
  }

  override def loadArray[T](offset: Long, count: Int)(implicit T: Struct[T]): Array[T] =
    data.loadArray[T](address + offset, count)

  override def loadToArray[T](offset: Long, array: Array[T], start: Int, count: Int)(implicit T: Struct[T]): Unit =
    data.loadToArray[T](address + offset, array, start, count)

  override def storeArray[T](offset: Long, array: Array[T], start: Int, count: Int)(implicit T: Struct[T]): Unit =
    data.storeArray[T](address + offset, array, start, count)

  override def readArray[T](count: Int)(implicit T: Struct[T]): Array[T] = {
    val array = data.loadArray[T](address, count)
    address += T.size * count
    array
  }

  override def readToArray[T](array: Array[T], start: Int, count: Int)(implicit T: Struct[T]): Unit = {
    data.loadToArray[T](address, array, start, count)
    address += T.size * count
  }

  override def writeArray[T](array: Array[T], start: Int, count: Int)(implicit T: Struct[T]): Unit = {
    data.storeArray[T](address, array, start, count)
    address += T.size * count
  }

  override def dup: Pointer = new DataPointer(data, address)

  override def toString: String = "DataPointer"+"("+ data +", "+ address +")"
}
