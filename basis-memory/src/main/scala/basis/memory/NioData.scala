//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

import basis.util._

/** `ByteBuffer` backed data.
  *
  * @author Chris Sachs
  * @since  0.0
  */
private[memory] final class NioData(val buffer: java.nio.ByteBuffer) extends Data {
  override def size: Long = buffer.capacity.toLong

  override def unit: Int = 1

  override def endian: Endianness = {
    if (buffer.order eq java.nio.ByteOrder.BIG_ENDIAN) BigEndian
    else if (buffer.order eq java.nio.ByteOrder.LITTLE_ENDIAN) LittleEndian
    else throw new MatchError(buffer.order)
  }

  override def copy(size: Long): NioData = {
    if (size < 0L || size > NioData.MaxSize) throw new DataSizeException(size.toString)
    val dst = java.nio.ByteBuffer.allocateDirect(size.toInt)
    val src = buffer.duplicate
    src.position(0).limit(src.capacity min dst.capacity)
    dst.put(src)
    dst.clear()
    new NioData(dst)
  }

  override def loadByte(address: Long): Byte =
    buffer.get(address.toInt)

  override def storeByte(address: Long, value: Byte): Unit =
    buffer.put(address.toInt, value)

  override def loadShort(address: Long): Short =
    buffer.getShort(address.toInt & -2)

  override def storeShort(address: Long, value: Short): Unit =
    buffer.putShort(address.toInt & -2, value)

  override def loadInt(address: Long): Int =
    buffer.getInt(address.toInt & -4)

  override def storeInt(address: Long, value: Int): Unit =
    buffer.putInt(address.toInt & -4, value)

  override def loadLong(address: Long): Long =
    buffer.getLong(address.toInt & -8)

  override def storeLong(address: Long, value: Long): Unit =
    buffer.putLong(address.toInt & -8, value)

  override def loadFloat(address: Long): Float =
    buffer.getFloat(address.toInt & -4)

  override def storeFloat(address: Long, value: Float): Unit =
    buffer.putFloat(address.toInt & -4, value)

  override def loadDouble(address: Long): Double =
    buffer.getDouble(address.toInt & -8)

  override def storeDouble(address: Long, value: Double): Unit =
    buffer.putDouble(address.toInt & -8, value)

  override def loadUnalignedShort(address: Long): Short =
    buffer.getShort(address.toInt)

  override def storeUnalignedShort(address: Long, value: Short): Unit =
    buffer.putShort(address.toInt, value)

  override def loadUnalignedInt(address: Long): Int =
    buffer.getInt(address.toInt)

  override def storeUnalignedInt(address: Long, value: Int): Unit =
    buffer.putInt(address.toInt, value)

  override def loadUnalignedLong(address: Long): Long =
    buffer.getLong(address.toInt)

  override def storeUnalignedLong(address: Long, value: Long): Unit =
    buffer.putLong(address.toInt, value)

  override def loadUnalignedFloat(address: Long): Float =
    buffer.getFloat(address.toInt)

  override def storeUnalignedFloat(address: Long, value: Float): Unit =
    buffer.putFloat(address.toInt, value)

  override def loadUnalignedDouble(address: Long): Double =
    buffer.getDouble(address.toInt)

  override def storeUnalignedDouble(address: Long, value: Double): Unit =
    buffer.putDouble(address.toInt, value)

  override def move(fromAddress: Long, toAddress: Long, size: Long): Unit = {
    val fromLimit = (fromAddress + size).toInt
    val toLimit = (toAddress + size).toInt
    if (fromAddress == toAddress) ()
    else if (fromAddress >= toAddress || fromLimit <= toAddress) {
      var p = fromAddress.toInt
      var q = toAddress.toInt
      while (q < toLimit) {
        buffer.put(q, buffer.get(p))
        p += 1
        q += 1
      }
    }
    else {
      var p = fromLimit - 1
      var q = toLimit - 1
      while (q >= toAddress) {
        buffer.put(q, buffer.get(p))
        p -= 1
        q -= 1
      }
    }
  }

  override def clear(fromAddress: Long, untilAddress: Long): Unit = {
    var i = fromAddress.toInt
    val until = untilAddress.toInt
    while (i < until) {
      buffer.put(i, 0.toByte)
      i += 1
    }
  }

  override def toString: String = "NioData"+"("+ size +")"
}

/** An allocator for native-endian data backed by a `ByteBuffer`. */
private[memory] object NioData extends Allocator with (Long => NioData) {
  override def MaxSize: Long = Int.MaxValue.toLong

  override def Endian: Endianness = NativeEndian

  override def alloc[T](count: Long)(implicit T: Struct[T]): NioData = apply(T.size * count)

  override def apply(size: Long): NioData = {
    if (size < 0L || size > MaxSize) throw new DataSizeException(size.toString)
    new NioData(java.nio.ByteBuffer.allocateDirect(size.toInt).order(
      if (NativeEndian eq BigEndian) java.nio.ByteOrder.BIG_ENDIAN
      else java.nio.ByteOrder.LITTLE_ENDIAN
    ))
  }

  override def toString: String = "NioData"
}
