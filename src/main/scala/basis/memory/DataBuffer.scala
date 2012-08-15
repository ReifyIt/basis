/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** `ByteBuffer` backed Data. */
final class DataBuffer(val buffer: java.nio.ByteBuffer) extends AnyVal with Data {
  @inline final override def size: Long = buffer.capacity.toLong
  
  final override def unit: Int = 1
  
  final override def endian: ByteOrder = {
    if (buffer.order eq java.nio.ByteOrder.BIG_ENDIAN) BigEndian
    else if (buffer.order eq java.nio.ByteOrder.LITTLE_ENDIAN) LittleEndian
    else throw new MatchError(buffer.order)
  }
  
  final override def copy(size: Long = this.size): DataBuffer = {
    require(0 <= size && size <= Int.MaxValue.toLong)
    val dst = java.nio.ByteBuffer.allocateDirect(size.toInt)
    val src = buffer.duplicate
    src.position(0).limit(math.min(src.capacity, dst.capacity))
    dst.put(src)
    dst.clear()
    new DataBuffer(dst)
  }
  
  @inline final override def loadByte(address: Long): Byte =
    buffer.get(address.toInt)
  
  @inline final override def storeByte(address: Long, value: Byte): Unit =
    buffer.put(address.toInt, value)
  
  @inline final override def loadShort(address: Long): Short =
    buffer.getShort(address.toInt & -2)
  
  @inline final override def storeShort(address: Long, value: Short): Unit =
    buffer.putShort(address.toInt & -2, value)
  
  @inline final override def loadInt(address: Long): Int =
    buffer.getInt(address.toInt & -4)
  
  @inline final override def storeInt(address: Long, value: Int): Unit =
    buffer.putInt(address.toInt & -4, value)
  
  @inline final override def loadLong(address: Long): Long =
    buffer.getLong(address.toInt & -8)
  
  @inline final override def storeLong(address: Long, value: Long): Unit =
    buffer.putLong(address.toInt & -8, value)
  
  @inline final override def loadChar(address: Long): Char =
    buffer.getChar(address.toInt & -2)
  
  @inline final override def storeChar(address: Long, value: Char): Unit =
    buffer.putChar(address.toInt & -2, value)
  
  @inline final override def loadFloat(address: Long): Float =
    buffer.getFloat(address.toInt & -4)
  
  @inline final override def storeFloat(address: Long, value: Float): Unit =
    buffer.putFloat(address.toInt & -4, value)
  
  @inline final override def loadDouble(address: Long): Double =
    buffer.getDouble(address.toInt & -8)
  
  @inline final override def storeDouble(address: Long, value: Double): Unit =
    buffer.putDouble(address.toInt & -8, value)
  
  @inline final override def loadUnalignedShort(address: Long): Short =
    buffer.getShort(address.toInt)
  
  @inline final override def storeUnalignedShort(address: Long, value: Short): Unit =
    buffer.putShort(address.toInt, value)
  
  @inline final override def loadUnalignedInt(address: Long): Int =
    buffer.getInt(address.toInt)
  
  @inline final override def storeUnalignedInt(address: Long, value: Int): Unit =
    buffer.putInt(address.toInt, value)
  
  @inline final override def loadUnalignedLong(address: Long): Long =
    buffer.getLong(address.toInt)
  
  @inline final override def storeUnalignedLong(address: Long, value: Long): Unit =
    buffer.putLong(address.toInt, value)
  
  @inline final override def loadUnalignedChar(address: Long): Char =
    buffer.getChar(address.toInt)
  
  @inline final override def storeUnalignedChar(address: Long, value: Char): Unit =
    buffer.putChar(address.toInt, value)
  
  @inline final override def loadUnalignedFloat(address: Long): Float =
    buffer.getFloat(address.toInt)
  
  @inline final override def storeUnalignedFloat(address: Long, value: Float): Unit =
    buffer.putFloat(address.toInt, value)
  
  @inline final override def loadUnalignedDouble(address: Long): Double =
    buffer.getDouble(address.toInt)
  
  @inline final override def storeUnalignedDouble(address: Long, value: Double): Unit =
    buffer.putDouble(address.toInt, value)
  
  final override def move(fromAddress: Long, toAddress: Long, size: Long) {
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
  
  final override def clear(fromAddress: Long, untilAddress: Long) {
    var i = fromAddress.toInt
    val until = untilAddress.toInt
    while (i < until) {
      buffer.put(i, 0.toByte)
      i += 1
    }
  }
  
  @inline final def toBE: DataBuffer = {
    if (buffer.order eq java.nio.ByteOrder.BIG_ENDIAN) this
    else new DataBuffer(buffer.duplicate.order(java.nio.ByteOrder.BIG_ENDIAN))
  }
  
  @inline final def toLE: DataBuffer = {
    if (buffer.order eq java.nio.ByteOrder.LITTLE_ENDIAN) this
    else new DataBuffer(buffer.duplicate.order(java.nio.ByteOrder.LITTLE_ENDIAN))
  }
  
  override def toString: String = "DataBuffer"+"("+ size +")"
}

/** An allocator for native-endian data backed by a `ByteBuffer`. */
object DataBuffer extends Allocator with (Long => DataBuffer) {
  override def MaxSize: Long = Int.MaxValue.toLong
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): DataBuffer =
    apply(unit.size * count)
  
  override def apply(size: Long): DataBuffer = {
    require(0L <= size && size <= MaxSize)
    new DataBuffer(java.nio.ByteBuffer.allocateDirect(size.toInt))
  }
  
  def unapply(data: DataBuffer): Some[java.nio.ByteBuffer] = Some(data.buffer)
  
  override def toString: String = "DataBuffer"
}
