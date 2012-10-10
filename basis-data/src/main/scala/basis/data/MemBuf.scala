/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

import scala._

/** `ByteBuffer` backed memory. */
class MemBuf(val buffer: java.nio.ByteBuffer) extends AnyVal with Mem {
  @inline override def size: Long = buffer.capacity.toLong
  
  override def unit: Int = 1
  
  override def endian: ByteOrder = {
    if (buffer.order eq java.nio.ByteOrder.BIG_ENDIAN) BigEndian
    else if (buffer.order eq java.nio.ByteOrder.LITTLE_ENDIAN) LittleEndian
    else throw new scala.MatchError(buffer.order)
  }
  
  override def copy(size: Long = this.size): MemBuf = {
    scala.Predef.require(0 <= size && size <= scala.Int.MaxValue.toLong)
    val dst = java.nio.ByteBuffer.allocateDirect(size.toInt)
    val src = buffer.duplicate
    src.position(0).limit(scala.math.min(src.capacity, dst.capacity))
    dst.put(src)
    dst.clear()
    new MemBuf(dst)
  }
  
  @inline override def loadByte(address: Long): Byte =
    buffer.get(address.toInt)
  
  @inline override def storeByte(address: Long, value: Byte): Unit =
    buffer.put(address.toInt, value)
  
  @inline override def loadShort(address: Long): Short =
    buffer.getShort(address.toInt & -2)
  
  @inline override def storeShort(address: Long, value: Short): Unit =
    buffer.putShort(address.toInt & -2, value)
  
  @inline override def loadInt(address: Long): Int =
    buffer.getInt(address.toInt & -4)
  
  @inline override def storeInt(address: Long, value: Int): Unit =
    buffer.putInt(address.toInt & -4, value)
  
  @inline override def loadLong(address: Long): Long =
    buffer.getLong(address.toInt & -8)
  
  @inline override def storeLong(address: Long, value: Long): Unit =
    buffer.putLong(address.toInt & -8, value)
  
  @inline override def loadFloat(address: Long): Float =
    buffer.getFloat(address.toInt & -4)
  
  @inline override def storeFloat(address: Long, value: Float): Unit =
    buffer.putFloat(address.toInt & -4, value)
  
  @inline override def loadDouble(address: Long): Double =
    buffer.getDouble(address.toInt & -8)
  
  @inline override def storeDouble(address: Long, value: Double): Unit =
    buffer.putDouble(address.toInt & -8, value)
  
  @inline override def loadUnalignedShort(address: Long): Short =
    buffer.getShort(address.toInt)
  
  @inline override def storeUnalignedShort(address: Long, value: Short): Unit =
    buffer.putShort(address.toInt, value)
  
  @inline override def loadUnalignedInt(address: Long): Int =
    buffer.getInt(address.toInt)
  
  @inline override def storeUnalignedInt(address: Long, value: Int): Unit =
    buffer.putInt(address.toInt, value)
  
  @inline override def loadUnalignedLong(address: Long): Long =
    buffer.getLong(address.toInt)
  
  @inline override def storeUnalignedLong(address: Long, value: Long): Unit =
    buffer.putLong(address.toInt, value)
  
  @inline override def loadUnalignedFloat(address: Long): Float =
    buffer.getFloat(address.toInt)
  
  @inline override def storeUnalignedFloat(address: Long, value: Float): Unit =
    buffer.putFloat(address.toInt, value)
  
  @inline override def loadUnalignedDouble(address: Long): Double =
    buffer.getDouble(address.toInt)
  
  @inline override def storeUnalignedDouble(address: Long, value: Double): Unit =
    buffer.putDouble(address.toInt, value)
  
  override def move(fromAddress: Long, toAddress: Long, size: Long) {
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
  
  override def clear(fromAddress: Long, untilAddress: Long) {
    var i = fromAddress.toInt
    val until = untilAddress.toInt
    while (i < until) {
      buffer.put(i, 0.toByte)
      i += 1
    }
  }
  
  @inline def toBE: MemBuf = {
    if (buffer.order eq java.nio.ByteOrder.BIG_ENDIAN) this
    else new MemBuf(buffer.duplicate.order(java.nio.ByteOrder.BIG_ENDIAN))
  }
  
  @inline def toLE: MemBuf = {
    if (buffer.order eq java.nio.ByteOrder.LITTLE_ENDIAN) this
    else new MemBuf(buffer.duplicate.order(java.nio.ByteOrder.LITTLE_ENDIAN))
  }
  
  override def toString: java.lang.String = "MemBuf"+"("+ size +")"
}

/** An allocator for native-endian memory backed by a `ByteBuffer`. */
object MemBuf extends Allocator with (Long => MemBuf) {
  override def MaxSize: Long = scala.Int.MaxValue.toLong
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): MemBuf =
    apply(unit.size * count)
  
  override def apply(size: Long): MemBuf = {
    scala.Predef.require(0L <= size && size <= MaxSize)
    new MemBuf(java.nio.ByteBuffer.allocateDirect(size.toInt))
  }
  
  def unapply(mem: MemBuf): Some[java.nio.ByteBuffer] = Some(mem.buffer)
  
  override def toString: java.lang.String = "MemBuf"
}
