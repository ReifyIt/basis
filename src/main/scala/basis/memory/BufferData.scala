/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import java.nio.ByteBuffer
import java.nio.ByteOrder

/** `ByteBuffer` backed Data. */
final class BufferData(val buffer: ByteBuffer) extends AnyVal with Data {
  @inline override def size: Long = buffer.capacity.toLong
  
  @inline override def unit: Int = 1
  
  override def endian: Endianness = buffer.order match {
    case ByteOrder.BIG_ENDIAN => Endianness.BigEndian
    case ByteOrder.LITTLE_ENDIAN => Endianness.LittleEndian
  }
  
  def copy(size: Long): BufferData = {
    require(0 <= size && size <= Int.MaxValue.toLong)
    val dst = ByteBuffer.allocateDirect(size.toInt)
    val src = buffer.duplicate
    src.position(0).limit(math.min(src.capacity, dst.capacity))
    dst.put(src)
    dst.clear()
    new BufferData(dst)
  }
  
  @inline override def loadByte(address: Long): Byte =
    buffer.get(address.toInt)
  
  @inline override def storeByte(address: Long, value: Byte): Unit =
    buffer.put(address.toInt, value)
  
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
  
  @inline override def loadUnalignedChar(address: Long): Char =
    buffer.getChar(address.toInt)
  
  @inline override def storeUnalignedChar(address: Long, value: Char): Unit =
    buffer.putChar(address.toInt, value)
  
  @inline override def loadUnalignedFloat(address: Long): Float =
    buffer.getFloat(address.toInt)
  
  @inline override def storeUnalignedFloat(address: Long, value: Float): Unit =
    buffer.putFloat(address.toInt, value)
  
  @inline override def loadUnalignedDouble(address: Long): Double =
    buffer.getDouble(address.toInt)
  
  @inline override def storeUnalignedDouble(address: Long, value: Double): Unit =
    buffer.putDouble(address.toInt, value)
  
  override def toString: String = endian match {
    case Endianness.BigEndian => "BufferDataBE"+"("+ size +")"
    case Endianness.LittleEndian => "BufferDataLE"+"("+ size +")"
  }
}

/** An allocator for big-endian data backed by a `ByteBuffer`. */
object BufferDataBE extends Allocator {
  override def MaxSize: Long = Int.MaxValue.toLong
  
  override def alloc[T](count: Long)(implicit unit: Struct[T]): BufferData =
    apply(unit.size * count)
  
  override def apply(size: Long): BufferData = {
    require(0L <= size && size <= MaxSize)
    val buffer = ByteBuffer.allocateDirect(size.toInt).order(ByteOrder.BIG_ENDIAN)
    new BufferData(buffer)
  }
  
  def unapply(data: BufferData): Option[ByteBuffer] = data.endian match {
    case Endianness.BigEndian => Some(data.buffer)
    case _ => None
  }
  
  override def toString: String = "BufferDataBE"
}

/** An allocator for little-endian data backed by a `ByteBuffer`. */
object BufferDataLE extends Allocator {
  override def MaxSize: Long = Int.MaxValue.toLong
  
  override def alloc[T](count: Long)(implicit unit: Struct[T]): BufferData =
    apply(unit.size * count)
  
  override def apply(size: Long): BufferData = {
    require(0L <= size && size <= MaxSize)
    val buffer = ByteBuffer.allocateDirect(size.toInt).order(ByteOrder.LITTLE_ENDIAN)
    new BufferData(buffer)
  }
  
  def unapply(data: BufferData): Option[ByteBuffer] = data.endian match {
    case Endianness.LittleEndian => Some(data.buffer)
    case _ => None
  }
  
  override def toString: String = "BufferDataLE"
}
