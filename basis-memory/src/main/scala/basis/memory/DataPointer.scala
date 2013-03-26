/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.memory

import basis.util._

/** An untyped pointer into a [[Data]] object.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  */
final class DataPointer(val data: Data, startAddress: Long) extends Pointer {
  def this(data: Data) = this(data, 0L)
  
  private[this] var _address: Long = 0L max startAddress min size
  
  private[this] var _limit: Long = size
  
  override def align(alignment: Long) {
    address = basis.memory.align(address, alignment)
  }
  
  override def address: Long = _address
  
  override def address_= (newAddress: Long) {
    _address = 0L max newAddress min size
  }
  
  override def limit: Long = _limit
  
  override def limit_= (newLimit: Long) {
    _limit = address max newLimit min size
  }
  
  override def size: Long = data.size
  
  override def endian: Endianness = data.endian
  
  override def readByte(): Byte = {
    val value = data.loadByte(address)
    address += 1L
    value
  }
  
  override def writeByte(value: Byte) {
    data.storeByte(address, value)
    address += 1L
  }
  
  override def readShort(): Short = {
    val value = data.loadShort(address)
    address += 2L
    value
  }
  
  override def writeShort(value: Short) {
    data.storeShort(address, value)
    address += 2L
  }
  
  override def readInt(): Int = {
    val value = data.loadInt(address)
    address += 4L
    value
  }
  
  override def writeInt(value: Int) {
    data.storeInt(address, value)
    address += 4L
  }
  
  override def readLong(): Long = {
    val value = data.loadLong(address)
    address += 8L
    value
  }
  
  override def writeLong(value: Long) {
    data.storeLong(address, value)
    address += 8L
  }
  
  override def readFloat(): Float = {
    val value = data.loadFloat(address)
    address += 4L
    value
  }
  
  override def writeFloat(value: Float) {
    data.storeFloat(address, value)
    address += 4L
  }
  
  override def readDouble(): Double = {
    val value = data.loadDouble(address)
    address += 8L
    value
  }
  
  override def writeDouble(value: Double) {
    data.storeDouble(address, value)
    address += 8L
  }
  
  override def readUnalignedShort(): Short = {
    val value = data.loadUnalignedShort(address)
    address += 2L
    value
  }
  
  override def writeUnalignedShort(value: Short) {
    data.storeUnalignedShort(address, value)
    address += 2L
  }
  
  override def readUnalignedInt(): Int = {
    val value = data.loadUnalignedInt(address)
    address += 4L
    value
  }
  
  override def writeUnalignedInt(value: Int) {
    data.storeUnalignedInt(address, value)
    address += 4L
  }
  
  override def readUnalignedLong(): Long = {
    val value = data.loadUnalignedLong(address)
    address += 8L
    value
  }
  
  override def writeUnalignedLong(value: Long) {
    data.storeUnalignedLong(address, value)
    address += 8L
  }
  
  override def readUnalignedFloat(): Float = {
    val value = data.loadUnalignedFloat(address)
    address += 4L
    value
  }
  
  override def writeUnalignedFloat(value: Float) {
    data.storeUnalignedFloat(address, value)
    address += 4L
  }
  
  override def readUnalignedDouble(): Double = {
    val value = data.loadUnalignedDouble(address)
    address += 8L
    value
  }
  
  override def writeUnalignedDouble(value: Double) {
    data.storeUnalignedDouble(address, value)
    address += 8L
  }
}
