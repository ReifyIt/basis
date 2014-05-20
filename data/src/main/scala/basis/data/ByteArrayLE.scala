//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis._
import basis.util._

final class ByteArrayLE(val __ : Array[Byte]) extends AnyVal with ByteArray with ByteOrder[LittleEndian] {
  override def endian: LittleEndian = LittleEndian

  override def size: Long = __.length.toLong

  override def loadByte(address: Long): Byte = {
    val offset = address.toInt
    __(offset)
  }

  override def storeByte(address: Long, value: Byte): Unit = {
    val offset = address.toInt
    __(offset) = value
  }

  override def loadShort(address: Long): Short = {
    val offset = address.toInt
    ((__(offset    ) & 0xFF)     ) |
    ((__(offset + 1)       ) << 8)
  }.toShort

  override def storeShort(address: Long, value: Short): Unit = {
    val offset = address.toInt
    __(offset    ) = (value     ).toByte
    __(offset + 1) = (value >> 8).toByte
  }

  override def loadInt(address: Long): Int = {
    val offset = address.toInt
    ((__(offset    ) & 0xFF)      ) |
    ((__(offset + 1) & 0xFF) <<  8) |
    ((__(offset + 2) & 0xFF) << 16) |
    ((__(offset + 3)       ) << 24)
  }

  override def storeInt(address: Long, value: Int): Unit = {
    val offset = address.toInt
    __(offset    ) = (value      ).toByte
    __(offset + 1) = (value >>  8).toByte
    __(offset + 2) = (value >> 16).toByte
    __(offset + 3) = (value >> 24).toByte
  }

  override def loadLong(address: Long): Long = {
    val offset = address.toInt
    ((__(offset    ) & 0xFF).toLong      ) |
    ((__(offset + 1) & 0xFF).toLong <<  8) |
    ((__(offset + 2) & 0xFF).toLong << 16) |
    ((__(offset + 3) & 0xFF).toLong << 24) |
    ((__(offset + 4) & 0xFF).toLong << 32) |
    ((__(offset + 5) & 0xFF).toLong << 40) |
    ((__(offset + 6) & 0xFF).toLong << 48) |
    ((__(offset + 7)       ).toLong << 56)
  }

  override def storeLong(address: Long, value: Long): Unit = {
    val offset = address.toInt
    __(offset    ) = (value      ).toByte
    __(offset + 1) = (value >>  8).toByte
    __(offset + 2) = (value >> 16).toByte
    __(offset + 3) = (value >> 24).toByte
    __(offset + 4) = (value >> 32).toByte
    __(offset + 5) = (value >> 40).toByte
    __(offset + 6) = (value >> 48).toByte
    __(offset + 7) = (value >> 56).toByte
  }

  override def loadFloat(address: Long): Float   = loadInt(address).toFloatBits
  override def loadDouble(address: Long): Double = loadLong(address).toDoubleBits

  override def storeFloat(address: Long, value: Float): Unit   = storeInt(address, value.toRawIntBits)
  override def storeDouble(address: Long, value: Double): Unit = storeLong(address, value.toRawLongBits)

  override def loadAlignedShort(address: Long): Short   = loadShort(address & -2L)
  override def loadAlignedInt(address: Long): Int       = loadInt(address & -4L)
  override def loadAlignedLong(address: Long): Long     = loadLong(address & -8L)
  override def loadAlignedFloat(address: Long): Float   = loadAlignedInt(address).toFloatBits
  override def loadAlignedDouble(address: Long): Double = loadAlignedLong(address).toDoubleBits

  override def storeAlignedShort(address: Long, value: Short): Unit   = storeShort(address & -2L, value)
  override def storeAlignedInt(address: Long, value: Int): Unit       = storeInt(address & -4L, value)
  override def storeAlignedLong(address: Long, value: Long): Unit     = storeLong(address & -8L, value)
  override def storeAlignedFloat(address: Long, value: Float): Unit   = storeFloat(address & -4L, value)
  override def storeAlignedDouble(address: Long, value: Double): Unit = storeDouble(address & -8L, value)

  override def ++ (that: Loader): ByteArrayLE = that match {
    case that: ByteArray =>
      val data = that.toArray
      val buffer = new Array[Byte](__.length + data.length)
      java.lang.System.arraycopy(__, 0, buffer, 0, __.length)
      java.lang.System.arraycopy(data, 0, buffer, __.length, data.length)
      new ByteArrayLE(buffer)
    case _ =>
      val framer = ByteArrayLE.Framer.expect(size + that.size)
      framer.writeData(this)
      framer.writeData(that)
      framer.state
  }

  override def reader(address: Long): Reader with ByteOrder[LittleEndian] = new ByteArrayLEReader(__, address.toInt)

  override def toArray: Array[Byte] = __

  protected override def stringPrefix: String = "ByteArrayLE"
}

object ByteArrayLE extends ByteOrder[LittleEndian] with Allocator[ByteArrayLE] {
  override def endian: LittleEndian = LittleEndian

  override val empty: ByteArrayLE = new ByteArrayLE(new Array[Byte](0))

  override def apply(data: Array[Byte]): ByteArrayLE = new ByteArrayLE(data)

  override def apply(size: Long): ByteArrayLE = new ByteArrayLE(new Array[Byte](size.toInt))

  implicit override def Framer: Framer with ByteOrder[LittleEndian] with State[ByteArrayLE] = new ByteArrayLEFramer

  override def toString: String = "ByteArrayLE"
}

private[data] final class ByteArrayLEReader(buffer: Array[Byte], private[this] var offset: Int) extends ByteOrder[LittleEndian] with Reader {
  override def endian: LittleEndian = LittleEndian

  override def readByte(): Byte = {
    val value = buffer(offset)
    offset += 1
    value
  }

  override def readShort(): Short = {
    val value =
      ((buffer(offset    ) & 0xFF)     ) |
      ((buffer(offset + 1)       ) << 8)
    offset += 2
    value.toShort
  }

  override def readInt(): Int = {
    val value =
      ((buffer(offset    ) & 0xFF)      ) |
      ((buffer(offset + 1) & 0xFF) <<  8) |
      ((buffer(offset + 2) & 0xFF) << 16) |
      ((buffer(offset + 3)       ) << 24)
    offset += 4
    value
  }

  override def readLong(): Long = {
    val value =
      ((buffer(offset    ) & 0xFF).toLong      ) |
      ((buffer(offset + 1) & 0xFF).toLong <<  8) |
      ((buffer(offset + 2) & 0xFF).toLong << 16) |
      ((buffer(offset + 3) & 0xFF).toLong << 24) |
      ((buffer(offset + 4) & 0xFF).toLong << 32) |
      ((buffer(offset + 5) & 0xFF).toLong << 40) |
      ((buffer(offset + 6) & 0xFF).toLong << 48) |
      ((buffer(offset + 7)       ).toLong << 56)
    offset += 8
    value
  }

  override def readFloat(): Float = readInt().toFloatBits

  override def readDouble(): Double = readLong().toDoubleBits
}

private[data] final class ByteArrayLEFramer extends State[ByteArrayLE] with ByteOrder[LittleEndian] with Framer {
  private[this] var buffer: Array[Byte] = _

  private[this] var offset: Int = 0

  private[this] var aliased: Boolean = true

  private[this] def expand(base: Int, size: Int): Int = {
    var n = (base max size) - 1
    n |= n >> 1; n |= n >> 2; n |= n >> 4; n |= n >> 8; n |= n >> 16
    n + 1
  }

  private[this] def prepare(size: Int): Unit = {
    if (aliased || size > buffer.length) {
      val array = new Array[Byte](expand(256, size))
      if (buffer != null) java.lang.System.arraycopy(buffer, 0, array, 0, offset)
      buffer = array
      aliased = false
    }
  }

  override def endian: LittleEndian = LittleEndian

  override def writeByte(value: Byte): Unit = {
    prepare(offset + 1)
    buffer(offset) = value
    offset += 1
  }

  override def writeShort(value: Short): Unit = {
    prepare(offset + 2)
    buffer(offset    ) = (value     ).toByte
    buffer(offset + 1) = (value >> 8).toByte
    offset += 2
  }

  override def writeInt(value: Int): Unit = {
    prepare(offset + 4)
    buffer(offset    ) = (value      ).toByte
    buffer(offset + 1) = (value >>  8).toByte
    buffer(offset + 2) = (value >> 16).toByte
    buffer(offset + 3) = (value >> 24).toByte
    offset += 4
  }

  override def writeLong(value: Long): Unit = {
    prepare(offset + 8)
    buffer(offset    ) = (value      ).toByte
    buffer(offset + 1) = (value >>  8).toByte
    buffer(offset + 2) = (value >> 16).toByte
    buffer(offset + 3) = (value >> 24).toByte
    buffer(offset + 4) = (value >> 32).toByte
    buffer(offset + 5) = (value >> 40).toByte
    buffer(offset + 6) = (value >> 48).toByte
    buffer(offset + 7) = (value >> 56).toByte
    offset += 8
  }

  override def writeFloat(value: Float): Unit = writeInt(value.toRawIntBits)

  override def writeDouble(value: Double): Unit = writeLong(value.toRawLongBits)

  override def writeData(data: Loader): Unit = data match {
    case data: ByteArray if offset == 0 =>
      buffer = data.toArray
      offset = buffer.length
      aliased = true
    case data: ByteArray =>
      val array = data.toArray
      prepare(offset + array.length)
      java.lang.System.arraycopy(array, 0, buffer, offset, array.length)
      offset += array.length
    case _ => super.writeData(data)
  }

  override def expect(count: Long): this.type = {
    if (buffer == null) {
      buffer = new Array[Byte](count.toInt)
      aliased = false
    }
    else if (offset + count.toInt > buffer.length) {
      val array = new Array[Byte](offset + count.toInt)
      java.lang.System.arraycopy(buffer, 0, array, 0, offset)
      buffer = array
      aliased = false
    }
    this
  }

  override def state: ByteArrayLE = {
    if (buffer == null) buffer = ByteArrayLE.empty.__
    else if (buffer.length != offset) {
      val array = new Array[Byte](offset)
      java.lang.System.arraycopy(buffer, 0, array, 0, offset)
      buffer = array
    }
    aliased = true
    new ByteArrayLE(buffer)
  }

  override def clear(): Unit = {
    buffer = null
    offset = 0
    aliased = true
  }

  override def toString: String = "ByteArrayLE"+"."+"Framer"+"()"
}
