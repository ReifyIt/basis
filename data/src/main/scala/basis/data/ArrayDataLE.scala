//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis._
import basis.util._

final class ArrayDataLE(val __ : Array[Byte]) extends AnyVal with Family[ArrayDataLE] with ByteOrder[LittleEndian] with ArrayData {
  override def endian: LittleEndian = LittleEndian

  override def as[E <: Endianness](endian: E): ArrayData with ByteOrder[E] = {
    if (endian.isLittle) this
    else if (endian.isBig) new ArrayDataBE(toArray)
    else throw new MatchError(endian)
  }.asInstanceOf[ArrayData with ByteOrder[E]]

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

  override def drop(lower: Long): ArrayDataLE = {
    val n = __.length
    val i = (0L max lower).toInt min n
    val buffer = new Array[Byte](n - i)
    java.lang.System.arraycopy(__, i, buffer, 0, n - i)
    new ArrayDataLE(buffer)
  }

  override def take(upper: Long): ArrayDataLE = {
    val n = (0L max upper).toInt min __.length
    val buffer = new Array[Byte](n)
    java.lang.System.arraycopy(__, 0, buffer, 0, n)
    new ArrayDataLE(buffer)
  }

  override def slice(lower: Long, upper: Long): ArrayDataLE = {
    val n = (0L max upper).toInt min __.length
    val i = (0L max lower).toInt min n
    val buffer = new Array[Byte](n - i)
    java.lang.System.arraycopy(__, i, buffer, 0, n - i)
    new ArrayDataLE(buffer)
  }

  override def reader(address: Long): Reader with ByteOrder[LittleEndian] = new ArrayDataLEReader(__, address.toInt)

  override def toArray: Array[Byte] = __

  protected override def stringPrefix: String = "ArrayDataLE"
}

object ArrayDataLE extends ByteOrder[LittleEndian] with Allocator[ArrayDataLE] {
  override def endian: LittleEndian = LittleEndian

  override val empty: ArrayDataLE = new ArrayDataLE(new Array[Byte](0))

  override def apply(size: Long): ArrayDataLE = new ArrayDataLE(new Array[Byte](size.toInt))

  override def apply(data: Array[Byte]): ArrayDataLE = new ArrayDataLE(data)

  implicit override def Framer: Framer with ByteOrder[LittleEndian] with State[ArrayDataLE] = new ArrayDataLEFramer

  override def toString: String = "ArrayDataLE"
}

private[data] final class ArrayDataLEReader(buffer: Array[Byte], private[this] var offset: Int) extends ByteOrder[LittleEndian] with Reader {
  override def endian: LittleEndian = LittleEndian

  override def isEOF: Boolean = offset >= buffer.length

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

  override def drop(lower: Long): this.type = {
    offset = ((offset.toLong + lower) min buffer.length.toLong).toInt
    this
  }
}

private[data] final class ArrayDataLEFramer extends State[ArrayDataLE] with ByteOrder[LittleEndian] with Framer {
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

  override def isEOF: Boolean = offset >= Int.MaxValue

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
    case data: ArrayData if offset == 0 =>
      buffer = data.toArray
      offset = buffer.length
      aliased = true
    case data: ArrayData =>
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

  override def state: ArrayDataLE = {
    if (buffer == null) buffer = ArrayDataLE.empty.__
    else if (buffer.length != offset) {
      val array = new Array[Byte](offset)
      java.lang.System.arraycopy(buffer, 0, array, 0, offset)
      buffer = array
    }
    aliased = true
    new ArrayDataLE(buffer)
  }

  override def clear(): Unit = {
    buffer = null
    offset = 0
    aliased = true
  }

  override def toString: String = "ArrayDataLE"+"."+"Framer"
}
