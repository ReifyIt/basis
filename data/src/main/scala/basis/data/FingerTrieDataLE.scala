//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis._
import basis.collections._
import basis.util._
import scala.annotation._

final class FingerTrieDataLE private[data] (
    private[data] override val prefix: Array[Byte],
    private[data] override val branch: immutable.FingerTrieSeq[Array[Byte]],
    private[data] override val suffix: Array[Byte],
    override val size: Long)
  extends FingerTrieData with Family[FingerTrieDataLE] with ByteOrder[LittleEndian] {

  override def endian: LittleEndian = LittleEndian

  override def loadByte(address: Long): Byte = {
    val n = address - prefix.length.toLong
    if (n < 0L) prefix(address.toInt)
    else {
      val j = n - (branch.length.toLong << 8)
      if (j < 0L) branch((n >> 8).toInt)(n.toInt & 0xFF)
      else suffix(j.toInt)
    }
  }

  override def loadShort(address: Long): Short = {
    val n = address - prefix.length.toLong
    if (n <= -2L) loadWholeShort(prefix, address.toInt)
    else if ((n.toInt & 0xFF) <= 254) {
      val j = n - (branch.length.toLong << 8)
      if (j < 0L) loadWholeShort(branch((n >> 8).toInt), n.toInt & 0xFF)
      else loadWholeShort(suffix, j.toInt)
    }
    else loadSplitShort(address)
  }

  override def loadInt(address: Long): Int = {
    val n = address - prefix.length.toLong
    if (n <= -4L) loadWholeInt(prefix, address.toInt)
    else if ((n.toInt & 0xFF) <= 252) {
      val j = n - (branch.length.toLong << 8)
      if (j < 0L) loadWholeInt(branch((n >> 8).toInt), n.toInt & 0xFF)
      else loadWholeInt(suffix, j.toInt)
    }
    else loadSplitInt(address)
  }

  override def loadLong(address: Long): Long = {
    val n = address - prefix.length.toLong
    if (n <= -8L) loadWholeLong(prefix, address.toInt)
    else if ((n.toInt & 0xFF) <= 248) {
      val j = n - (branch.length.toLong << 8)
      if (j < 0L) loadWholeLong(branch((n >> 8).toInt), n.toInt & 0xFF)
      else loadWholeLong(suffix, j.toInt)
    }
    else loadSplitLong(address)
  }

  override def loadFloat(address: Long): Float = loadInt(address).toFloatBits

  override def loadDouble(address: Long): Double = loadLong(address).toDoubleBits

  override def loadAlignedShort(address: Long): Short = loadShort(address & -2L)

  override def loadAlignedInt(address: Long): Int = loadInt(address & -4L)

  override def loadAlignedLong(address: Long): Long = loadLong(address & -8L)

  override def loadAlignedFloat(address: Long): Float = loadAlignedInt(address).toFloatBits

  override def loadAlignedDouble(address: Long): Double = loadAlignedLong(address).toDoubleBits

  override def reader(address: Long): Reader with ByteOrder[LittleEndian] = new FingerTrieDataLEReader(this)

  private[this] def loadWholeShort(array: Array[Byte], offset: Int): Short = {
    ((array(offset    ) & 0xFF)     ) |
    ((array(offset + 1)       ) << 8)
  }.toShort

  private[this] def loadSplitShort(address: Long): Short = {
    ((loadByte(address     ) & 0xFF)     ) |
    ((loadByte(address + 1L)       ) << 8)
  }.toShort

  private[this] def loadWholeInt(array: Array[Byte], offset: Int): Int = {
    ((array(offset    ) & 0xFF)      ) |
    ((array(offset + 1) & 0xFF) <<  8) |
    ((array(offset + 2) & 0xFF) << 16) |
    ((array(offset + 3)       ) << 24)
  }

  private[this] def loadSplitInt(address: Long): Int = {
    ((loadByte(address     ) & 0xFF)      ) |
    ((loadByte(address + 1L) & 0xFF) <<  8) |
    ((loadByte(address + 2L) & 0xFF) << 16) |
    ((loadByte(address + 3L)       ) << 24)
  }

  private[this] def loadWholeLong(array: Array[Byte], offset: Int): Long = {
    ((array(offset    ) & 0xFF).toLong      ) |
    ((array(offset + 1) & 0xFF).toLong <<  8) |
    ((array(offset + 2) & 0xFF).toLong << 16) |
    ((array(offset + 3) & 0xFF).toLong << 24) |
    ((array(offset + 4) & 0xFF).toLong << 32) |
    ((array(offset + 5) & 0xFF).toLong << 40) |
    ((array(offset + 6) & 0xFF).toLong << 48) |
    ((array(offset + 7)       ).toLong << 56)
  }

  private[this] def loadSplitLong(address: Long): Long = {
    ((loadByte(address     ) & 0xFF).toLong      ) |
    ((loadByte(address + 1L) & 0xFF).toLong <<  8) |
    ((loadByte(address + 2L) & 0xFF).toLong << 16) |
    ((loadByte(address + 3L) & 0xFF).toLong << 24) |
    ((loadByte(address + 4L) & 0xFF).toLong << 32) |
    ((loadByte(address + 5L) & 0xFF).toLong << 40) |
    ((loadByte(address + 6L) & 0xFF).toLong << 48) |
    ((loadByte(address + 7L)       ).toLong << 56)
  }

  override def drop(lower: Long): FingerTrieDataLE = {
    val n = lower - prefix.length.toLong
    val k = size - lower
    if (lower <= 0L) this
    else if (lower >= size) FingerTrieDataLE.empty
    else if (n == 0L) {
      if (branch.length > 0) new FingerTrieDataLE(branch.head, branch.tail, suffix, k)
      else new FingerTrieDataLE(suffix, immutable.FingerTrieSeq.empty, FingerTrieData.EmptyByteArray, k)
    }
    else if (n < 0L) {
      val newPrefix = new Array[Byte](-n.toInt)
      System.arraycopy(prefix, lower.toInt, newPrefix, 0, -n.toInt)
      new FingerTrieDataLE(newPrefix, branch, suffix, k)
    }
    else {
      val j = n - (branch.length.toLong << 8)
      if (j < 0L) {
        val split = branch.drop((n >> 8).toInt)
        val splitPrefix = split.head
        val newPrefix = new Array[Byte](splitPrefix.length - (n.toInt & 0xFF))
        System.arraycopy(splitPrefix, n.toInt & 0xFF, newPrefix, 0, newPrefix.length)
        new FingerTrieDataLE(newPrefix, split.tail, suffix, k)
      }
      else {
        val newPrefix = new Array[Byte](k.toInt)
        System.arraycopy(suffix, j.toInt, newPrefix, 0, k.toInt)
        new FingerTrieDataLE(newPrefix, immutable.FingerTrieSeq.empty, FingerTrieData.EmptyByteArray, k)
      }
    }
  }

  override def take(upper: Long): FingerTrieDataLE = {
    val n = upper - prefix.length.toLong
    if (upper <= 0L) FingerTrieDataLE.empty
    else if (upper >= size) this
    else if (n == 0L) new FingerTrieDataLE(prefix, immutable.FingerTrieSeq.empty, FingerTrieData.EmptyByteArray, upper)
    else if (n < 0L) {
      val newPrefix = new Array[Byte](upper.toInt)
      System.arraycopy(prefix, 0, newPrefix, 0, upper.toInt)
      new FingerTrieDataLE(newPrefix, immutable.FingerTrieSeq.empty, FingerTrieData.EmptyByteArray, upper)
    }
    else {
      val j = n - (branch.length.toLong << 8)
      if (j == 0L) {
        if (branch.length > 0) new FingerTrieDataLE(prefix, branch.body, branch.foot, upper)
        else new FingerTrieDataLE(suffix, immutable.FingerTrieSeq.empty, FingerTrieData.EmptyByteArray, upper)
      }
      else if (j < 0L) {
        val split = branch.take((((n + 0xFFL) & ~0xFFL) >> 8).toInt)
        val splitSuffix = split.foot
        val newSuffix = new Array[Byte](((((n.toInt & 0xFF) ^ 0xFF) + 1) & 0x100) | (n.toInt & 0xFF))
        System.arraycopy(splitSuffix, 0, newSuffix, 0, newSuffix.length)
        new FingerTrieDataLE(prefix, split.body, newSuffix, upper)
      }
      else {
        val newSuffix = new Array[Byte](j.toInt)
        System.arraycopy(suffix, 0, newSuffix, 0, j.toInt)
        new FingerTrieDataLE(prefix, branch, newSuffix, upper)
      }
    }
  }

  override def slice(lower: Long, upper: Long): FingerTrieDataLE = {
    if (lower >= upper) FingerTrieDataLE.empty
    else drop(lower).take(upper - (0L max lower))
  }

  protected override def stringPrefix: String = "FingerTrieDataLE"
}

object FingerTrieDataLE extends ByteOrder[LittleEndian] with DataFactory[FingerTrieDataLE] {
  override def endian: LittleEndian = LittleEndian

  override val empty: FingerTrieDataLE =
    new FingerTrieDataLE(FingerTrieData.EmptyByteArray, immutable.FingerTrieSeq.empty, FingerTrieData.EmptyByteArray, 0L)

  implicit override def Framer: Framer with ByteOrder[LittleEndian] with State[FingerTrieDataLE] = new FingerTrieDataLEFramer

  override def toString: String = "FingerTrieDataLE"
}

private[data] final class FingerTrieDataLEReader(
    private[this] val segmenter: Iterator[Array[Byte]],
    private[this] var buffer: Array[Byte],
    private[this] var index: Int)
  extends ByteOrder[LittleEndian] with Reader {

  def this(data: FingerTrieDataLE) = {
    this(new FingerTrieDataArraySegmenter(data), null, 0)
    if (!segmenter.isEmpty) {
      buffer = segmenter.head
      segmenter.step()
    }
    else buffer = FingerTrieData.EmptyByteArray
  }

  override def endian: LittleEndian = LittleEndian

  override def isEOF: Boolean = index >= buffer.length && segmenter.isEmpty

  override def readByte(): Byte = {
    val value = buffer(index)
    step(1)
    value
  }

  override def readShort(): Short = {
    if (index <= buffer.length - 2) {
      val value =
        ((buffer(index    ) & 0xFF)     ) |
        ((buffer(index + 1)       ) << 8)
      step(2)
      value
    }
    else
      ((readByte() & 0xFF)     ) |
      ((readByte()       ) << 8)
  }.toShort

  override def readInt(): Int = {
    if (index <= buffer.length - 4) {
      val value =
        ((buffer(index    ) & 0xFF)      ) |
        ((buffer(index + 1) & 0xFF) <<  8) |
        ((buffer(index + 2) & 0xFF) << 16) |
        ((buffer(index + 3)       ) << 24)
      step(4)
      value
    }
    else
      ((readByte() & 0xFF)      ) |
      ((readByte() & 0xFF) <<  8) |
      ((readByte() & 0xFF) << 16) |
      ((readByte()       ) << 24)
  }

  override def readLong(): Long = {
    if (index <= buffer.length - 8) {
      val value =
        ((buffer(index    ) & 0xFF).toLong      ) |
        ((buffer(index + 1) & 0xFF).toLong <<  8) |
        ((buffer(index + 2) & 0xFF).toLong << 16) |
        ((buffer(index + 3) & 0xFF).toLong << 24) |
        ((buffer(index + 4) & 0xFF).toLong << 32) |
        ((buffer(index + 5) & 0xFF).toLong << 40) |
        ((buffer(index + 6) & 0xFF).toLong << 48) |
        ((buffer(index + 7)       ).toLong << 56)
      step(8)
      value
    }
    else
      ((readByte() & 0xFF).toLong      ) |
      ((readByte() & 0xFF).toLong <<  8) |
      ((readByte() & 0xFF).toLong << 16) |
      ((readByte() & 0xFF).toLong << 24) |
      ((readByte() & 0xFF).toLong << 32) |
      ((readByte() & 0xFF).toLong << 40) |
      ((readByte() & 0xFF).toLong << 48) |
      ((readByte()       ).toLong << 56)
  }

  override def readFloat(): Float = readInt().toFloatBits

  override def readDouble(): Double = readLong().toDoubleBits

  @tailrec override def drop(lower: Long): Reader with ByteOrder[LittleEndian] = {
    val more = lower - (buffer.length.toLong - index.toLong)
    if (more < 0L) {
      index += lower.toInt
      this
    }
    else if (!segmenter.isEmpty) {
      buffer = segmenter.head
      segmenter.step()
      index = 0
      drop(more)
    }
    else {
      buffer = FingerTrieData.EmptyByteArray
      index = 0
      this
    }
  }

  private[this] def step(count: Int): Unit = {
    index += count
    if (index >= buffer.length) {
      if (!segmenter.isEmpty) {
        buffer = segmenter.head
        segmenter.step()
      }
      else buffer = FingerTrieData.EmptyByteArray
      index = 0
    }
  }
}

private[data] final class FingerTrieDataLEFramer(
    private[this] var prefix: Array[Byte],
    private[this] var branch: Builder[Array[Byte]] with State[immutable.FingerTrieSeq[Array[Byte]]],
    private[this] var buffer: Array[Byte],
    private[this] var length: Long)
  extends State[FingerTrieDataLE] with ByteOrder[LittleEndian] with Framer {

  def this(that: FingerTrieData) =
    this(if (that.size > 256L) that.prefix else null,
         if (that.size > 512L) immutable.FingerTrieSeq.Builder[Array[Byte]] ++= that.branch else null,
         if (that.size > 256L) that.suffix else if (that.size > 0L) that.prefix else null,
         that.size)

  def this() = this(null, null, null, 0L)

  private[this] def skew: Int = (if (prefix != null) length - prefix.length.toLong else length).toInt & 0xFF

  private[this] def prepare(offset: Int): Unit = {
    if (offset == 0) {
      if (buffer != null) {
        if (prefix == null) prefix = buffer
        else {
          if (branch == null) branch = immutable.FingerTrieSeq.Builder[Array[Byte]]
          branch.append(buffer)
        }
      }
      buffer = new Array[Byte](256)
    }
    else if (buffer.length < 256) {
      val newBuffer = new Array[Byte](256)
      System.arraycopy(buffer, 0, newBuffer, 0, offset)
      buffer = newBuffer
    }
  }

  override def endian: LittleEndian = LittleEndian

  override def isEOF: Boolean = length >= Long.MaxValue

  override def writeByte(value: Byte): Unit = {
    val offset = skew
    prepare(offset)
    buffer(offset) = value
    length += 1L
  }

  override def writeShort(value: Short): Unit = {
    val offset = skew
    if (offset <= 254) {
      prepare(offset)
      buffer(offset    ) = (value     ).toByte
      buffer(offset + 1) = (value >> 8).toByte
      length += 2L
    }
    else {
      writeByte((value     ).toByte)
      writeByte((value >> 8).toByte)
    }
  }

  override def writeInt(value: Int): Unit = {
    val offset = skew
    if (offset <= 252) {
      prepare(offset)
      buffer(offset    ) = (value      ).toByte
      buffer(offset + 1) = (value >>  8).toByte
      buffer(offset + 2) = (value >> 16).toByte
      buffer(offset + 3) = (value >> 24).toByte
      length += 4L
    }
    else {
      writeByte((value      ).toByte)
      writeByte((value >>  8).toByte)
      writeByte((value >> 16).toByte)
      writeByte((value >> 24).toByte)
    }
  }

  override def writeLong(value: Long): Unit = {
    val offset = skew
    if (offset <= 248) {
      prepare(offset)
      buffer(offset    ) = (value      ).toByte
      buffer(offset + 1) = (value >>  8).toByte
      buffer(offset + 2) = (value >> 16).toByte
      buffer(offset + 3) = (value >> 24).toByte
      buffer(offset + 4) = (value >> 32).toByte
      buffer(offset + 5) = (value >> 40).toByte
      buffer(offset + 6) = (value >> 48).toByte
      buffer(offset + 7) = (value >> 56).toByte
      length += 8L
    }
    else {
      writeByte((value      ).toByte)
      writeByte((value >>  8).toByte)
      writeByte((value >> 16).toByte)
      writeByte((value >> 24).toByte)
      writeByte((value >> 32).toByte)
      writeByte((value >> 40).toByte)
      writeByte((value >> 48).toByte)
      writeByte((value >> 56).toByte)
    }
  }

  override def writeFloat(value: Float): Unit = writeInt(value.toRawIntBits)

  override def writeDouble(value: Double): Unit = writeLong(value.toRawLongBits)

  override def writeData(data: Loader): Unit = data match {
    case _ if data.size == 0 => ()
    case that: FingerTrieData =>
      val offset = skew
      if (length == 0L) {
        if (that.size > 256L) {
          prefix = that.prefix
          if (that.size > 512L) branch = immutable.FingerTrieSeq.Builder[Array[Byte]] ++= that.branch
          buffer = that.suffix
        }
        else buffer = that.prefix
        length = that.size
      }
      else if (((offset + that.prefix.length) & 0xFF) == 0) {
        if (buffer.length < 256) {
          val newBuffer = new Array[Byte](256)
          System.arraycopy(buffer, 0, newBuffer, 0, offset)
          buffer = newBuffer
        }
        if (offset > 0) System.arraycopy(that.prefix, 0, buffer, offset, 256 - offset)
        else {
          if (prefix == null) prefix = buffer
          else {
            if (branch == null) branch = immutable.FingerTrieSeq.Builder[Array[Byte]]
            branch.append(buffer)
          }
          buffer = that.prefix
        }
        if (that.suffix.length > 0) {
          if (branch == null) branch = immutable.FingerTrieSeq.Builder[Array[Byte]]
          branch.append(buffer)
          branch.appendAll(that.branch)
          buffer = that.suffix
        }
        length += that.size
      }
      else super.writeData(data)
    case _ => super.writeData(data)
  }

  override def state: FingerTrieDataLE = {
    if (length == 0) FingerTrieDataLE.empty
    else {
      val offset = skew
      if (offset != 0 && offset != buffer.length) {
        val suffix = new Array[Byte](offset)
        System.arraycopy(buffer, 0, suffix, 0, offset)
        buffer = suffix
      }
      if (prefix == null) new FingerTrieDataLE(buffer, immutable.FingerTrieSeq.empty, FingerTrieData.EmptyByteArray, length)
      else if (branch == null) new FingerTrieDataLE(prefix, immutable.FingerTrieSeq.empty, buffer, length)
      else new FingerTrieDataLE(prefix, branch.state, buffer, length)
    }
  }

  override def clear(): Unit = {
    prefix = null
    branch = null
    buffer = null
    length = 0L
  }

  override def toString: String = "FingerTrieDataLE"+"."+"Framer"
}
