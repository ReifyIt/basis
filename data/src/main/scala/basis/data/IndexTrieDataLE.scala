//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis._
import basis.util._

sealed abstract class IndexTrieDataLE extends IndexTrieData with ByteOrder[LittleEndian] {
  override def endian: LittleEndian = LittleEndian

  override def loadByte(address: Long): Byte

  override def loadShort(address: Long): Short = {
    val offset = address.toInt & 0xFF
    if (offset <= 254) {
      val node1 = getNode1(address)
      ((node1(offset    ) & 0xFF)     ) |
      ((node1(offset + 1)       ) << 8)
    }
    else
      ((loadByte(address     ) & 0xFF)     ) |
      ((loadByte(address + 1L)       ) << 8)
  }.toShort

  override def loadInt(address: Long): Int = {
    val offset = address.toInt & 0xFF
    if (offset <= 252) {
      val node1 = getNode1(address)
      ((node1(offset    ) & 0xFF)      ) |
      ((node1(offset + 1) & 0xFF) <<  8) |
      ((node1(offset + 2) & 0xFF) << 16) |
      ((node1(offset + 3)       ) << 24)
    }
    else
      ((loadByte(address     ) & 0xFF)      ) |
      ((loadByte(address + 1L) & 0xFF) <<  8) |
      ((loadByte(address + 2L) & 0xFF) << 16) |
      ((loadByte(address + 3L)       ) << 24)
  }

  override def loadLong(address: Long): Long = {
    val offset = address.toInt & 0xFF
    if (offset <= 248) {
      val node1 = getNode1(address)
      ((node1(offset    ) & 0xFF).toLong      ) |
      ((node1(offset + 1) & 0xFF).toLong <<  8) |
      ((node1(offset + 2) & 0xFF).toLong << 16) |
      ((node1(offset + 3) & 0xFF).toLong << 24) |
      ((node1(offset + 4) & 0xFF).toLong << 32) |
      ((node1(offset + 5) & 0xFF).toLong << 40) |
      ((node1(offset + 6) & 0xFF).toLong << 48) |
      ((node1(offset + 7)       ).toLong << 56)
    }
    else
      ((loadByte(address     ) & 0xFF).toLong      ) |
      ((loadByte(address + 1L) & 0xFF).toLong <<  8) |
      ((loadByte(address + 2L) & 0xFF).toLong << 16) |
      ((loadByte(address + 3L) & 0xFF).toLong << 24) |
      ((loadByte(address + 4L) & 0xFF).toLong << 32) |
      ((loadByte(address + 5L) & 0xFF).toLong << 40) |
      ((loadByte(address + 6L) & 0xFF).toLong << 48) |
      ((loadByte(address + 7L)       ).toLong << 56)
  }

  override def loadFloat(address: Long): Float = loadInt(address).toFloatBits

  override def loadDouble(address: Long): Double = loadLong(address).toDoubleBits

  override def loadAlignedShort(address: Long): Short = {
    val offset = address.toInt & 0xFE
    val node1 = getNode1(address)
    ((node1(offset    ) & 0xFF)     ) |
    ((node1(offset + 1)       ) << 8)
  }.toShort

  override def loadAlignedInt(address: Long): Int = {
    val offset = address.toInt & 0xFC
    val node1 = getNode1(address)
    ((node1(offset    ) & 0xFF)      ) |
    ((node1(offset + 1) & 0xFF) <<  8) |
    ((node1(offset + 2) & 0xFF) << 16) |
    ((node1(offset + 3)       ) << 24)
  }

  override def loadAlignedLong(address: Long): Long = {
    val offset = address.toInt & 0xF8
    val node1 = getNode1(address)
    ((node1(offset    ) & 0xFF).toLong      ) |
    ((node1(offset + 1) & 0xFF).toLong <<  8) |
    ((node1(offset + 2) & 0xFF).toLong << 16) |
    ((node1(offset + 3) & 0xFF).toLong << 24) |
    ((node1(offset + 4) & 0xFF).toLong << 32) |
    ((node1(offset + 5) & 0xFF).toLong << 40) |
    ((node1(offset + 6) & 0xFF).toLong << 48) |
    ((node1(offset + 7)       ).toLong << 56)
  }

  override def loadAlignedFloat(address: Long): Float = loadAlignedInt(address).toFloatBits

  override def loadAlignedDouble(address: Long): Double = loadAlignedLong(address).toDoubleBits

  override def mutateByte(address: Long, value: Byte): IndexTrieDataLE = {
    val offset = address.toInt & 0xFF
    val node1 = copyNode1(address)
    node1(offset) = value
    mutateNode1(address, node1)
  }

  override def mutateShort(address: Long, value: Short): IndexTrieDataLE = {
    val offset = address.toInt & 0xFF
    if (offset <= 254) {
      val node1 = copyNode1(address)
      node1(offset    ) = (value     ).toByte
      node1(offset + 1) = (value >> 8).toByte
      mutateNode1(address, node1)
    }
    else
      mutateByte(address     , (value     ).toByte).
      mutateByte(address + 1L, (value >> 8).toByte)
  }

  override def mutateInt(address: Long, value: Int): IndexTrieDataLE = {
    val offset = address.toInt & 0xFF
    if (offset <= 252) {
      val node1 = copyNode1(address)
      node1(offset    ) = (value      ).toByte
      node1(offset + 1) = (value >>  8).toByte
      node1(offset + 2) = (value >> 16).toByte
      node1(offset + 3) = (value >> 24).toByte
      mutateNode1(address, node1)
    }
    else
      mutateByte(address     , (value      ).toByte).
      mutateByte(address + 1L, (value >>  8).toByte).
      mutateByte(address + 2L, (value >> 16).toByte).
      mutateByte(address + 3L, (value >> 24).toByte)
  }

  override def mutateLong(address: Long, value: Long): IndexTrieDataLE = {
    val offset = address.toInt & 0xFF
    if (offset <= 248) {
      val node1 = copyNode1(address)
      node1(offset    ) = (value      ).toByte
      node1(offset + 1) = (value >>  8).toByte
      node1(offset + 2) = (value >> 16).toByte
      node1(offset + 3) = (value >> 24).toByte
      node1(offset + 4) = (value >> 32).toByte
      node1(offset + 5) = (value >> 40).toByte
      node1(offset + 6) = (value >> 48).toByte
      node1(offset + 7) = (value >> 56).toByte
      mutateNode1(address, node1)
    }
    else
      mutateByte(address     , (value      ).toByte).
      mutateByte(address + 1L, (value >>  8).toByte).
      mutateByte(address + 2L, (value >> 16).toByte).
      mutateByte(address + 3L, (value >> 24).toByte).
      mutateByte(address + 4L, (value >> 32).toByte).
      mutateByte(address + 5L, (value >> 40).toByte).
      mutateByte(address + 6L, (value >> 48).toByte).
      mutateByte(address + 7L, (value >> 56).toByte)
  }

  override def mutateFloat(address: Long, value: Float): IndexTrieDataLE = mutateInt(address, value.toRawIntBits)

  override def mutateDouble(address: Long, value: Double): IndexTrieDataLE = mutateLong(address, value.toRawLongBits)

  override def ++ (that: Loader): IndexTrieDataLE = {
    val framer = IndexTrieDataLE.Framer
    framer.writeData(this)
    framer.writeData(that)
    framer.state
  }

  protected[data] def getNode1(address: Long): Array[Byte]

  protected[data] def copyNode1(address: Long): Array[Byte] = {
    val node1 = getNode1(address)
    val newNode1 = new Array[Byte](node1.length)
    java.lang.System.arraycopy(node1, 0, newNode1, 0, node1.length)
    newNode1
  }

  protected[data] def mutateNode1(address: Long, newNode1: Array[Byte]): IndexTrieDataLE

  protected override def stringPrefix: String = "IndexTrieDataLE"
}

object IndexTrieDataLE extends ByteOrder[LittleEndian] with DataFactory[IndexTrieDataLE] {
  override def endian: LittleEndian = LittleEndian

  override val empty: IndexTrieDataLE = new IndexTrieDataLE0

  implicit override def Framer: Framer with ByteOrder[LittleEndian] with State[IndexTrieDataLE] = new IndexTrieDataLEFramer

  override def toString: String = "IndexTrieDataLE"
}

private[data] final class IndexTrieDataLE0 extends IndexTrieDataLE {
  override def size: Long = 0L

  override def reader(address: Long): Reader with ByteOrder[LittleEndian] = new IndexTrieDataLEReader()

  override def loadByte(address: Long): Byte = throw new IndexOutOfBoundsException(address.toString)

  protected[data] override def getNode1(address: Long): Array[Byte] = throw new IndexOutOfBoundsException(address.toString)

  protected[data] override def mutateNode1(address: Long, newNode1: Array[Byte]): IndexTrieDataLE = throw new IndexOutOfBoundsException(address.toString)
}

private[data] final class IndexTrieDataLE1(
    private[data] val node1: Array[Byte],
    override val size: Long)
  extends IndexTrieDataLE {

  override def reader(address: Long): Reader with ByteOrder[LittleEndian] = new IndexTrieDataLEReader(node1, size)

  override def loadByte(address: Long): Byte = node1(address.toInt)

  protected[data] override def getNode1(address: Long): Array[Byte] = node1

  protected[data] override def mutateNode1(address: Long, newNode1: Array[Byte]): IndexTrieDataLE = new IndexTrieDataLE1(newNode1, size)
}

private[data] final class IndexTrieDataLE2(
    private[data] val node2: Array[Array[Byte]],
    override val size: Long)
  extends IndexTrieDataLE {

  override def reader(address: Long): Reader with ByteOrder[LittleEndian] = new IndexTrieDataLEReader(node2, size)

  override def loadByte(address: Long): Byte = {
    val lo = address.toInt
    (node2(lo >>> 8 & 0xFF)
          (lo       & 0xFF))
  }

  protected[data] override def getNode1(address: Long): Array[Byte] = {
    val lo = address.toInt
    node2(lo >>> 8 & 0xFF)
  }

  protected[data] override def mutateNode1(address: Long, newNode1: Array[Byte]): IndexTrieDataLE = {
    val lo = address.toInt
    val newNode2 = new Array[Array[Byte]](node2.length)
    java.lang.System.arraycopy(node2, 0, newNode2, 0, node2.length)

    newNode2(lo >>> 8 & 0xFF) = newNode1
    new IndexTrieDataLE2(newNode2, size)
  }
}

private[data] final class IndexTrieDataLE3(
    private[data] val node3: Array[Array[Array[Byte]]],
    override val size: Long)
  extends IndexTrieDataLE {

  override def reader(address: Long): Reader with ByteOrder[LittleEndian] = new IndexTrieDataLEReader(node3, size)

  override def loadByte(address: Long): Byte = {
    val lo = address.toInt
    (node3(lo >>> 16 & 0xFF)
          (lo >>>  8 & 0xFF)
          (lo        & 0xFF))
  }

  protected[data] override def getNode1(address: Long): Array[Byte] = {
    val lo = address.toInt
    (node3(lo >>> 16 & 0xFF)
          (lo >>>  8 & 0xFF))
  }

  protected[data] override def mutateNode1(address: Long, newNode1: Array[Byte]): IndexTrieDataLE = {
    val lo = address.toInt
    val newNode3 = new Array[Array[Array[Byte]]](node3.length)
    java.lang.System.arraycopy(node3, 0, newNode3, 0, node3.length)

    val node2 = newNode3(lo >>> 16 & 0xFF)
    val newNode2 = new Array[Array[Byte]](node2.length)
    newNode3(lo >>> 16 & 0xFF) = newNode2
    java.lang.System.arraycopy(node2, 0, newNode2, 0, node2.length)

    newNode2(lo >>>  8 & 0xFF) = newNode1
    new IndexTrieDataLE3(newNode3, size)
  }
}

private[data] final class IndexTrieDataLE4(
    private[data] val node4: Array[Array[Array[Array[Byte]]]],
    override val size: Long)
  extends IndexTrieDataLE {

  override def reader(address: Long): Reader with ByteOrder[LittleEndian] = new IndexTrieDataLEReader(node4, size)

  override def loadByte(address: Long): Byte = {
    val lo = address.toInt
    (node4(lo >>> 24 & 0xFF)
          (lo >>> 16 & 0xFF)
          (lo >>>  8 & 0xFF)
          (lo        & 0xFF))
  }

  protected[data] override def getNode1(address: Long): Array[Byte] = {
    val lo = address.toInt
    (node4(lo >>> 24 & 0xFF)
          (lo >>> 16 & 0xFF)
          (lo >>>  8 & 0xFF))
  }

  protected[data] override def mutateNode1(address: Long, newNode1: Array[Byte]): IndexTrieDataLE = {
    val lo = address.toInt
    val newNode4 = new Array[Array[Array[Array[Byte]]]](node4.length)
    java.lang.System.arraycopy(node4, 0, newNode4, 0, node4.length)

    val node3 = newNode4(lo >>> 24 & 0xFF)
    val newNode3 = new Array[Array[Array[Byte]]](node3.length)
    newNode4(lo >>> 24 & 0xFF) = newNode3
    java.lang.System.arraycopy(node3, 0, newNode3, 0, node3.length)

    val node2 = newNode3(lo >>> 16 & 0xFF)
    val newNode2 = new Array[Array[Byte]](node2.length)
    newNode3(lo >>> 16 & 0xFF) = newNode2
    java.lang.System.arraycopy(node2, 0, newNode2, 0, node2.length)

    newNode2(lo >>>  8 & 0xFF) = newNode1
    new IndexTrieDataLE4(newNode4, size)
  }
}

private[data] final class IndexTrieDataLE5(
    private[data] val node5: Array[Array[Array[Array[Array[Byte]]]]],
    override val size: Long)
  extends IndexTrieDataLE {

  override def reader(address: Long): Reader with ByteOrder[LittleEndian] = new IndexTrieDataLEReader(node5, size)

  override def loadByte(address: Long): Byte = {
    val hi = (address >>> 32).toInt
    val lo = address.toInt
    (node5(hi        & 0xFF)
          (lo >>> 24 & 0xFF)
          (lo >>> 16 & 0xFF)
          (lo >>>  8 & 0xFF)
          (lo        & 0xFF))
  }

  protected[data] override def getNode1(address: Long): Array[Byte] = {
    val hi = (address >>> 32).toInt
    val lo = address.toInt
    (node5(hi        & 0xFF)
          (lo >>> 24 & 0xFF)
          (lo >>> 16 & 0xFF)
          (lo >>>  8 & 0xFF))
  }

  protected[data] override def mutateNode1(address: Long, newNode1: Array[Byte]): IndexTrieDataLE = {
    val hi = (address >>> 32).toInt
    val lo = address.toInt
    val newNode5 = new Array[Array[Array[Array[Array[Byte]]]]](node5.length)
    java.lang.System.arraycopy(node5, 0, newNode5, 0, node5.length)

    val node4 = newNode5(hi        & 0xFF)
    val newNode4 = new Array[Array[Array[Array[Byte]]]](node4.length)
    newNode5(hi        & 0xFF) = newNode4
    java.lang.System.arraycopy(node4, 0, newNode4, 0, node4.length)

    val node3 = newNode4(lo >>> 24 & 0xFF)
    val newNode3 = new Array[Array[Array[Byte]]](node3.length)
    newNode4(lo >>> 24 & 0xFF) = newNode3
    java.lang.System.arraycopy(node3, 0, newNode3, 0, node3.length)

    val node2 = newNode3(lo >>> 16 & 0xFF)
    val newNode2 = new Array[Array[Byte]](node2.length)
    newNode3(lo >>> 16 & 0xFF) = newNode2
    java.lang.System.arraycopy(node2, 0, newNode2, 0, node2.length)

    newNode2(lo >>>  8 & 0xFF) = newNode1
    new IndexTrieDataLE5(newNode5, size)
  }
}

private[data] final class IndexTrieDataLE6(
    private[data] val node6: Array[Array[Array[Array[Array[Array[Byte]]]]]],
    override val size: Long)
  extends IndexTrieDataLE {

  override def reader(address: Long): Reader with ByteOrder[LittleEndian] = new IndexTrieDataLEReader(node6, size)

  override def loadByte(address: Long): Byte = {
    val hi = (address >>> 32).toInt
    val lo = address.toInt
    (node6(hi >>>  8 & 0xFF)
          (hi        & 0xFF)
          (lo >>> 24 & 0xFF)
          (lo >>> 16 & 0xFF)
          (lo >>>  8 & 0xFF)
          (lo        & 0xFF))
  }

  protected[data] override def getNode1(address: Long): Array[Byte] = {
    val hi = (address >>> 32).toInt
    val lo = address.toInt
    (node6(hi >>>  8 & 0xFF)
          (hi        & 0xFF)
          (lo >>> 24 & 0xFF)
          (lo >>> 16 & 0xFF)
          (lo >>>  8 & 0xFF))
  }

  protected[data] override def mutateNode1(address: Long, newNode1: Array[Byte]): IndexTrieDataLE = {
    val hi = (address >>> 32).toInt
    val lo = address.toInt
    val newNode6 = new Array[Array[Array[Array[Array[Array[Byte]]]]]](node6.length)
    java.lang.System.arraycopy(node6, 0, newNode6, 0, node6.length)

    val node5 = newNode6(hi >>>  8 & 0xFF)
    val newNode5 = new Array[Array[Array[Array[Array[Byte]]]]](node5.length)
    newNode6(hi >>>  8 & 0xFF) = newNode5
    java.lang.System.arraycopy(node5, 0, newNode5, 0, node5.length)

    val node4 = newNode5(hi        & 0xFF)
    val newNode4 = new Array[Array[Array[Array[Byte]]]](node4.length)
    newNode5(hi        & 0xFF) = newNode4
    java.lang.System.arraycopy(node4, 0, newNode4, 0, node4.length)

    val node3 = newNode4(lo >>> 24 & 0xFF)
    val newNode3 = new Array[Array[Array[Byte]]](node3.length)
    newNode4(lo >>> 24 & 0xFF) = newNode3
    java.lang.System.arraycopy(node3, 0, newNode3, 0, node3.length)

    val node2 = newNode3(lo >>> 16 & 0xFF)
    val newNode2 = new Array[Array[Byte]](node2.length)
    newNode3(lo >>> 16 & 0xFF) = newNode2
    java.lang.System.arraycopy(node2, 0, newNode2, 0, node2.length)

    newNode2(lo >>>  8 & 0xFF) = newNode1
    new IndexTrieDataLE6(newNode6, size)
  }
}

private[data] final class IndexTrieDataLEReader(
    private[this] val size: Long,
    private[this] var index: Long,
    private[this] var node1: Array[Byte],
    private[this] var node2: Array[Array[Byte]],
    private[this] var node3: Array[Array[Array[Byte]]],
    private[this] var node4: Array[Array[Array[Array[Byte]]]],
    private[this] var node5: Array[Array[Array[Array[Array[Byte]]]]],
    private[this] var node6: Array[Array[Array[Array[Array[Array[Byte]]]]]])
  extends ByteOrder[LittleEndian] with Reader {

  def this() = this(0L, 0L, null, null, null, null, null, null)

  def this(node1: Array[Byte], size: Long) =
    this(size, 0L, node1, null, null, null, null, null)

  def this(node2: Array[Array[Byte]], size: Long) = {
    this(size, 0L, node2(0), node2, null, null, null, null)
    node1 = node2(0)
  }
 
  def this(node3: Array[Array[Array[Byte]]], size: Long) = {
    this(size, 0L, null, null, node3, null, null, null)
    node2 = node3(0)
    node1 = node2(0)
  }

  def this(node4: Array[Array[Array[Array[Byte]]]], size: Long) = {
    this(size, 0L, null, null, null, node4, null, null)
    node3 = node4(0)
    node2 = node3(0)
    node1 = node2(0)
  }

  def this(node5: Array[Array[Array[Array[Array[Byte]]]]], size: Long) = {
    this(size, 0L, null, null, null, null, node5, null)
    node4 = node5(0)
    node3 = node4(0)
    node2 = node3(0)
    node1 = node2(0)
  }

  def this(node6: Array[Array[Array[Array[Array[Array[Byte]]]]]], size: Long) = {
    this(size, 0L, null, null, null, null, null, node6)
    node5 = node6(0)
    node4 = node5(0)
    node3 = node4(0)
    node2 = node3(0)
    node1 = node2(0)
  }

  override def endian: LittleEndian = LittleEndian

  override def readByte(): Byte = {
    val offset = index.toInt & 0xFF
    val value = node1(offset)
    step(1L)
    value
  }

  override def readShort(): Short = {
    val offset = index.toInt & 0xFF
    if (offset <= 254)  {
      val value =
        ((node1(offset    ) & 0xFF)     ) |
        ((node1(offset + 1)       ) << 8)
      step(2L)
      value
    }
    else
      ((readByte() & 0xFF)     ) |
      ((readByte()       ) << 8)
  }.toShort

  override def readInt(): Int = {
    val offset = index.toInt & 0xFF
    if (offset <= 252) {
      val value =
        ((node1(offset    ) & 0xFF)      ) |
        ((node1(offset + 1) & 0xFF) <<  8) |
        ((node1(offset + 2) & 0xFF) << 16) |
        ((node1(offset + 3)       ) << 24)
      step(4L)
      value
    }
    else
      ((readByte() & 0xFF)      ) |
      ((readByte() & 0xFF) <<  8) |
      ((readByte() & 0xFF) << 16) |
      ((readByte()       ) << 24)
  }

  override def readLong(): Long = {
    val offset = index.toInt & 0xFF
    if (offset <= 248) {
      val value =
        ((node1(offset    ) & 0xFF).toLong      ) |
        ((node1(offset + 1) & 0xFF).toLong <<  8) |
        ((node1(offset + 2) & 0xFF).toLong << 16) |
        ((node1(offset + 3) & 0xFF).toLong << 24) |
        ((node1(offset + 4) & 0xFF).toLong << 32) |
        ((node1(offset + 5) & 0xFF).toLong << 40) |
        ((node1(offset + 6) & 0xFF).toLong << 48) |
        ((node1(offset + 7)       ).toLong << 56)
      step(8L)
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

  private[this] def step(count: Long): Unit = {
    val diff = index ^ (index + count)
    index += count
    if (index < size && diff >= (1L << 8)) {
      if (diff >= (1L << 16)) {
        if (diff >= (1L << 24)) {
          if (diff >= (1L << 32)) {
            if (diff >= (1L << 40)) {
              node5 = node6((index >>> 40).toInt & 0xFF)
            }
            node4 = node5((index >>> 32).toInt & 0xFF)
          }
          node3 = node4((index >>> 24).toInt & 0xFF)
        }
        node2 = node3((index >>> 16).toInt & 0xFF)
      }
      node1 = node2((index >>> 8).toInt & 0xFF)
    }
  }
}

private[data] final class IndexTrieDataLEFramer extends State[IndexTrieDataLE] with ByteOrder[LittleEndian] with Framer {
  private[this] var node1: Array[Byte] = _
  private[this] var node2: Array[Array[Byte]] = _
  private[this] var node3: Array[Array[Array[Byte]]] = _
  private[this] var node4: Array[Array[Array[Array[Byte]]]] = _
  private[this] var node5: Array[Array[Array[Array[Array[Byte]]]]] = _
  private[this] var node6: Array[Array[Array[Array[Array[Array[Byte]]]]]] = _

  private[this] var length: Long = 0

  private[this] var aliased: Int = 0

  private[this] def gotoNode1(): Unit = {
    if (length >= (1L << 8)) gotoNode2()
    if (aliased == 1 || (length & 0x00000000000000FFL) == 0L) {
      val oldNode1 = node1
      node1 = new Array[Byte](256)
      if (aliased == 1) {
        java.lang.System.arraycopy(oldNode1, 0, node1, 0, oldNode1.length)
        aliased = 0
      }
      if (length == (1L << 8)) node2(0) = oldNode1
      if (length >= (1L << 8)) node2((length >>> 8).toInt & 0xFF) = node1
    }
  }

  private[this] def gotoNode2(): Unit = {
    if (length >= (1L << 16)) gotoNode3()
    if (aliased == 2 || (length & 0x000000000000FFFFL) == 0L || length == (1L << 8)) {
      val oldNode2 = node2
      node2 = new Array[Array[Byte]](256)
      if (aliased == 2) {
        java.lang.System.arraycopy(oldNode2, 0, node2, 0, oldNode2.length)
        aliased = 1
      }
      if (length == (1L << 16)) node3(0) = oldNode2
      if (length >= (1L << 16)) node3((length >>> 16).toInt & 0xFF) = node2
    }
  }

  private[this] def gotoNode3(): Unit = {
    if (length >= (1L << 24)) gotoNode4()
    if (aliased == 3 || (length & 0x0000000000FFFFFFL) == 0L || length == (1L << 16)) {
      val oldNode3 = node3
      node3 = new Array[Array[Array[Byte]]](256)
      if (aliased == 3) {
        java.lang.System.arraycopy(oldNode3, 0, node3, 0, oldNode3.length)
        aliased = 2
      }
      if (length == (1L << 24)) node4(0) = oldNode3
      if (length >= (1L << 24)) node4((length >>> 24).toInt & 0xFF) = node3
    }
  }

  private[this] def gotoNode4(): Unit = {
    if (length >= (1L << 32)) gotoNode5()
    if (aliased == 4 || (length & 0x00000000FFFFFFFFL) == 0L || length == (1L << 24)) {
      val oldNode4 = node4
      node4 = new Array[Array[Array[Array[Byte]]]](256)
      if (aliased == 4) {
        java.lang.System.arraycopy(oldNode4, 0, node4, 0, oldNode4.length)
        aliased = 3
      }
      if (length == (1L << 32)) node5(0) = oldNode4
      if (length >= (1L << 32)) node5((length >>> 32).toInt & 0xFF) = node4
    }
  }

  private[this] def gotoNode5(): Unit = {
    if (length >= (1L << 40)) gotoNode6()
    if (aliased == 5 || (length & 0x000000FFFFFFFFFFL) == 0L || length == (1L << 32)) {
      val oldNode5 = node5
      node5 = new Array[Array[Array[Array[Array[Byte]]]]](256)
      if (aliased == 5) {
        java.lang.System.arraycopy(oldNode5, 0, node5, 0, oldNode5.length)
        aliased = 4
      }
      if (length == (1L << 40)) node6(0) = oldNode5
      if (length >= (1L << 40)) node6((length >>> 40).toInt & 0xFF) = node5
    }
  }

  private[this] def gotoNode6(): Unit = {
    if (length >= (1L << 48)) throw new UnsupportedOperationException("maximum length exceeded")
    if (aliased == 6 || length == (1L << 48)) {
      val oldNode6 = node6
      node6 = new Array[Array[Array[Array[Array[Array[Byte]]]]]](256)
      if (aliased == 6) {
        java.lang.System.arraycopy(oldNode6, 0, node6, 0, oldNode6.length)
        aliased = 5
      }
    }
  }

  private[this] def alias: IndexTrieDataLE = if (length == 0L) IndexTrieDataLE.empty else alias1

  private[this] def alias1: IndexTrieDataLE = {
    if ((length.toInt & 0xFF) != 0) {
      val last1 = (length - 1L).toInt & 0xFF
      val oldNode1 = node1
      node1 = new Array[Byte](last1 + 1)
      java.lang.System.arraycopy(oldNode1, 0, node1, 0, last1 + 1)
      aliased = 1
    }
    if (length <= (1L << 8)) new IndexTrieDataLE1(node1, length) else alias2
  }

  private[this] def alias2: IndexTrieDataLE = {
    if (aliased == 1 || ((length >>> 8).toInt & 0xFF) != 0) {
      val last2 = ((length - 1L) >>> 8).toInt & 0xFF
      val oldNode2 = node2
      node2 = new Array[Array[Byte]](last2 + 1)
      java.lang.System.arraycopy(oldNode2, 0, node2, 0, last2)
      node2(last2) = node1
      aliased = 2
    }
    if (length <= (1L << 16)) new IndexTrieDataLE2(node2, length) else alias3
  }

  private[this] def alias3: IndexTrieDataLE = {
    if (aliased == 2 || ((length >>> 16).toInt & 0xFF) != 0) {
      val last3 = ((length - 1L) >>> 16).toInt & 0xFF
      val oldNode3 = node3
      node3 = new Array[Array[Array[Byte]]](last3 + 1)
      java.lang.System.arraycopy(oldNode3, 0, node3, 0, last3)
      node3(last3) = node2
      aliased = 3
    }
    if (length <= (1L << 24)) new IndexTrieDataLE3(node3, length) else alias4
  }

  private[this] def alias4: IndexTrieDataLE = {
    if (aliased == 3 || ((length >>> 24).toInt & 0xFF) != 0) {
      val last4 = ((length - 1L) >>> 24).toInt & 0xFF
      val oldNode4 = node4
      node4 = new Array[Array[Array[Array[Byte]]]](last4 + 1)
      java.lang.System.arraycopy(oldNode4, 0, node4, 0, last4)
      node4(last4) = node3
      aliased = 4
    }
    if (length <= (1L << 32)) new IndexTrieDataLE4(node4, length) else alias5
  }

  private[this] def alias5: IndexTrieDataLE = {
    if (aliased == 4 || ((length >>> 32).toInt & 0xFF) != 0) {
      val last5 = ((length - 1L) >>> 32).toInt & 0xFF
      val oldNode5 = node5
      node5 = new Array[Array[Array[Array[Array[Byte]]]]](last5 + 1)
      java.lang.System.arraycopy(oldNode5, 0, node5, 0, last5)
      node5(last5) = node4
      aliased = 5
    }
    if (length <= (1L << 40)) new IndexTrieDataLE5(node5, length) else alias6
  }

  private[this] def alias6: IndexTrieDataLE = {
    if (aliased == 5 || ((length >>> 40).toInt & 0xFF) != 0) {
      val last6 = ((length - 1L) >>> 40).toInt & 0xFF
      val oldNode6 = node6
      node6 = new Array[Array[Array[Array[Array[Array[Byte]]]]]](last6 + 1)
      java.lang.System.arraycopy(oldNode6, 0, node6, 0, last6)
      node6(last6) = node5
      aliased = 6
    }
    new IndexTrieDataLE6(node6, length)
  }

  override def endian: LittleEndian = LittleEndian

  override def writeByte(value: Byte): Unit = {
    val offset = length.toInt & 0xFF
    gotoNode1()
    node1(offset) = value
    length += 1L
  }

  override def writeShort(value: Short): Unit = {
    val offset = length.toInt & 0xFF
    if (offset <= 254) {
      gotoNode1()
      node1(offset    ) = (value     ).toByte
      node1(offset + 1) = (value >> 8).toByte
      length += 2L
    }
    else {
      writeByte((value     ).toByte)
      writeByte((value >> 8).toByte)
    }
  }

  override def writeInt(value: Int): Unit = {
    val offset = length.toInt & 0xFF
    if (offset <= 252) {
      gotoNode1()
      node1(offset    ) = (value      ).toByte
      node1(offset + 1) = (value >>  8).toByte
      node1(offset + 2) = (value >> 16).toByte
      node1(offset + 3) = (value >> 24).toByte
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
    val offset = length.toInt & 0xFF
    if (offset <= 248) {
      gotoNode1()
      node1(offset    ) = (value      ).toByte
      node1(offset + 1) = (value >>  8).toByte
      node1(offset + 2) = (value >> 16).toByte
      node1(offset + 3) = (value >> 24).toByte
      node1(offset + 4) = (value >> 32).toByte
      node1(offset + 5) = (value >> 40).toByte
      node1(offset + 6) = (value >> 48).toByte
      node1(offset + 7) = (value >> 56).toByte
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
    case data: IndexTrieDataLE0 if length == 0L => ()
    case data: IndexTrieDataLE1 if (length & 0x00000000000000FFL) == 0L =>
      gotoNode2()
      if (length == (1L << 8)) node2(0) = node1
      node1 = data.node1
      node2((length >>> 8).toInt & 0xFF) = node1
      length += data.size
      if (data.size < (1L << 8)) aliased = 1
    case data: IndexTrieDataLE2 if (length & 0x000000000000FFFFL) == 0L =>
      gotoNode3()
      if (length == (1L << 16)) node3(0) = node2
      node2 = data.node2
      node1 = node2(node2.length - 1)
      node3((length >>> 16).toInt & 0xFF) = node2
      length += data.size
      if (data.size < (1L << 16)) aliased = 2
    case data: IndexTrieDataLE3 if (length & 0x0000000000FFFFFFL) == 0L =>
      gotoNode4()
      if (length == (1L << 24)) node4(0) = node3
      node3 = data.node3
      node2 = node3(node3.length - 1)
      node1 = node2(node2.length - 1)
      node4((length >>> 24).toInt & 0xFF) = node3
      length += data.size
      if (data.size < (1L << 24)) aliased = 3
    case data: IndexTrieDataLE4 if (length & 0x00000000FFFFFFFFL) == 0L =>
      gotoNode5()
      if (length == (1L << 32)) node5(0) = node4
      node4 = data.node4
      node3 = node4(node4.length - 1)
      node2 = node3(node3.length - 1)
      node1 = node2(node2.length - 1)
      node5((length >>> 32).toInt & 0xFF) = node4
      length += data.size
      if (data.size < (1L << 32)) aliased = 4
    case data: IndexTrieDataLE5 if (length & 0x000000FFFFFFFFFFL) == 0L =>
      gotoNode6()
      if (length == (1L << 40)) node6(0) = node5
      node5 = data.node5
      node4 = node5(node5.length - 1)
      node3 = node4(node4.length - 1)
      node2 = node3(node3.length - 1)
      node1 = node2(node2.length - 1)
      node6((length >>> 40).toInt & 0xFF) = node5
      length += data.size
      if (data.size < (1L << 40)) aliased = 5
    case data: IndexTrieDataLE6 if length == 0L =>
      node6 = data.node6
      node5 = node6(node6.length - 1)
      node4 = node5(node5.length - 1)
      node3 = node4(node4.length - 1)
      node2 = node3(node3.length - 1)
      node1 = node2(node2.length - 1)
      length = data.size
      if (data.size < (1L << 48)) aliased = 6
    case _ => super.writeData(data)
  }

  override def state: IndexTrieDataLE = alias

  override def clear(): Unit = {
    node1 = null
    node2 = null
    node3 = null
    node4 = null
    node5 = null
    node6 = null
    length = 0
    aliased = 0
  }

  override def toString: String = "IndexTrieDataLE"+"."+"Framer"+"()"
}
