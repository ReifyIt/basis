/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** Native-endian data backed by a `Short` array. */
private[memory] abstract class Data2 extends Data {
  def words: Array[Short]
  
  override def size: Long = words.length.toLong << 1
  
  override def unit: Int = 2
  
  override def copy(size: Long): Data2
  
  override def move(fromAddress: Long, toAddress: Long, size: Long) {
    val fromLimit = fromAddress + size
    val toLimit = toAddress + size
    if (fromAddress == toAddress) ()
    else if ((size & 1L) == 0L && (fromAddress & 1L) == 0L && (toAddress & 1L) == 0L)
      java.lang.System.arraycopy(words, (fromAddress >> 1).toInt,
                                 words, (toAddress   >> 1).toInt, (size >> 1).toInt)
    else if (fromAddress >= toAddress || fromLimit <= toAddress) {
      var p = fromAddress
      var q = toAddress
      while (q < toLimit) {
        storeByte(q, loadByte(p))
        p += 1L
        q += 1L
      }
    }
    else {
      var p = fromLimit - 1L
      var q = toLimit - 1L
      while (q >= toAddress) {
        storeByte(q, loadByte(p))
        p -= 1L
        q -= 1L
      }
    }
  }
  
  override def clear(fromAddress: Long, untilAddress: Long) {
    if (fromAddress > untilAddress)
      throw new java.lang.IllegalArgumentException("fromAddress > untilAddress")
    else if (fromAddress == untilAddress) ()
    else if ((fromAddress & 1L) != 0L) {
      storeByte(fromAddress, 0.toByte)
      clear(fromAddress + 1L, untilAddress)
    }
    else {
      java.util.Arrays.fill(words, (fromAddress  >> 1).toInt,
                                   (untilAddress >> 1).toInt, 0.toShort)
      clear(untilAddress & -2L, untilAddress)
    }
  }
}

/** An allocator for native-endian data backed by a `Short` array. */
private[memory] object Data2 extends Allocator with (Long => Data2) {
  override def MaxSize: Long = Int.MaxValue.toLong << 1
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Data2 = NativeEndian match {
    case BigEndian    => Data2BE.alloc[T](count)(unit)
    case LittleEndian => Data2LE.alloc[T](count)(unit)
  }
  
  override def apply(size: Long): Data2 = NativeEndian match {
    case BigEndian    => Data2BE(size)
    case LittleEndian => Data2LE(size)
  }
  
  override def toString: String = "Data2"
}
