/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** Native-endian data backed by a `Long` array. */
private[memory] abstract class Data8 extends Data {
  def words: Array[Long]
  
  override def size: Long = words.length.toLong << 3
  
  override def unit: Int = 8
  
  override def copy(size: Long): Data8
  
  override def move(fromAddress: Long, toAddress: Long, size: Long) {
    val fromLimit = fromAddress + size
    val toLimit = toAddress + size
    if (fromAddress == toAddress) ()
    else if ((size & 7L) == 0L && (fromAddress & 7L) == 0L && (toAddress & 7L) == 0L)
      java.lang.System.arraycopy(words, (fromAddress >> 3).toInt,
                                 words, (toAddress   >> 3).toInt, (size >> 3).toInt)
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
    else if ((fromAddress & 7L) != 0L) {
      storeByte(fromAddress, 0.toByte)
      clear(fromAddress + 1L, untilAddress)
    }
    else {
      java.util.Arrays.fill(words, (fromAddress  >> 3).toInt,
                                   (untilAddress >> 3).toInt, 0L)
      clear(untilAddress & -8L, untilAddress)
    }
  }
}

/** An allocator for native-endian data backed by a `Long` array. */
private[memory] object Data8 extends Allocator with (Long => Data8) {
  override def MaxSize: Long = Int.MaxValue.toLong << 3
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Data8 = NativeEndian match {
    case BigEndian    => Data8BE.alloc[T](count)(unit)
    case LittleEndian => Data8LE.alloc[T](count)(unit)
  }
  
  override def apply(size: Long): Data8 = NativeEndian match {
    case BigEndian    => Data8BE(size)
    case LittleEndian => Data8LE(size)
  }
  
  override def toString: String = "Data8"
}