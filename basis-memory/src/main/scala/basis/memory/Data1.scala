/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.memory

/** Native-endian data backed by a `Byte` array.
  * 
  * @author Chris Sachs
  * @since  0.0
  */
private[memory] abstract class Data1 extends Data {
  def words: Array[Byte]
  
  override def size: Long = words.length.toLong
  
  override def unit: Int = 1
  
  override def copy(size: Long): Data1
  
  override def move(fromAddress: Long, toAddress: Long, size: Long): Unit =
    java.lang.System.arraycopy(words, fromAddress.toInt, words, toAddress.toInt, size.toInt)
  
  override def clear(fromAddress: Long, untilAddress: Long): Unit =
    java.util.Arrays.fill(words, fromAddress.toInt, untilAddress.toInt, 0.toByte)
}

/** An allocator for native-endian data backed by a `Byte` array. */
private[memory] object Data1 extends Allocator with (Long => Data1) {
  override def MaxSize: Long = Int.MaxValue.toLong
  
  override def alloc[T](count: Long)(implicit T: Struct[T]): Data1 = NativeEndian match {
    case BigEndian    => Data1BE.alloc[T](count)(T)
    case LittleEndian => Data1LE.alloc[T](count)(T)
  }
  
  override def apply(size: Long): Data1 = NativeEndian match {
    case BigEndian    => Data1BE(size)
    case LittleEndian => Data1LE(size)
  }
  
  override def toString: String = "Data1"
}
