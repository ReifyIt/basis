/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.memory

/** Big-endian data backed by a `Byte` array. */
private[memory] final class Data1BE(override val words: Array[Byte]) extends Data1 with DataBE {
  override def endian: BigEndian.type = BigEndian
  
  override def copy(size: Long): Data1BE = {
    Predef.require(0L <= size && size <= Int.MaxValue.toLong)
    val words = new Array[Byte](size.toInt)
    java.lang.System.arraycopy(this.words, 0, words, 0, java.lang.Math.min(this.words.length, words.length))
    new Data1BE(words)
  }
  
  override def loadByte(address: Long): Byte =
    words(address.toInt)
  
  override def storeByte(address: Long, value: Byte): Unit =
    words(address.toInt) = value
  
  override def loadUnalignedShort(address: Long): Short = {
    val i = address.toInt
    ((words(i)             << 8) |
     (words(i + 1) & 0xFF)).toShort
  }
  
  override def storeUnalignedShort(address: Long, value: Short) {
    val i = address.toInt
    words(i)     = (value >> 8).toByte
    words(i + 1) =  value.toByte
  }
  
  override def loadUnalignedInt(address: Long): Int = {
    val i = address.toInt
     (words(i)             << 24) |
    ((words(i + 1) & 0xFF) << 16) |
    ((words(i + 2) & 0xFF) <<  8) |
     (words(i + 3) & 0xFF)
  }
  
  override def storeUnalignedInt(address: Long, value: Int) {
    val i = address.toInt
    words(i)     = (value >> 24).toByte
    words(i + 1) = (value >> 16).toByte
    words(i + 2) = (value >>  8).toByte
    words(i + 3) =  value.toByte
  }
  
  override def loadUnalignedLong(address: Long): Long = {
    val i = address.toInt
     (words(i).toLong             << 56) |
    ((words(i + 1) & 0xFF).toLong << 48) |
    ((words(i + 2) & 0xFF).toLong << 40) |
    ((words(i + 3) & 0xFF).toLong << 32) |
    ((words(i + 4) & 0xFF).toLong << 24) |
    ((words(i + 5) & 0xFF).toLong << 16) |
    ((words(i + 6) & 0xFF).toLong <<  8) |
     (words(i + 7) & 0xFF).toLong
  }
  
  override def storeUnalignedLong(address: Long, value: Long) {
    val i = address.toInt
    words(i)     = (value >> 56).toByte
    words(i + 1) = (value >> 48).toByte
    words(i + 2) = (value >> 40).toByte
    words(i + 3) = (value >> 32).toByte
    words(i + 4) = (value >> 24).toByte
    words(i + 5) = (value >> 16).toByte
    words(i + 6) = (value >>  8).toByte
    words(i + 7) =  value.toByte
  }
  
  override def toString: String = "Data1BE"+"("+ size +")"
}

/** An allocator for big-endian data backed by a `Byte` array. */
private[memory] object Data1BE extends Allocator with (Long => Data1BE) {
  override def MaxSize: Long = Int.MaxValue.toLong
  
  override def alloc[T](count: Long)(implicit T: Struct[T]): Data1BE = apply(T.size * count)
  
  override def apply(size: Long): Data1BE = {
    Predef.require(0L <= size && size <= MaxSize)
    val words = new Array[Byte](size.toInt)
    new Data1BE(words)
  }
  
  override def toString: String = "Data1BE"
}