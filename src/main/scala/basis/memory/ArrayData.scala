/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** Data backed by an array.
  * 
  * @author Chris Sachs
  * 
  * @tparam V   the element type of the backing array.
  */
trait ArrayData[V] extends Any with Data {
  /** The backing array. */
  def array: Array[V]
  
  override def copy(size: Long): ArrayData[V]
}

/** An allocator for big-endian data backed by some primitive array. */
object ArrayDataBE extends Allocator {
  override def MaxSize: Long = Int.MaxValue.toLong << 3
  
  override def alloc[T](count: Long)(implicit unit: Struct[T]): Data = {
    val size = unit.size * count
    if (size <= Int.MaxValue.toLong) unit.alignment match {
      case 1L => ByteDataBE(size)
      case 2L => ShortDataBE(size)
      case 4L => IntDataBE(size)
      case _  => LongDataBE(size)
    }
    else if (size <= (Int.MaxValue.toLong << 1)) unit.alignment match {
      case 1L | 2L => ShortDataBE(size)
      case 4L => IntDataBE(size)
      case _  => LongDataBE(size)
    }
    else if (size <= (Int.MaxValue.toLong << 2)) unit.alignment match {
      case 1L | 2L | 4L => IntDataBE(size)
      case _ => LongDataBE(size)
    }
    else LongDataBE(size)
  }
  
  override def apply(size: Long): Data = alloc[Byte](size)
  
  override def toString: String = "ArrayDataBE"
}

/** An allocator for little-endian data backed by some primitive array. */
object ArrayDataLE extends Allocator {
  override def MaxSize: Long = Int.MaxValue << 3
  
  override def alloc[T](count: Long)(implicit unit: Struct[T]): Data = {
    val size = unit.size * count
    if (size <= Int.MaxValue.toLong) unit.alignment match {
      case 1L => ByteDataLE(size)
      case 2L => ShortDataLE(size)
      case 4L => IntDataLE(size)
      case _  => LongDataLE(size)
    }
    else if (size <= (Int.MaxValue.toLong << 1)) unit.alignment match {
      case 1L | 2L => ShortDataLE(size)
      case 4L => IntDataLE(size)
      case _  => LongDataLE(size)
    }
    else if (size <= (Int.MaxValue.toLong << 2)) unit.alignment match {
      case 1L | 2L | 4L => IntDataLE(size)
      case _ => LongDataLE(size)
    }
    else LongDataLE(size)
  }
  
  override def apply(size: Long): Data = alloc[Byte](size)
  
  override def toString: String = "ArrayDataLE"
}
