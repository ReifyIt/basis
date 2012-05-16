/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

/** Contains an abstract memory model with struct typeclasses. */
package object memory {
  import Endianness._
  
  /** Returns an address advanced to its next power-of-2 alignment.
    * 
    * @param  alignment   the required alignment–forced to a power-of-2.
    * @param  address     the address to align.
    * @return the aligned address.
    */
  def align(alignment: Long)(address: Long): Long = {
    var n = alignment - 1L
    n |= n >>> 1
    n |= n >>> 2
    n |= n >>> 4
    n |= n >>> 8
    n |= n >>> 16
    n |= n >>> 32
    (address + n) & ~n
  }
  
  /** Returns the alignment of a struct type. */
  @inline def alignOf[T](implicit struct: Struct[T]): Long = struct.alignment
  
  /** Returns the size of a struct type. */
  @inline def sizeOf[T](implicit struct: Struct[T]): Long = struct.size
  
  /** An allocator for native-endian data backed by some primitive array. */
  lazy val ArrayData: Allocator = NativeEndian match {
    case BigEndian => ArrayDataBE
    case LittleEndian => ArrayDataLE
  }
  
  /** An allocator for native-endian data backed by a `Byte` array. */
  lazy val ByteData: ArrayAllocator[Byte] = NativeEndian match {
    case BigEndian => ByteDataBE
    case LittleEndian => ByteDataLE
  }
  
  /** An allocator for native-endian data backed by a `Short` array. */
  lazy val ShortData: ArrayAllocator[Short] = NativeEndian match {
    case BigEndian => ShortDataBE
    case LittleEndian => ShortDataLE
  }
  
  /** An allocator for native-endian data backed by an `Int` array. */
  lazy val IntData: ArrayAllocator[Int] = NativeEndian match {
    case BigEndian => IntDataBE
    case LittleEndian => IntDataLE
  }
  
  /** An allocator for native-endian data backed by a `Long` array. */
  lazy val LongData: ArrayAllocator[Long] = NativeEndian match {
    case BigEndian => LongDataBE
    case LittleEndian => LongDataLE
  }
  
  /** An allocator for native-endian data backed by a `ByteBuffer`. */
  /* This kills the 2.10.0-M3 compiler */
  //lazy val BufferData: Allocator = NativeEndian match {
  //  case BigEndian => BufferDataBE
  //  case LittleEndian => BufferDataLE
  //}
}
