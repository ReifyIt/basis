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
  
  /** Returns an `Int` address advanced to a power-of-two alignment.
    * 
    * @param  alignment   the required alignment–forced to a power-of-two.
    * @param  address     the address to align.
    * @return the aligned address.
    */
  def align(alignment: Int)(address: Int): Int = {
    var n = alignment - 1
    n |= n >>> 1
    n |= n >>> 2
    n |= n >>> 4
    n |= n >>> 8
    n |= n >>> 16
    (address + n) & ~n
  }
  
  /** Returns a `Long` address advanced to a power-of-two alignment.
    * 
    * @param  alignment   the required alignment–forced to a power-of-two.
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
  
  /** Returns the alignment of some value type. */
  @inline def alignOf[T](implicit struct: ValueType[T]): Long = struct.alignment
  
  /** Returns the size of some value type. */
  @inline def sizeOf[T](implicit struct: ValueType[T]): Long = struct.size
  
  /** Returns the implicit data type for some instance type.
    * 
    * @example {{{
    * scala> typeOf[(Int, Double)]
    * res0: basis.memory.DataType[(Int, Double)] = Row2(PaddedInt, PaddedDouble).project(size = 16, alignment = 8)
    * }}}
    * 
    * @tparam T         the instance type of the data type to get.
    * @param  datatype  the implicit data type itself.
    * @return the implicitly supplied data type.
    */
  @inline def typeOf[T](implicit datatype: DataType[T]): DataType[T] = datatype
  
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
}
