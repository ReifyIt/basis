//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

import basis.collections._

/** A memory allocator.
  *
  * @author   Chris Sachs
  * @version  0.0
  * @since    0.0
  */
trait Allocator extends (Long => Data) {
  /** Returns the maximum allocation size supported by this allocator. */
  def MaxSize: Long

  /** Returns the byte order of allocated data. */
  def Endian: Endianness

  /** Returns `sizeOf[T] * count` bytes of newly allocated data.
    * Implementations may return a `Data` class optimized for the given struct.
    *
    * @tparam T       the struct instance type.
    * @param  count   the number of struct values to allocate.
    * @param  T       the implicit struct.
    * @return the allocated, zero-filled data.
    */
  def alloc[T](count: Long)(implicit T: Struct[T]): Data

  /** Returns `size` bytes of newly allocated data.
    *
    * @param  size  the number of bytes to allocate.
    * @return the allocated, zero-filled data.
    */
  override def apply(size: Long): Data

  /** Returns a new growable `Framer` fed by this allocator.  */
  def Framer(): Framer with State[Data] = new DataFramer(this)
}

/** A factory for memory allocators. */
object Allocator {
  /** Returns the default memory allocator. */
  implicit def default: Allocator = Data

  def apply[T](endian: Endianness)(implicit T: Struct[T]): Allocator = {
    if (endian eq BigEndian) {
      if (endian.isNative) T.alignment match {
        case 1L => Data1BE
        case 2L => Data2BE
        case 4L => Data4BE
        case _  => Data8BE
      }
      else Data1BE
    }
    else if (endian eq LittleEndian) {
      if (endian.isNative) T.alignment match {
        case 1L => Data1LE
        case 2L => Data2LE
        case 4L => Data4LE
        case _  => Data8LE
      }
      else Data1LE
    }
    else throw new MatchError(endian)
  }
}
