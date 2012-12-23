/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.memory

/** A memory allocator. */
abstract class Allocator extends (Long => Data) {
  /** Returns the maximum allocation size supported by this allocator. */
  def MaxSize: Long
  
  /** Returns newly allocated memory for a certain number of struct values.
    * Allocates `sizeOf[T] * count` bytes of data. Implementations may
    * return a `Data` class optimized for the given struct.
    * 
    * @tparam T       the struct instance type.
    * @param  count   the number of struct values to allocate.
    * @param  T       the implicit struct.
    * @return the allocated, zero-filled data.
    */
  def alloc[T](count: Long)(implicit T: Struct[T]): Data
  
  /** Returns a certain number of bytes of newly allocated memory.
    * 
    * @param  size  the number of bytes to allocate.
    * @return the allocated, zero-filled data.
    */
  def apply(size: Long): Data
}

/** An implicit factory for the default memory allocator. */
object Allocator {
  /** Returns the default memory allocator. */
  implicit def default: Allocator = Data
}
