/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.memory

/** A `Data` allocator. */
abstract class Allocator extends scala.runtime.AbstractFunction1[Long, Data] {
  /** Returns the maximum size in bytes this allocator can allocate. */
  def MaxSize: Long
  
  /** Returns newly allocated `Data` for a number of unit sized values.
    * Allocates `unit.size * count` bytes of data. May return a `Data` class
    * optimized for the given unit type.
    * 
    * @tparam T       the unit value type.
    * @param  count   the number of units to allocate.
    * @param  unit    the implicit unit value type.
    * @return the allocated, zero-filled data.
    */
  def alloc[T](count: Long)(implicit unit: ValType[T]): Data
  
  /** Returns a number of bytes of newly allocated `Data`.
    * 
    * @param  size  the number of bytes to allocate.
    * @return the allocated, zero-filled data.
    */
  def apply(size: Long): Data
}

/** Provides an implicit default `Data` allocator. */
object Allocator {
  /** Returns the default `Data` allocator. */
  implicit def default: Allocator = Data
}
