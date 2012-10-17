/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

/** A memory allocator.
  * 
  * @author Chris Sachs
  */
abstract class Allocator extends scala.runtime.AbstractFunction1[Long, Mem] {
  /** Returns the maximum size in bytes this allocator can allocate. */
  def MaxSize: Long
  
  /** Returns allocated memory for a number of unit sized values.
    * Allocates `unit.size * count` bytes of memory. May return a `Mem` class
    * optimized for the given unit type.
    * 
    * @tparam T       the unit value type.
    * @param  count   the number of units to allocate.
    * @param  unit    the implicit unit value type.
    * @return the allocated, zero-filled memory.
    */
  def alloc[T](count: Long)(implicit unit: ValType[T]): Mem
  
  /** Returns a number of bytes of allocated memory.
    * 
    * @param  size  the number of bytes to allocate.
    * @return the allocated, zero-filled memory.
    */
  def apply(size: Long): Mem
}

/** Provides an implicit default memory allocator. */
object Allocator {
  /** Returns the default memory allocator. */
  implicit def default: Allocator = Mem
}
