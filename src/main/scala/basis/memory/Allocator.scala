/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import scala.annotation.implicitNotFound

/** A `Data` allocator.
  * 
  * @author Chris Sachs
  * 
  * @see  [[basis.memory.Data]]
  */
@implicitNotFound("Cannot find implicit allocator.")
trait Allocator {
  /** The maximum number of bytes this allocator can allocate. */
  def MaxSize: Long
  
  /** Allocates data for a number of unit sized values.
    * Allocates `struct.size * count` bytes of data. May return a `Data` class
    * optimized for the given unit struct.
    * 
    * @tparam T       the unit struct type.
    * @param  count   the number of units to allocate.
    * @param  unit    the implicit unit struct.
    * @return the allocated zero-filled data.
    */
  def alloc[T](count: Long)(implicit unit: ValType[T]): Data
  
  /** Allocates a number of bytes of data.
    * 
    * @param  size  the number of bytes to allocate.
    * @return the allocated zero-filled data.
    */
  def apply(size: Long): Data
}

/** Contains the implicit default allocator. */
object Allocator {
  /** The implicit default allocator. */
  implicit val default: Allocator = ArrayData
}
