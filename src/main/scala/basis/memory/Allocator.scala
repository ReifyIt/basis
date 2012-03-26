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
  */
@implicitNotFound("No implicit allocator")
abstract class Allocator {
  /** The maximum number of bytes this allocator can allocate. */
  def MaxSize: Long
  
  /** Allocates data for a number of unit sized values.
    * Allocates `struct.size * count` bytes of data. May return a `Data` class
    * optimized for the given unit struct.
    * 
    * @tparam T       the unit struct type.
    * @param  count   the number of units to allocate.
    * @param  unit    the implicit unit struct.
    * @return the zero-filled data.
    */
  def alloc[T](count: Long)(implicit unit: Struct[T]): Data
  
  /** Stores a sequence of values to newly allocated data.
    * Allocates `struct.size * count` bytes of data.
    * 
    * @tparam T       the struct type.
    * @param  values  the sequence to store.
    * @param  struct  the implicit struct.
    * @return the initialized data.
    */
  def apply[T](values: T*)(implicit struct: Struct[T]): Data = {
    val count = values.length
    val data = alloc[T](count)
    var p = 0L
    var i = 0
    while (i < count) {
      struct.store(data, p, values(i))
      p += struct.size
      i += 1
    }
    data
  }
  
  /** Stores a sequence of generated values to newly allocated data.
    * Allocates `struct.size * count` bytes of data.
    * 
    * @tparam T       the struct type.
    * @param  count   the number of values in the sequence.
    * @param  value   the value generator.
    * @param  struct  the implicit struct.
    * @return the initialized data.
    */
  def fill[@specialized T](count: Long)(value: => T)(implicit struct: Struct[T]): Data = {
    val data = alloc[T](count)
    var p = 0L
    var i = 0L
    while (i < count) {
      struct.store(data, p, value)
      p += struct.size
      i += 1L
    }
    data
  }
  
  /** Stores a sequence of iterated function values to newly allocated data.
    * Allocates `struct.size * count` bytes of data.
    * 
    * @tparam T       the struct type.
    * @param  start   the initial value of the sequence.
    * @param  count   the number of values in the sequence.
    * @param  f       the iteratively applied function.
    * @param  struct  the implicit struct.
    * @return the initialized data.
    */
  def iterate[@specialized T](start: T, count: Long)(f: T => T)(implicit struct: Struct[T]): Data = {
    val data = alloc[T](count)
    if (count > 0L) {
      var value = start
      struct.store(data, 0L, value)
      var p = struct.size
      var i = 1L
      while (i < count) {
        value = f(value)
        struct.store(data, p, value)
        p += struct.size
        i += 1L
      }
    }
    data
  }
  
  /** Stores a sequence of enumerated function values to newly allocated data.
    * Applies sequential `Long` values to the enumerator function starting from 0.
    * Allocates `struct.size * count` bytes of data.
    * 
    * @tparam T       the struct type.
    * @param  count   the number of values in the sequence.
    * @param  f       the enumerator function.
    * @param  struct  the implicit struct.
    * @return the initialized data.
    */
  def tabulate[@specialized T](count: Long)(f: Long => T)(implicit struct: Struct[T]): Data = {
    val data = alloc[T](count)
    var p = 0L
    var i = 0L
    while (i < count) {
      struct.store(data, p, f(i))
      p += struct.size
      i += 1L
    }
    data
  }
}

/** Contains the implicit default allocator. */
object Allocator {
  /** The default allocator. */
  implicit def default: Allocator = Data.Block
}
