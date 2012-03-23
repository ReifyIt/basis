/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import scala.annotation.implicitNotFound

/** Abstracts over the allocation of `Data`.
  * 
  * @author Chris Sachs
  */
@implicitNotFound("No implicit Allocator")
abstract class Allocator {
  /** The maximum number of bytes this Allocator can allocate. */
  def MaxSize: Long
  
  /** Allocates `Data` for a number of unit sized values.
    * Allocates `struct.size * count` bytes of data. May return a `Data` class
    * optimized for the given unit struct.
    * 
    * @tparam T       the Scala type of the unit struct.
    * @param  count   the number of units to allocate.
    * @param  unit    the implicit unit struct.
    * @return the allocated zero-filled `Data`.
    */
  def alloc[T](count: Long)(implicit unit: Struct[T]): Data
  
  /** Allocates `Data` storing a sequence of values.
    * Allocates `struct.size * count` bytes of data.
    * 
    * @tparam T       the type of Scala values to store.
    * @param  values  the sequence to store.
    * @param  struct  the implicit value type of `T`.
    * @return the initialized `Data`
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
  
  /** Allocates `Data` storing a sequence of generated values.
    * Allocates `struct.size * count` bytes of data.
    * 
    * @tparam T       the type of Scala values to store.
    * @param  count   the number of values in the sequence.
    * @param  value   the value generator.
    * @param  struct  the implicit value type of `T`.
    * @return the initialized `Data`.
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
  
  /** Allocates `Data` storing a sequence of iterated function values.
    * Allocates `struct.size * count` bytes of data.
    * 
    * @tparam T       the type of Scala values to store.
    * @param  start   the initial value of the sequence.
    * @param  count   the number of values in the sequence.
    * @param  f       the repeatedly applied function.
    * @param  struct  the implicit value type of `f`.
    * @return the initialized `Data`.
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
  
  /** Allocates `Data` storing a sequence of indexed function values starting from 0.
    * Allocates `struct.size * count` bytes of data.
    * 
    * @tparam T       the type of Scala values to store.
    * @param  count   the number of values in the sequence.
    * @param  f       the indexed function.
    * @param  struct  the implicit value type of `f`.
    * @return the initialized `Data`.
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

/** Contains an implicit default `Allocator`. */
object Allocator {
  /** The default allocator. */
  implicit def default: Allocator = Data.Block
}
