/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
import java.nio.ByteBuffer
import java.nio.ByteOrder

import scala.reflect.ClassTag

import Endianness._

/** A byte-addressable memory region.
  * 
  * ==Address space==
  * 
  * Data has a 64-bit ''address space'' ranging from `0` until `size`. Each
  * ''address'' in the space identifies a unique storage location for a single
  * `Byte` value. Multi-byte values occupy multiple storage locations and thus
  * have multiple addresses–one address per byte. The lowest address of a
  * multi-byte sequence canonically refers to the whole byte sequence.
  * 
  * ==Data values==
  * 
  * Data stores structured value types. ''Value type'' in this context stands
  * for an isomorphism between Scala values and fixed-length byte sequences,
  * with a possible restriction on address alignment.
  * 
  * ===Primitive values===
  * 
  * Primitive value types have dedicated `load` and `store` methods. Multi-byte
  * primitives have ''aligned'' and ''unaligned'' variants. The data's `endian`
  * property determines the ''endianness'' of multi-byte data values.
  * 
  * ===Struct values===
  * 
  * [[basis.memory.Struct]] typeclasses abstract over value types. Generic
  * `load` and `store` methods delegate to the implicit struct typeclass
  * associated with each method's type parameter.
  * 
  * ==Alignment==
  * 
  * N-byte divisible addresses are said to be N-byte ''aligned''. Using aligned
  * addresses reduces some multi-byte data accesses to single array operations,
  * which can noticeably improve performance. Alignment sensitive allocators
  * such as the default `Block` allocator try to allocate Data backed by a
  * primitive array whose element size matches the alignment of the unit struct
  * passed to the allocator. This allocation strategy makes possible the
  * performance benefit of using aligned addresses.
  * 
  * Aligned data accesses truncate unaligned addresses to the required alignment.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * scala> val data = Data.alloc[Int](1L) // allocate data for a single Int value.
  * data: basis.memory.Data = Block4LE(4) // Data class will vary by architecture.
  * 
  * scala> data.storeInt(0L, 0xCAFEBABE) // store an Int value to address 0.
  * 
  * scala> data.loadInt(0L).toHexString // load an Int value from address 0.
  * res1: String = cafebabe
  * 
  * scala> data.loadByte(0L).toHexString // load the low byte.
  * res2: String = ffffffbe // the least significant byte comes first in this case.
  * 
  * scala> data.loadShort(2L).toHexString // load the high bytes.
  * res3: String = ffffcafe // toHexString sign extends the result to an Int.
  * 
  * scala> data.loadShort(1L).toHexString // load an unaligned address.
  * res4: String = ffffbabe // the address was truncated, oops.
  * 
  * scala> data.loadUnalignedShort(1L).toHexString // load the middle bytes.
  * res5: String = fffffeba
  * }}}
  */
trait Data extends Any {
  /** The number of addressable bytes in the address space. */
  def size: Long
  
  /** The internal word size. */
  def unit: Int
  
  /** The data's byte order. */
  def endian: Endianness
  
  /** Returns a resized copy of this data.
    * 
    * @param  size  the number of bytes to copy.
    * @return the copied data.
    */
  def copy(size: Long = this.size): Data
  
  /** Loads a single byte.
    * 
    * @param  address   the address to load.
    * @return the loaded `Byte` value.
    */
  def loadByte(address: Long): Byte
  
  /** Stores a single byte.
    * 
    * @param  address   the storage address.
    * @param  value     the `Byte` value to store.
    */
  def storeByte(address: Long, value: Byte): Unit
  
  /** Loads a 2-byte `endian` ordered word as a native-endian `Short` value.
    * Truncates `address` to 2-byte alignment.
    * 
    * @param  address   the 2-byte aligned address to load.
    * @return the loaded `Short` value.
    */
  def loadShort(address: Long): Short =
    loadUnalignedShort(address & ~1L)
  
  /** Stores a native-endian `Short` value as a 2-byte `endian` ordered word.
    * Truncates `address` to 2-byte alignment.
    * 
    * @param  address   the 2-byte aligned storage address.
    * @param  value     the `Short` value to store.
    */
  def storeShort(address: Long, value: Short): Unit =
    storeUnalignedShort(address & ~1L, value)
  
  /** Loads a 4-byte `endian` ordered word as a native-endian `Int` value.
    * Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned address to load.
    * @return the loaded `Int` value.
    */
  def loadInt(address: Long): Int =
    loadUnalignedInt(address & ~3L)
  
  /** Stores a native-endian `Int` value as a 4-byte `endian` ordered word.
    * Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned storage address.
    * @param  value     the `Int` value to store.
    */
  def storeInt(address: Long, value: Int): Unit =
    storeUnalignedInt(address & ~3L, value)
  
  /** Loads an 8-byte `endian` ordered word as a native-endian `Long` value.
    * Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned address to load.
    * @return the loaded `Long` value.
    */ 
  def loadLong(address: Long): Long =
    loadUnalignedLong(address & ~7L)
  
  /** Store a native-endian `Long` value as an 8-byte `endian` ordered word.
    * Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned storage address.
    * @param  value     the `Long` value to store.
    */
  def storeLong(address: Long, value: Long): Unit =
    storeUnalignedLong(address & ~7L, value)
  
  /** Loads a 2-byte `endian` ordered word as a native-endian `Char` value.
    * Truncates `address` to 2-byte alignment.
    * 
    * @param  address   the 2-byte aligned address to load.
    * @return the loaded `Char` value.
    */
  def loadChar(address: Long): Char =
    loadShort(address).toChar
  
  /** Stores a native-endian `Char` value as a 2-byte `endian` ordered word.
    * Truncates `address` to 2-byte alignment.
    * 
    * @param  address   the 2-byte aligned storage address.
    * @param  value     the `Char` value to store.
    */
  def storeChar(address: Long, value: Char): Unit =
    storeShort(address, value.toShort)
  
  /** Loads a 4-byte `endian` ordered word as a native-endian `Float` value.
    * Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned address to load.
    * @return the loaded `Float` value.
    */
  def loadFloat(address: Long): Float =
    intBitsToFloat(loadInt(address))
  
  /** Stores a native-endian `Float` value as a 4-byte `endian` ordered word.
    * Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned storage address.
    * @param  value     the `Float` value to store.
    */
  def storeFloat(address: Long, value: Float): Unit =
    storeInt(address, floatToRawIntBits(value))
  
  /** Loads an 8-byte `endian` ordered word as a native-endian `Double` value.
    * Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned address to load.
    * @return the loaded `Double` value.
    */
  def loadDouble(address: Long): Double =
    longBitsToDouble(loadLong(address))
  
  /** Stores a native-endian `Double` value as an 8-byte `endian` ordered word.
    * Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned storage address.
    * @param  value     the `Double` value to store.
    */
  def storeDouble(address: Long, value: Double): Unit =
    storeLong(address, doubleToRawLongBits(value))
  
  /** Loads a 2-byte `endian` ordered word as a native-endian `Short` value.
    * 
    * @param  address   the unaligned address to load.
    * @return the loaded `Short` value.
    */
  def loadUnalignedShort(address: Long): Short = endian match {
    case BigEndian =>
      ((loadByte(address)              << 8) |
       (loadByte(address + 1L) & 0xFF)).toShort
    case LittleEndian =>
      ((loadByte(address)      & 0xFF)       |
       (loadByte(address + 1L)         << 8)).toShort
  }
  
  /** Stores a native-endian `Short` value as a 2-byte `endian` ordered word.
    * 
    * @param  address   the unaligned storage address.
    * @param  value     the `Short` value to store.
    */
  def storeUnalignedShort(address: Long, value: Short): Unit = endian match {
    case BigEndian =>
      storeByte(address,      (value >> 8).toByte)
      storeByte(address + 1L,  value.toByte)
    case LittleEndian =>
      storeByte(address,       value.toByte)
      storeByte(address + 1L, (value >> 8).toByte)
  }
  
  /** Loads a 4-byte `endian` ordered word as a native-endian `Int` value.
    * 
    * @param  address   the unaligned address to load.
    * @return the loaded `Int` value.
    */
  def loadUnalignedInt(address: Long): Int = endian match {
    case BigEndian =>
       (loadByte(address)              << 24) |
      ((loadByte(address + 1L) & 0xFF) << 16) |
      ((loadByte(address + 2L) & 0xFF) <<  8) |
       (loadByte(address + 3L) & 0xFF)
    case LittleEndian =>
       (loadByte(address)      & 0xFF)        |
      ((loadByte(address + 1L) & 0xFF) <<  8) |
      ((loadByte(address + 2L) & 0xFF) << 16) |
       (loadByte(address + 3L)         << 24)
  }
  
  /** Stores a native-endian `Int` value as a 4-byte `endian` ordered word.
    * 
    * @param  address   the unaligned storage address.
    * @param  value     the `Int` value to store.
    */
  def storeUnalignedInt(address: Long, value: Int): Unit = endian match {
    case BigEndian =>
      storeByte(address,      (value >> 24).toByte)
      storeByte(address + 1L, (value >> 16).toByte)
      storeByte(address + 2L, (value >>  8).toByte)
      storeByte(address + 3L,  value.toByte)
    case LittleEndian =>
      storeByte(address,       value.toByte)
      storeByte(address + 1L, (value >>  8).toByte)
      storeByte(address + 2L, (value >> 16).toByte)
      storeByte(address + 3L, (value >> 24).toByte)
  }
  
  /** Loads an 8-byte `endian` ordered word as a native-endian `Long` value.
    * 
    * @param  address   the unaligned address to load.
    * @return the loaded `Long` value.
    */
  def loadUnalignedLong(address: Long): Long = endian match {
    case BigEndian =>
       (loadByte(address).toLong              << 56) |
      ((loadByte(address + 1L) & 0xFF).toLong << 48) |
      ((loadByte(address + 2L) & 0xFF).toLong << 40) |
      ((loadByte(address + 3L) & 0xFF).toLong << 32) |
      ((loadByte(address + 4L) & 0xFF).toLong << 24) |
      ((loadByte(address + 5L) & 0xFF).toLong << 16) |
      ((loadByte(address + 6L) & 0xFF).toLong <<  8) |
       (loadByte(address + 7L) & 0xFF).toLong
    case LittleEndian =>
       (loadByte(address)      & 0xFF).toLong        |
      ((loadByte(address + 1L) & 0xFF).toLong <<  8) |
      ((loadByte(address + 2L) & 0xFF).toLong << 16) |
      ((loadByte(address + 3L) & 0xFF).toLong << 24) |
      ((loadByte(address + 4L) & 0xFF).toLong << 32) |
      ((loadByte(address + 5L) & 0xFF).toLong << 40) |
      ((loadByte(address + 6L) & 0xFF).toLong << 48) |
       (loadByte(address + 7L).toLong         << 56)
  }
  
  /** Stores a native-endian `Long` value as an 8-byte `endian` ordered word.
    * 
    * @param  address   the unaligned storage address.
    * @param  value     the `Long` value to store.
    */
  def storeUnalignedLong(address: Long, value: Long): Unit = endian match {
    case BigEndian =>
      storeByte(address,      (value >> 56).toByte)
      storeByte(address + 1L, (value >> 48).toByte)
      storeByte(address + 2L, (value >> 40).toByte)
      storeByte(address + 3L, (value >> 32).toByte)
      storeByte(address + 4L, (value >> 24).toByte)
      storeByte(address + 5L, (value >> 16).toByte)
      storeByte(address + 6L, (value >>  8).toByte)
      storeByte(address + 7L,  value.toByte)
    case LittleEndian =>
      storeByte(address,       value.toByte)
      storeByte(address + 1L, (value >>  8).toByte)
      storeByte(address + 2L, (value >> 16).toByte)
      storeByte(address + 3L, (value >> 24).toByte)
      storeByte(address + 4L, (value >> 32).toByte)
      storeByte(address + 5L, (value >> 40).toByte)
      storeByte(address + 6L, (value >> 48).toByte)
      storeByte(address + 7L, (value >> 56).toByte)
  }
  
  /** Loads a 2-byte `endian` ordered word as a native-endian `Char` value.
    * 
    * @param  address   the unaligned address to load.
    * @return the loaded `Char` value.
    */
  def loadUnalignedChar(address: Long): Char =
    loadUnalignedShort(address).toChar
  
  /** Stores a native-endian `Char` value as a 2-byte `endian` ordered word.
    * 
    * @param  address   the unaligned storage address.
    * @param  value     the `Char` value to store.
    */
  def storeUnalignedChar(address: Long, value: Char): Unit =
    storeUnalignedShort(address, value.toShort)
  
  /** Loads a 4-byte `endian` ordered word as a native-endian `Float` value.
    * 
    * @param  address   the unaligned address to load.
    * @return the loaded `Float` value.
    */
  def loadUnalignedFloat(address: Long): Float =
    intBitsToFloat(loadUnalignedInt(address))
  
  /** Stores a native-endian `Float` value as a 4-byte `endian` ordered word.
    * 
    * @param  address   the unaligned storage address.
    * @param  value     the `Float` value to store.
    */
  def storeUnalignedFloat(address: Long, value: Float): Unit =
    storeUnalignedInt(address, floatToRawIntBits(value))
  
  /** Loads an 8-byte `endian` ordered word as a native-endian `Double` value.
    * 
    * @param  address   the unaligned address to load.
    * @return the loaded `Double` value.
    */
  def loadUnalignedDouble(address: Long): Double =
    longBitsToDouble(loadUnalignedLong(address))
  
  /** Stores a native-endian `Double` value as an 8-byte `endian` ordered word.
    * 
    * @param  address   the unaligned storage address.
    * @param  value     the `Double` value to store.
    */
  def storeUnalignedDouble(address: Long, value: Double): Unit =
    storeUnalignedLong(address, doubleToRawLongBits(value))
  
  /** Loads an instance of a data value.
    * 
    * @tparam T         the struct type.
    * @param  address   the aligned address to load.
    * @param  struct    the implicit struct.
    * @return the loaded instance.
    */
  final def load[@specialized T](address: Long)(implicit struct: Struct[T]): T =
    struct.load(this, address)
  
  /** Stores an instance as a data value.
    * 
    * @tparam T         the struct type.
    * @param  address   the aligned storage address.
    * @param  value     the instance to store.
    * @param  struct    the implicit struct.
    */
  final def store[@specialized T](address: Long, value: T)(implicit struct: Struct[T]): Unit =
    struct.store(this, address, value)
  
  /** Loads a sequence of data values as a new instance array.
    * 
    * @tparam T         the struct type.
    * @param  address   the aligned address to load.
    * @param  count     the number of values to load.
    * @param  struct    the implicit struct.
    * @param  classTag  the reflective type of the array to load.
    * @return the loaded array of Scala values.
    */
  def loadArray[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean, AnyRef) T]
      (address: Long, count: Int)
      (implicit struct: Struct[T], classTag: ClassTag[T]): Array[T] = {
    val array = classTag.newArray(count)
    copyToArray[T](address, array, 0, count)
    array
  }
  
  /** Copies a sequence of data values to an instance array slice.
    * 
    * @tparam T         the struct type.
    * @param  address   the aligned address to load.
    * @param  array     the array to copy to.
    * @param  start     the lower bound of the array slice to copy to.
    * @param  count     the number of values to copy.
    * @param  struct    the implicit struct.
    */
  def copyToArray[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean, AnyRef) T]
      (address: Long, array: Array[T], start: Int, count: Int)
      (implicit struct: Struct[T]) {
    val end = start + count
    var p = address
    var i = start
    while (i < end) {
      array(i) = struct.load(this, p)
      p += struct.size
      i += 1
    }
  }
  
  /** Stores an instance array slice as a sequence of data values.
    * 
    * @tparam T         the struct type.
    * @param  address   the aligned storage address.
    * @param  array     the array to store from.
    * @param  start     the lower bound of the array slice to store from.
    * @param  count     the number of values to store.
    * @param  struct    the implicit struct.
    */
  def storeArray[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean, AnyRef) T]
      (address: Long, array: Array[T], start: Int, count: Int)
      (implicit struct: Struct[T]) {
    val end = start + count
    var p = address
    var i = start
    while (i < end) {
      struct.store(this, p, array(i))
      p += struct.size
      i += 1
    }
  }
  
  /** Copies a byte sequence to this data.
    * 
    * @param  address   the address to transfer to.
    * @param  data      the data to transfer from.
    * @param  base      the address to transfer from.
    * @param  size      the number of bytes to transfer.
    */
  def transfer(address: Long, data: Data, base: Long, size: Long) {
    val limit = address + size
    var p = address
    var q = base
    if (unit == 8 && (size & 7L) == 0L && (p & 7L) == 0L && (q & 7L) == 0L && endian == data.endian) {
      while (p < limit) {
        storeLong(p, data.loadLong(q))
        p += 8L
        q += 8L
      }
    }
    else if (unit == 4 && (size & 3L) == 0L && (p & 3L) == 0L && (q & 3L) == 0L && endian == data.endian) {
      while (p < limit) {
        storeInt(p, data.loadInt(q))
        p += 4L
        q += 4L
      }
    }
    else if (unit == 2 && (size & 1L) == 0L && (p & 1L) == 0L && (q & 1L) == 0L && endian == data.endian) {
      while (p < limit) {
        storeShort(p, data.loadShort(q))
        p += 2L
        q += 2L
      }
    }
    else {
      while (p < limit) {
        storeByte(p, data.loadByte(q))
        p += 1L
        q += 1L
      }
    }
  }
  
  /** Moves a byte sequence to a different, potentially overlapping address.
    * 
    * @param  fromAddress   the address to copy from.
    * @param  toAddress     the address to copy to.
    * @param  size          the number of bytes to copy.
    */
  def move(fromAddress: Long, toAddress: Long, size: Long) {
    val fromLimit = fromAddress + size
    val toLimit = toAddress + size
    if (toAddress == fromAddress) ()
    else if (toAddress <= fromAddress || toAddress >= fromLimit) {
      var p = toAddress
      var q = fromAddress
      if (unit == 8 && (size & 7L) == 0L && (p & 7L) == 0L && (q & 7L) == 0L) {
        while (p < toLimit) {
          storeLong(p, loadLong(q))
          p += 8L
          q += 8L
        }
      }
      else if (unit == 4 && (size & 3L) == 0L && (p & 3L) == 0L && (q & 3L) == 0L) {
        while (p < toLimit) {
          storeInt(p, loadInt(q))
          p += 4L
          q += 4L
        }
      }
      else if (unit == 2 && (size & 1L) == 0L && (p & 1L) == 0L && (q & 1L) == 0L) {
        while (p < toLimit) {
          storeShort(p, loadShort(q))
          p += 2L
          q += 2L
        }
      }
      else {
        while (p < toLimit) {
          storeByte(p, loadByte(q))
          p += 1L
          q += 1L
        }
      }
    }
    else {
      var p = toLimit - 1L
      var q = fromLimit - 1L
      if (unit == 8 && (size & 7L) == 0L && (p & 7L) == 0L && (q & 7L) == 0L) {
        while (p >= toAddress) {
          storeLong(p, loadLong(q))
          p -= 8L
          q -= 8L
        }
      }
      else if (unit == 4 && (size & 3L) == 0L && (p & 3L) == 0L && (q & 3L) == 0L) {
        while (p >= toAddress) {
          storeInt(p, loadInt(q))
          p -= 4L
          q -= 4L
        }
      }
      else if (unit == 2 && (size & 1L) == 0L && (p & 1L) == 0L && (q & 1L) == 0L) {
        while (p >= toAddress) {
          storeShort(p, loadShort(q))
          p -= 2L
          q -= 2L
        }
      }
      else {
        while (p >= toAddress) {
          storeByte(p, loadByte(q))
          p -= 1L
          q -= 1L
        }
      }
    }
  }
  
  /** Zeros a range of addresses.
    * 
    * @param  base    the lower bound of the address range.
    * @param  limit   the exclusive least upper bound of the address range.
    */
  def clear(base: Long = 0L, limit: Long = size) {
    var p = base
    if (unit == 8 && (base & 7L) == 0L && (limit & 7L) == 0L) {
      while (p < limit) {
        storeLong(p, 0L)
        p += 8L
      }
    }
    else if (unit == 4 && (base & 3L) == 0L && (limit & 3L) == 0L) {
      while (p < limit) {
        storeInt(p, 0)
        p += 4L
      }
    }
    else if (unit == 2 && (base & 1L) == 0L && (limit & 1L) == 0L) {
      while (p < limit) {
        storeShort(p, 0.toShort)
        p += 2L
      }
    }
    else {
      while (p < limit) {
        storeByte(p, 0.toByte)
        p += 1L
      }
    }
  }
}

/** Contains convenience functions for allocating data. */
object Data {
  /** Allocates data for a number of unit sized values.
    * Allocates `struct.size * count` bytes of data. May return a `Data` class
    * optimized for the given unit struct. This convenience function delegates
    * to the implicitly scoped allocator.
    * 
    * @tparam T           the unit struct type.
    * @param  count       the number of units to allocate.
    * @param  allocator   the implicit allocator to delegate to.
    * @param  unit        the implicit unit struct.
    * @return the allocated zero-filled data.
    */
  @inline def alloc[T](count: Long)(implicit allocator: Allocator, unit: Struct[T]): Data =
    allocator.alloc[T](count)
  
  /** Allocates a number of bytes of data. This convenience function delegates
    * to the implicitly scoped allocator.
    * 
    * @param  size        the number of bytes to allocate.
    * @param  allocator   the implicit allocator to delegate to.
    * @return the allocated zero-filled data.
    */
  @inline def apply(size: Long)(implicit allocator: Allocator): Data = allocator(size)
}
