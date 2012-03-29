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

import scala.reflect.ClassManifest

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
abstract class Data {
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
    * @param  manifest  the manifest to create the array of type `T`.
    * @return the loaded array of Scala values.
    */
  def loadArray[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean, AnyRef) T]
      (address: Long, count: Int)
      (implicit struct: Struct[T], manifest: ClassManifest[T]): Array[T] = {
    val array = manifest.newArray(count)
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

/** Conatins data implementations and allocators. */
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
    * @return the zero-filled data.
    */
  @inline def alloc[T](count: Long)(implicit allocator: Allocator, unit: Struct[T]): Data =
    allocator.alloc[T](count)
  
  /** Stores a sequence of values to newly allocated data.
    * Allocates `struct.size * count` bytes of data. This convenience function
    * delegates to the implicitly scoped allocator.
    * 
    * @tparam T           the struct type.
    * @param  values      the sequence to store.
    * @param  allocator   the implicit allocator to delegate to.
    * @param  struct      the implicit struct.
    * @return the initialized data.
    */
  @inline def apply[T](values: T*)(implicit allocator: Allocator, struct: Struct[T]): Data =
    allocator[T](values: _*)
  
  /** Stores a sequence of generated values to newly allocated data.
    * Allocates `struct.size * count` bytes of data. This convenience function
    * delegates to the implicitly scoped allocator.
    * 
    * @tparam T           the struct type.
    * @param  count       the number of values in the sequence.
    * @param  value       the value generator.
    * @param  allocator   the implicit allocator to delegate to.
    * @param  struct      the implicit struct.
    * @return the initialized data.
    */
  @inline def fill[@specialized T](count: Long)(value: => T)(implicit allocator: Allocator, struct: Struct[T]): Data =
    allocator.fill[T](count)(value)
  
  /** Stores a sequence of iterated function values to newly allocated data.
    * Allocates `struct.size * count` bytes of data. This convenience function
    * delegates to the implicitly scoped allocator.
    * 
    * @tparam T           the struct type.
    * @param  start       the initial value of the sequence.
    * @param  count       the number of values in the sequence.
    * @param  f           the iteratively applied function.
    * @param  allocator   the implicit allocator to delegate to.
    * @param  struct      the implicit struct.
    * @return the initialized data.
    */
  @inline def iterate[@specialized T](start: T, count: Long)(f: T => T)(implicit allocator: Allocator, struct: Struct[T]): Data =
    allocator.iterate[T](start, count)(f)
  
  /** Stores a sequence of enumerated function values to newly allocated data.
    * Applies sequential `Long` values to the enumerator function starting from 0.
    * Allocates `struct.size * count` bytes of data. This convenience function
    * delegates to the implicitly scoped allocator.
    * 
    * @tparam T           the struct type.
    * @param  count       the number of values in the sequence.
    * @param  f           the enumerator function.
    * @param  allocator   the implicit allocator to delegate to.
    * @param  struct      the implicit struct.
    * @return the initialized data.
    */
  @inline def tabulate[@specialized T](count: Long)(f: Long => T)(implicit allocator: Allocator, struct: Struct[T]): Data =
    allocator.tabulate[T](count)(f)
  
  /** Returns native-endian data backed by the given `Byte` array. */
  def wrap(array: Array[Byte]): Data = NativeEndian match {
    case BigEndian => new Block1BE(array)
    case LittleEndian => new Block1LE(array)
  }
  
  /** Returns native-endian data backed by the given `Short` array. */
  def wrap(array: Array[Short]): Data = NativeEndian match {
    case BigEndian => new Block2BE(array)
    case LittleEndian => new Block2LE(array)
  }
  
  /** Returns native-endian data backed by the given `Int` array. */
  def wrap(array: Array[Int]): Data = NativeEndian match {
    case BigEndian => new Block4BE(array)
    case LittleEndian => new Block4LE(array)
  }
  
  /** Returns native-endian data backed by the given `Long` array. */
  def wrap(array: Array[Long]): Data = NativeEndian match {
    case BigEndian => new Block8BE(array)
    case LittleEndian => new Block8LE(array)
  }
  
  /** Returns native-endian data backed by the given `ByteBuffer`. */
  def wrap(buffer: ByteBuffer): Data = new Chunk(buffer)
  
  /** The native-endian Block allocator. Allocates data backed by a primitive arrays. */
  val Block: Allocator = NativeEndian match {
    case BigEndian => BlockBE
    case LittleEndian => BlockLE
  }
  
  /** The native-endian Block1 allocator. Allocates data backed by a `Byte` array. */
  val Block1: Allocator = NativeEndian match {
    case BigEndian => Block1BE
    case LittleEndian => Block1LE
  }
  
  /** The native-endian Block2 allocator. Allocates data backed by a `Short` array. */
  val Block2: Allocator = NativeEndian match {
    case BigEndian => Block2BE
    case LittleEndian => Block2LE
  }
  
  /** The native-endian Block4 allocator. Allocates data backed by an `Int` array. */
  val Block4: Allocator = NativeEndian match {
    case BigEndian => Block4BE
    case LittleEndian => Block4LE
  }
  
  /** The native-endian Block8 allocator. Allocates data backed by a `Long` array. */
  val Block8: Allocator = NativeEndian match {
    case BigEndian => Block8BE
    case LittleEndian => Block8LE
  }
  
  /** The native-endian Chunk allocator. Allocates data backed by a `ByteBuffer`. */
  val Chunk: Allocator = NativeEndian match {
    case BigEndian => ChunkBE
    case LittleEndian => ChunkLE
  }
  
  /** The big-endian Block allocator. Allocates data backed by a primitive array. */
  object BlockBE extends Allocator {
    def MaxSize: Long = Int.MaxValue.toLong << 3
    
    def alloc[T](count: Long)(implicit unit: Struct[T]): Data = {
      val size = unit.size * count
      if (size <= Int.MaxValue.toLong) unit.alignment match {
        case 1L => new Block1BE(size)
        case 2L => new Block2BE(size)
        case 4L => new Block4BE(size)
        case _  => new Block8BE(size)
      }
      else if (size <= (Int.MaxValue.toLong << 1)) unit.alignment match {
        case 1L | 2L => new Block2BE(size)
        case 4L => new Block4BE(size)
        case _  => new Block8BE(size)
      }
      else if (size <= (Int.MaxValue.toLong << 2)) unit.alignment match {
        case 1L | 2L | 4L => new Block4BE(size)
        case _ => new Block8BE(size)
      }
      else new Block8BE(size)
    }
    
    override def toString: String = "BlockBE"
  }
  
  /** The little-endian Block allocator. Allocates data backed by a primitive array. */
  object BlockLE extends Allocator {
    def MaxSize: Long = Int.MaxValue << 3
    
   def alloc[T](count: Long)(implicit unit: Struct[T]): Data = {
      val size = unit.size * count
      if (size <= Int.MaxValue.toLong) unit.alignment match {
        case 1L => new Block1LE(size)
        case 2L => new Block2LE(size)
        case 4L => new Block4LE(size)
        case _  => new Block8LE(size)
      }
      else if (size <= (Int.MaxValue.toLong << 1)) unit.alignment match {
        case 1L | 2L => new Block2LE(size)
        case 4L => new Block4LE(size)
        case _  => new Block8LE(size)
      }
      else if (size <= (Int.MaxValue.toLong << 2)) unit.alignment match {
        case 1L | 2L | 4L => new Block4LE(size)
        case _ => new Block8LE(size)
      }
      else new Block8LE(size)
    }
    
    override def toString: String = "BlockLE"
  }
  
  /** Big-endian `Byte` array backed data. */
  final class Block1BE(val array: Array[Byte]) extends Data {
    def this(size: Long) = this {
      require(0L <= size && size <= Int.MaxValue.toLong)
      new Array[Byte](size.toInt)
    }
    
    def size: Long = array.length.toLong
    
    def unit: Int = 1
    
    def endian: BigEndian.type = BigEndian
    
    def copy(size: Long): Block1BE = {
      val data = new Block1BE(size)
      data.transfer(0L, this, 0L, math.min(this.size, data.size))
      data
    }
    
    def loadByte(address: Long): Byte = {
      val i = address.toInt
      array(i)
    }
    
    def storeByte(address: Long, value: Byte): Unit = {
      val i = address.toInt
      array(i) = value
    }
    
    override def loadUnalignedShort(address: Long): Short = {
      val i = address.toInt
      ((array(i)             << 8) |
       (array(i + 1) & 0xFF)).toShort
    }
    
    override def storeUnalignedShort(address: Long, value: Short) {
      val i = address.toInt
      array(i)     = (value >> 8).toByte
      array(i + 1) =  value.toByte
    }
    
    override def loadUnalignedInt(address: Long): Int = {
      val i = address.toInt
       (array(i)             << 24) |
      ((array(i + 1) & 0xFF) << 16) |
      ((array(i + 2) & 0xFF) <<  8) |
       (array(i + 3) & 0xFF)
    }
    
    override def storeUnalignedInt(address: Long, value: Int) {
      val i = address.toInt
      array(i)     = (value >> 24).toByte
      array(i + 1) = (value >> 16).toByte
      array(i + 2) = (value >>  8).toByte
      array(i + 3) =  value.toByte
    }
    
    override def loadUnalignedLong(address: Long): Long = {
      val i = address.toInt
       (array(i).toLong             << 56) |
      ((array(i + 1) & 0xFF).toLong << 48) |
      ((array(i + 2) & 0xFF).toLong << 40) |
      ((array(i + 3) & 0xFF).toLong << 32) |
      ((array(i + 4) & 0xFF).toLong << 24) |
      ((array(i + 5) & 0xFF).toLong << 16) |
      ((array(i + 6) & 0xFF).toLong <<  8) |
       (array(i + 7) & 0xFF).toLong
    }
    
    override def storeUnalignedLong(address: Long, value: Long) {
      val i = address.toInt
      array(i)     = (value >> 56).toByte
      array(i + 1) = (value >> 48).toByte
      array(i + 2) = (value >> 40).toByte
      array(i + 3) = (value >> 32).toByte
      array(i + 4) = (value >> 24).toByte
      array(i + 5) = (value >> 16).toByte
      array(i + 6) = (value >>  8).toByte
      array(i + 7) =  value.toByte
    }
    
    override def toString: String =
      "Block1BE"+"("+ size +")"
  }
  
  /** The big-endian Block1 allocator. Allocates data backed by a `Byte` array. */
  object Block1BE extends Allocator {
    def MaxSize: Long = Int.MaxValue.toLong
    
    def alloc[T](count: Long)(implicit unit: Struct[T]): Block1BE =
      new Block1BE(unit.size * count)
    
    def unapply(data: Block1BE): Some[Array[Byte]] = Some(data.array)
    
    override def toString: String = "Block1BE"
  }
  
  /** Little-endian `Byte` array backed data. */
  final class Block1LE(val array: Array[Byte]) extends Data {
    def this(size: Long) = this {
      require(0L <= size && size <= Int.MaxValue.toLong)
      new Array[Byte](size.toInt)
    }
    
    def size: Long = array.length.toLong
    
    def unit: Int = 1
    
    def endian: LittleEndian.type = LittleEndian
    
    def copy(size: Long): Block1LE = {
      val data = new Block1LE(size)
      data.transfer(0L, this, 0L, math.min(this.size, data.size))
      data
    }
    
    def loadByte(address: Long): Byte = {
      val i = address.toInt
      array(i)
    }
    
    def storeByte(address: Long, value: Byte) {
      val i = address.toInt
      array(i) = value
    }
    
    override def loadUnalignedShort(address: Long): Short = {
      val i = address.toInt
      ((array(i)     & 0xFF)       |
       (array(i + 1)         << 8)).toShort
    }
    
    override def storeUnalignedShort(address: Long, value: Short) {
      val i = address.toInt
      array(i)     =  value.toByte
      array(i + 1) = (value >> 8).toByte
    }
    
    override def loadUnalignedInt(address: Long): Int = {
      val i = address.toInt
       (array(i)     & 0xFF)        |
      ((array(i + 1) & 0xFF) <<  8) |
      ((array(i + 2) & 0xFF) << 16) |
       (array(i + 3)         << 24)
    }
    
    override def storeUnalignedInt(address: Long, value: Int) {
      val i = address.toInt
      array(i)     =  value.toByte
      array(i + 1) = (value >>  8).toByte
      array(i + 2) = (value >> 16).toByte
      array(i + 3) = (value >> 24).toByte
    }
    
    override def loadUnalignedLong(address: Long): Long = {
      val i = address.toInt
       (array(i)     & 0xFF).toLong        |
      ((array(i + 1) & 0xFF).toLong <<  8) |
      ((array(i + 2) & 0xFF).toLong << 16) |
      ((array(i + 3) & 0xFF).toLong << 24) |
      ((array(i + 4) & 0xFF).toLong << 32) |
      ((array(i + 5) & 0xFF).toLong << 40) |
      ((array(i + 6) & 0xFF).toLong << 48) |
       (array(i + 7).toLong         << 56)
    }
    
    override def storeUnalignedLong(address: Long, value: Long) {
      val i = address.toInt
      array(i)     =  value.toByte
      array(i + 1) = (value >>  8).toByte
      array(i + 2) = (value >> 16).toByte
      array(i + 3) = (value >> 24).toByte
      array(i + 4) = (value >> 32).toByte
      array(i + 5) = (value >> 40).toByte
      array(i + 6) = (value >> 48).toByte
      array(i + 7) = (value >> 56).toByte
    }
    
    override def toString: String =
      "Block1LE"+"("+ size +")"
  }
  
  /** The little-endian Block1 allocator. Allocates data backed by a `Byte` array. */
  object Block1LE extends Allocator {
    def MaxSize: Long = Int.MaxValue.toLong
    
    def alloc[T](count: Long)(implicit unit: Struct[T]): Block1LE =
      new Block1LE(unit.size * count)
    
    def unapply(data: Block1LE): Some[Array[Byte]] = Some(data.array)
    
    override def toString: String = "Block1LE"
  }
  
  /** Big-endian `Short` array backed data. */
  final class Block2BE(val array: Array[Short]) extends Data {
    def this(size: Long) = this {
      require(0L <= size && size <= (Int.MaxValue.toLong << 1))
      new Array[Short]((align(2L)(size) >> 1).toInt)
    }
    
    def size: Long = array.length.toLong << 1
    
    def unit: Int = 2
    
    def endian: BigEndian.type = BigEndian
    
    def copy(size: Long): Block2BE = {
      val data = new Block2BE(size)
      data.transfer(0L, this, 0L, math.min(this.size, data.size))
      data
    }
    
    def loadByte(address: Long): Byte = {
      val i = (address >> 1).toInt
      val j = ((address.toInt & 1) ^ 1) << 3
      (array(i) >>> j).toByte
    }
    
    def storeByte(address: Long, value: Byte) {
      val i = (address >> 1).toInt
      val j = ((address.toInt & 1) ^ 1) << 3
      array(i) = ((array(i) & ~(0xFF << j)) | ((value & 0xFF) << j)).toShort
    }
    
    override def loadShort(address: Long): Short = {
      val i = (address >> 1).toInt
      array(i)
    }
    
    override def storeShort(address: Long, value: Short) {
      val i = (address >> 1).toInt
      array(i) = value
    }
    
    override def loadInt(address: Long): Int = {
      val i = (address >> 1).toInt & ~1
      (array(i)               << 16) |
      (array(i + 1) & 0xFFFF)
    }
    
    override def storeInt(address: Long, value: Int) {
      val i = (address >> 1).toInt & ~1
      array(i) =     (value >>> 16).toShort
      array(i + 1) = value.toShort
    }
    
    override def loadLong(address: Long): Long = {
      val i = (address >> 1).toInt & ~3
       (array(i).toLong               << 48) |
      ((array(i + 1) & 0xFFFF).toLong << 32) |
      ((array(i + 2) & 0xFFFF).toLong << 16) |
       (array(i + 3) & 0xFFFF).toLong
    }
    
    override def storeLong(address: Long, value: Long) {
      val i = (address >> 1).toInt & ~3
      array(i) =     (value >>> 48).toShort
      array(i + 1) = (value >>> 32).toShort
      array(i + 2) = (value >>> 16).toShort
      array(i + 3) = value.toShort
    }
    
    override def toString: String =
      "Block2BE"+"("+ size +")"
  }
  
  /** The big-endian Block2 allocator. Allocates data backed by a `Short` array. */
  object Block2BE extends Allocator {
    def MaxSize: Long = Int.MaxValue.toLong << 1
    
    def alloc[T](count: Long)(implicit unit: Struct[T]): Block2BE =
      new Block2BE(unit.size * count)
    
    def unapply(data: Block2BE): Some[Array[Short]] = Some(data.array)
    
    override def toString: String = "Block2BE"
  }
  
  /** Little-endian `Short` array backed data. */
  final class Block2LE(val array: Array[Short]) extends Data {
    def this(size: Long) = this {
      require(0L <= size && size <= (Int.MaxValue.toLong << 1))
      new Array[Short]((align(2L)(size) >> 1).toInt)
    }
    
    def size: Long = array.length.toLong << 1
    
    def unit: Int = 2
    
    def endian: LittleEndian.type = LittleEndian
    
    def copy(size: Long): Block2LE = {
      val data = new Block2LE(size)
      data.transfer(0L, this, 0L, math.min(this.size, data.size))
      data
    }
    
    def loadByte(address: Long): Byte = {
      val i = (address >> 1).toInt
      val j = (address.toInt & 1) << 3
      (array(i) >>> j).toByte
    }
    
    def storeByte(address: Long, value: Byte) {
      val i = (address >> 1).toInt
      val j = (address.toInt & 1) << 3
      array(i) = ((array(i) & ~(0xFF << j)) | ((value & 0xFF) << j)).toShort
    }
    
    override def loadShort(address: Long): Short = {
      val i = (address >> 1).toInt
      array(i)
    }
    
    override def storeShort(address: Long, value: Short) {
      val i = (address >> 1).toInt
      array(i) = value
    }
    
    override def loadInt(address: Long): Int = {
      val i = (address >> 1).toInt & ~1
      (array(i)     & 0xFFFF)        |
      (array(i + 1)           << 16)
    }
    
    override def storeInt(address: Long, value: Int) {
      val i = (address >> 1).toInt & ~1
      array(i)     = value.toShort
      array(i + 1) = (value >>> 16).toShort
    }
    
    override def loadLong(address: Long): Long = {
      val i = (address >> 1).toInt & ~3
       (array(i)     & 0xFFFF).toLong        |
      ((array(i + 1) & 0xFFFF).toLong << 16) |
      ((array(i + 2) & 0xFFFF).toLong << 32) |
       (array(i + 3).toLong           << 48)
    }
    
    override def storeLong(address: Long, value: Long) {
      val i = (address >> 1).toInt & ~3
      array(i) =     value.toShort
      array(i + 1) = (value >>> 16).toShort
      array(i + 2) = (value >>> 32).toShort
      array(i + 3) = (value >>> 48).toShort
    }
    
    override def toString: String =
      "Block2LE"+"("+ size +")"
  }
  
  /** The little-endian Block2 allocator. Allocates data backed by a `Short` array. */
  object Block2LE extends Allocator {
    def MaxSize: Long = Int.MaxValue.toLong << 1
    
    def alloc[T](count: Long)(implicit unit: Struct[T]): Block2LE =
      new Block2LE(unit.size * count)
    
    def unapply(data: Block2LE): Some[Array[Short]] = Some(data.array)
    
    override def toString: String = "Block2LE"
  }
  
  /** Big-endian `Int` array backed data. */
  final class Block4BE(val array: Array[Int]) extends Data {
    def this(size: Long) = this {
      require(0L <= size && size <= (Int.MaxValue.toLong << 2))
      new Array[Int]((align(4L)(size) >> 2).toInt)
    }
    
    def size: Long = array.length.toLong << 2
    
    def unit: Int = 4
    
    def endian: BigEndian.type = BigEndian
    
    def copy(size: Long): Block4BE = {
      val data = new Block4BE(size)
      data.transfer(0L, this, 0L, math.min(this.size, data.size))
      data
    }
    
    def loadByte(address: Long): Byte = {
      val i = (address >> 2).toInt
      val j = ((address.toInt & 3) ^ 3) << 3
      (array(i) >>> j).toByte
    }
    
    def storeByte(address: Long, value: Byte) {
      val i = (address >> 2).toInt
      val j = ((address.toInt & 3) ^ 3) << 3
      array(i) = (array(i) & ~(0xFF << j)) | ((value & 0xFF) << j)
    }
    
    override def loadShort(address: Long): Short = {
      val i = (address >> 2).toInt
      val j = ((address.toInt & 2) ^ 2) << 3
      (array(i) >>> j).toShort
    }
    
    override def storeShort(address: Long, value: Short) {
      val i = (address >> 2).toInt
      val j = ((address.toInt & 2) ^ 2) << 3
      array(i) = (array(i) & ~(0xFFFF << j)) | ((value & 0xFFFF) << j)
    }
    
    override def loadInt(address: Long): Int = {
      val i = (address >> 2).toInt
      array(i)
    }
    
    override def storeInt(address: Long, value: Int) {
      val i = (address >> 2).toInt
      array(i) = value
    }
    
    override def loadLong(address: Long): Long = {
      val i = (address >> 2).toInt & ~1
      (array(i).toLong                    << 32) |
      (array(i + 1).toLong & 0xFFFFFFFFL)
    }
    
    override def storeLong(address: Long, value: Long) {
      val i = (address >> 2).toInt & ~1
      array(i)     = (value >>> 32).toInt
      array(i + 1) = value.toInt
    }
    
    override def toString: String =
      "Block4BE"+"("+ size +")"
  }
  
  /** The big-endian Block4 allocator. Allocates data backed by an `Int` array. */
  object Block4BE extends Allocator {
    def MaxSize: Long = Int.MaxValue.toLong << 2
    
    def alloc[T](count: Long)(implicit unit: Struct[T]): Block4BE =
      new Block4BE(unit.size * count)
    
    def unapply(data: Block4BE): Some[Array[Int]] = Some(data.array)
    
    override def toString: String = "Block4BE"
  }
  
  /** Little-endian `Int` array backed data. */
  final class Block4LE(val array: Array[Int]) extends Data {
    def this(size: Long) = this {
      require(0L <= size && size <= (Int.MaxValue.toLong << 2))
      new Array[Int]((align(4L)(size) >> 2).toInt)
    }
    
    def size: Long = array.length.toLong << 2
    
    def unit: Int = 4
    
    def endian: LittleEndian.type = LittleEndian
    
    def copy(size: Long): Block4LE = {
      val data = new Block4LE(size)
      data.transfer(0L, this, 0L, math.min(this.size, data.size))
      data
    }
    
    def loadByte(address: Long): Byte = {
      val i = (address >> 2).toInt
      val j = (address.toInt & 3) << 3
      (array(i) >>> j).toByte
    }
    
    def storeByte(address: Long, value: Byte) {
      val i = (address >> 2).toInt
      val j = (address.toInt & 3) << 3
      array(i) = (array(i) & ~(0xFF << j)) | ((value & 0xFF) << j)
    }
    
    override def loadShort(address: Long): Short = {
      val i = (address >> 2).toInt
      val j = (address.toInt & 2) << 3
      (array(i) >>> j).toShort
    }
    
    override def storeShort(address: Long, value: Short) {
      val i = (address >> 2).toInt
      val j = (address.toInt & 2) << 3
      array(i) = (array(i) & ~(0xFFFF << j)) | ((value & 0xFFFF) << j)
    }
    
    override def loadInt(address: Long): Int = {
      val i = (address >> 2).toInt
      array(i)
    }
    
    override def storeInt(address: Long, value: Int) {
      val i = (address >> 2).toInt
      array(i) = value
    }
    
    override def loadLong(address: Long): Long = {
      val i = (address >> 2).toInt & ~1
      (array(i).toLong     & 0xFFFFFFFFL)        |
      (array(i + 1).toLong                << 32)
    }
    
    override def storeLong(address: Long, value: Long) {
      val i = (address >> 2).toInt & ~1
      array(i) = value.toInt
      array(i + 1) = (value >>> 32).toInt
    }
    
    override def toString: String =
      "Block4LE"+"("+ size +")"
  }
  
  /** The little-endian Block4 allocator. Allocates data backed by an `Int` array. */
  object Block4LE extends Allocator {
    def MaxSize: Long = Int.MaxValue.toLong << 2
    
    def alloc[T](count: Long)(implicit unit: Struct[T]): Block4LE =
      new Block4LE(unit.size * count)
    
    def unapply(data: Block4LE): Some[Array[Int]] = Some(data.array)
    
    override def toString: String = "Block4LE"
  }
  
  /** Big-endian `Long` array backed data. */
  final class Block8BE(val array: Array[Long]) extends Data {
    def this(size: Long) = this {
      require(0L <= size && size <= (Int.MaxValue.toLong << 3))
      new Array[Long]((align(8L)(size) >> 3).toInt)
    }
    
    def size: Long = array.length.toLong << 3
    
    def unit: Int = 8
    
    def endian: BigEndian.type = BigEndian
    
    def copy(size: Long): Block8BE = {
      val data = new Block8BE(size)
      data.transfer(0L, this, 0L, math.min(this.size, data.size))
      data
    }
    
    def loadByte(address: Long): Byte = {
      val i = (address >> 3).toInt
      val j = ((address.toInt & 7) ^ 7) << 3
      (array(i) >>> j).toByte
    }
    
    def storeByte(address: Long, value: Byte) {
      val i = (address >> 3).toInt
      val j = ((address.toInt & 7) ^ 7) << 3
      array(i) = (array(i) & ~(0xFFL << j)) | ((value & 0xFF).toLong << j)
    }
    
    override def loadShort(address: Long): Short = {
      val i = (address >> 3).toInt
      val j = ((address.toInt & 6) ^ 6) << 3
      (array(i) >>> j).toShort
    }
    
    override def storeShort(address: Long, value: Short) {
      val i = (address >> 3).toInt
      val j = ((address.toInt & 6) ^ 6) << 3
      array(i) = (array(i) & ~(0xFFFFL << j)) | ((value & 0xFFFF).toLong << j)
    }
    
    override def loadInt(address: Long): Int = {
      val i = (address >> 3).toInt
      val j = ((address.toInt & 4) ^ 4) << 3
      (array(i) >> j).toInt
    }
    
    override def storeInt(address: Long, value: Int) {
      val i = (address >> 3).toInt
      val j = ((address.toInt & 4) ^ 4) << 3
      array(i) = (array(i) & ~(0xFFFFFFFFL << j)) | ((value & 0xFFFFFFFFL) << j)
    }
    
    override def loadLong(address: Long): Long = {
      val i = (address >> 3).toInt
      array(i)
    }
    
    override def storeLong(address: Long, value: Long) {
      val i = (address >> 3).toInt
      array(i) = value
    }
    
    override def toString: String =
      "Block8BE"+"("+ size +")"
  }
  
  /** The big-endian Block8 allocator. Allocates data backed by a `Long` array. */
  object Block8BE extends Allocator {
    def MaxSize: Long = Int.MaxValue.toLong << 3
    
    def alloc[T](count: Long)(implicit unit: Struct[T]): Block8BE =
      new Block8BE(unit.size * count)
    
    def unapply(data: Block8BE): Some[Array[Long]] = Some(data.array)
    
    override def toString: String = "Block8BE"
  }
  
  /** Little-endian `Long` array backed data. */
  final class Block8LE(val array: Array[Long]) extends Data {
    def this(size: Long) = this {
      require(0L <= size && size <= (Int.MaxValue.toLong << 3))
      new Array[Long]((align(8L)(size) >> 3).toInt)
    }
    
    def size: Long = array.length.toLong << 3
    
    def unit: Int = 8
    
    def endian: LittleEndian.type = LittleEndian
    
    def copy(size: Long): Block8LE = {
      val data = new Block8LE(size)
      data.transfer(0L, this, 0L, math.min(this.size, data.size))
      data
    }
    
    def loadByte(address: Long): Byte = {
      val i = (address >> 3).toInt
      val j = (address.toInt & 7) << 3
      (array(i) >>> j).toByte
    }
    
    def storeByte(address: Long, value: Byte) {
      val i = (address >> 3).toInt
      val j = (address.toInt & 7) << 3
      array(i) = (array(i) & ~(0xFFL << j)) | ((value & 0xFF).toLong << j)
    }
    
    override def loadShort(address: Long): Short = {
      val i = (address >> 3).toInt
      val j = (address.toInt & 6) << 3
      (array(i) >>> j).toShort
    }
    
    override def storeShort(address: Long, value: Short) {
      val i = (address >> 3).toInt
      val j = (address.toInt & 6) << 3
      array(i) = (array(i) & ~(0xFFFFL << j)) | ((value & 0xFFFF).toLong << j)
    }
    
    override def loadInt(address: Long): Int = {
      val i = (address >> 3).toInt
      val j = (address.toInt & 4) << 3
      (array(i) >>> j).toInt
    }
    
    override def storeInt(address: Long, value: Int) {
      val i = (address >> 3).toInt
      val j = (address.toInt & 4) << 3
      array(i) = (array(i) & ~(0xFFFFFFFFL << j)) | ((value & 0xFFFFFFFFL) << j)
    }
    
    override def loadLong(address: Long): Long = {
      val i = (address >> 3).toInt
      array(i)
    }
    
    override def storeLong(address: Long, value: Long) {
      val i = (address >> 3).toInt
      array(i) = value
    }
    
    override def toString: String =
      "Block8LE"+"("+ size +")"
  }
  
  /** The little-endian Block8 allocator. Allocates data backed by a `Long` array. */
  object Block8LE extends Allocator {
    def MaxSize: Long = Int.MaxValue.toLong << 3
    
    def alloc[T](count: Long)(implicit unit: Struct[T]): Block8LE =
      new Block8LE(unit.size * count)
    
    def unapply(data: Block8LE): Some[Array[Long]] = Some(data.array)
    
    override def toString: String = "Block8LE"
  }
  
  /** `ByteBuffer` backed Data. */
  final class Chunk(val buffer: ByteBuffer) extends Data {
    def this(size: Long)(implicit endian: Endianness) = this {
      require(0L <= size && size <= Int.MaxValue.toLong)
      val byteOrder = endian match {
        case BigEndian => ByteOrder.BIG_ENDIAN
        case LittleEndian => ByteOrder.LITTLE_ENDIAN
      }
      ByteBuffer.allocateDirect(size.toInt).order(byteOrder)
    }
    
    def size: Long = buffer.capacity.toLong
    
    def unit: Int = 1
    
    def endian: Endianness = buffer.order match {
      case ByteOrder.BIG_ENDIAN => BigEndian
      case ByteOrder.LITTLE_ENDIAN => LittleEndian
    }
    
    def copy(size: Long): Chunk = {
      val data = new Chunk(size)(endian)
      data.transfer(0L, this, 0L, math.min(this.size, data.size))
      data
    }
    
    def loadByte(address: Long): Byte =
      buffer.get(address.toInt)
    
    def storeByte(address: Long, value: Byte): Unit =
      buffer.put(address.toInt, value)
    
    override def loadUnalignedShort(address: Long): Short =
      buffer.getShort(address.toInt)
    
    override def storeUnalignedShort(address: Long, value: Short): Unit =
      buffer.putShort(address.toInt, value)
    
    override def loadUnalignedInt(address: Long): Int =
      buffer.getInt(address.toInt)
    
    override def storeUnalignedInt(address: Long, value: Int): Unit =
      buffer.putInt(address.toInt, value)
    
    override def loadUnalignedLong(address: Long): Long =
      buffer.getLong(address.toInt)
    
    override def storeUnalignedLong(address: Long, value: Long): Unit =
      buffer.putLong(address.toInt, value)
    
    override def loadUnalignedChar(address: Long): Char =
      buffer.getChar(address.toInt)
    
    override def storeUnalignedChar(address: Long, value: Char): Unit =
      buffer.putChar(address.toInt, value)
    
    override def loadUnalignedFloat(address: Long): Float =
      buffer.getFloat(address.toInt)
    
    override def storeUnalignedFloat(address: Long, value: Float): Unit =
      buffer.putFloat(address.toInt, value)
    
    override def loadUnalignedDouble(address: Long): Double =
      buffer.getDouble(address.toInt)
    
    override def storeUnalignedDouble(address: Long, value: Double): Unit =
      buffer.putDouble(address.toInt, value)
    
    override def toString: String = endian match {
      case BigEndian => "ChunkBE"+"("+ size +")"
      case LittleEndian => "ChunkLE"+"("+ size +")"
    }
  }
  
  /** The big-endian Chunk allocator. Allocates data backed by a `ByteBuffer`. */
  object ChunkBE extends Allocator {
    def MaxSize: Long = Int.MaxValue.toLong
    
    def alloc[T](count: Long)(implicit unit: Struct[T]): Chunk =
      new Chunk(unit.size * count)(BigEndian)
    
    def unapply(data: Chunk): Option[ByteBuffer] = data.endian match {
      case BigEndian => Some(data.buffer)
      case _ => None
    }
    
    override def toString: String = "ChunkBE"
  }
  
  /** The little-endian Chunk allocator. Allocates data backed by a `ByteBuffer`. */
  object ChunkLE extends Allocator {
    def MaxSize: Long = Int.MaxValue.toLong
    
    def alloc[T](count: Long)(implicit unit: Struct[T]): Chunk =
      new Chunk(unit.size * count)(LittleEndian)
    
    def unapply(data: Chunk): Option[ByteBuffer] = data.endian match {
      case LittleEndian => Some(data.buffer)
      case _ => None
    }
    
    override def toString: String = "ChunkLE"
  }
}
