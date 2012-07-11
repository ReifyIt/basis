/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}

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
  * data: basis.memory.Data = Base4LE(4) // Data class will vary by architecture.
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
  /** Returns the number of addressable bytes in the address space. */
  def size: Long
  
  /** Returns the internal word size. */
  def unit: Int
  
  /** Returns the data's byte order. */
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
    * @tparam T         the instance type to load.
    * @param  address   the aligned address to load.
    * @param  struct    the implicit value type.
    * @return the loaded instance.
    */
  final def load[@specialized T](address: Long)(implicit struct: ValType[T]): T =
    struct.load(this, address)
  
  /** Stores an instance as a data value.
    * 
    * @tparam T         the instance type to store.
    * @param  address   the aligned storage address.
    * @param  value     the instance to store.
    * @param  struct    the implicit value type to store.
    */
  final def store[@specialized T](address: Long, value: T)(implicit struct: ValType[T]): Unit =
    struct.store(this, address, value)
  
  /** Loads a sequence of data values as a new instance array.
    * 
    * @tparam T         the instance type to load.
    * @param  address   the aligned address to load.
    * @param  count     the number of values to load.
    * @param  struct    the implicit value type to load.
    * @param  classTag  the reflective type of the array to load.
    * @return the loaded array of Scala values.
    */
  def loadArray[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean, AnyRef) T]
      (address: Long, count: Int)
      (implicit struct: ValType[T], classTag: ClassTag[T]): Array[T] = {
    val array = classTag.newArray(count)
    copyToArray[T](address, array, 0, count)
    array
  }
  
  /** Copies a sequence of data values to an instance array slice.
    * 
    * @tparam T         the instance type to load.
    * @param  address   the aligned address to load.
    * @param  array     the array to copy to.
    * @param  start     the lower bound of the array slice to copy to.
    * @param  count     the number of values to copy.
    * @param  struct    the implicit value type to load.
    */
  def copyToArray[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean, AnyRef) T]
      (address: Long, array: Array[T], start: Int, count: Int)
      (implicit struct: ValType[T]) {
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
    * @tparam T         the instance type to store.
    * @param  address   the aligned storage address.
    * @param  array     the array to store from.
    * @param  start     the lower bound of the array slice to store from.
    * @param  count     the number of values to store.
    * @param  struct    the implicit value type to store.
    */
  def storeArray[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean, AnyRef) T]
      (address: Long, array: Array[T], start: Int, count: Int)
      (implicit struct: ValType[T]) {
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
  @inline def alloc[T](count: Long)(implicit allocator: Allocator, unit: ValType[T]): Data =
    allocator.alloc[T](count)
  
  /** Allocates a number of bytes of data. This convenience function delegates
    * to the implicitly scoped allocator.
    * 
    * @param  size        the number of bytes to allocate.
    * @param  allocator   the implicit allocator to delegate to.
    * @return the allocated zero-filled data.
    */
  @inline def apply(size: Long)(implicit allocator: Allocator): Data = allocator(size)
  
  /** An allocator for native-endian data backed by a primitive array. */
  val Base: Allocator = NativeEndian match {
    case BigEndian    => BaseBE
    case LittleEndian => BaseLE
  }
  
  /** An allocator for native-endian data backed by a `Byte` array. */
  val Base1: BaseAllocator[Byte] = NativeEndian match {
    case BigEndian    => Base1BE
    case LittleEndian => Base1LE
  }
  
  /** An allocator for native-endian data backed by a `Short` array. */
  val Base2: BaseAllocator[Short] = NativeEndian match {
    case BigEndian    => Base2BE
    case LittleEndian => Base2LE
  }
  
  /** An allocator for native-endian data backed by an `Int` array. */
  val Base4: BaseAllocator[Int] = NativeEndian match {
    case BigEndian    => Base4BE
    case LittleEndian => Base4LE
  }
  
  /** An allocator for native-endian data backed by a `Long` array. */
  val Base8: BaseAllocator[Long] = NativeEndian match {
    case BigEndian    => Base8BE
    case LittleEndian => Base8LE
  }
  
  /** Data backed by an array.
    * 
    * @tparam V   the element type of the backing array.
    */
  trait Base[V] extends Any with Data {
    /** The backing array. */
    def array: Array[V]
    override def copy(size: Long): Base[V]
  }
  
  /** An allocator for `Data` backed by an array.
    * 
    * @tparam V   the element type of allocated array data.
    */
  trait BaseAllocator[V] extends Allocator {
    override def alloc[T](count: Long)(implicit unit: ValType[T]): Base[V]
    override def apply(size: Long): Base[V]
    /** Returns a data object backed by the given array. */
    def wrap(array: Array[V]): Base[V]
  }
  
  /** An allocator for big-endian data backed by a primitive array. */
  object BaseBE extends Allocator {
    override def MaxSize: Long = Int.MaxValue.toLong << 3
    override def alloc[T](count: Long)(implicit unit: ValType[T]): Data = {
      val size = unit.size * count
      if (size <= Int.MaxValue.toLong) unit.alignment match {
        case 1L => Base1BE(size)
        case 2L => Base2BE(size)
        case 4L => Base4BE(size)
        case _  => Base8BE(size)
      }
      else if (size <= (Int.MaxValue.toLong << 1)) unit.alignment match {
        case 1L | 2L => Base2BE(size)
        case 4L      => Base4BE(size)
        case _       => Base8BE(size)
      }
      else if (size <= (Int.MaxValue.toLong << 2)) unit.alignment match {
        case 1L | 2L | 4L => Base4BE(size)
        case _            => Base8BE(size)
      }
      else Base8BE(size)
    }
    override def apply(size: Long): Data = alloc[Byte](size)
    override def toString: String = "BaseBE"
  }
  
  /** An allocator for little-endian data backed by a primitive array. */
  object BaseLE extends Allocator {
    override def MaxSize: Long = Int.MaxValue << 3
    override def alloc[T](count: Long)(implicit unit: ValType[T]): Data = {
      val size = unit.size * count
      if (size <= Int.MaxValue.toLong) unit.alignment match {
        case 1L => Base1LE(size)
        case 2L => Base2LE(size)
        case 4L => Base4LE(size)
        case _  => Base8LE(size)
      }
      else if (size <= (Int.MaxValue.toLong << 1)) unit.alignment match {
        case 1L | 2L => Base2LE(size)
        case 4L      => Base4LE(size)
        case _       => Base8LE(size)
      }
      else if (size <= (Int.MaxValue.toLong << 2)) unit.alignment match {
        case 1L | 2L | 4L => Base4LE(size)
        case _            => Base8LE(size)
      }
      else Base8LE(size)
    }
    override def apply(size: Long): Data = alloc[Byte](size)
    override def toString: String = "BaseLE"
  }
  
  /** Big-endian `Byte` array backed data. */
  final class Base1BE(val array: Array[Byte]) extends AnyVal with Base[Byte] {
    @inline override def size: Long = array.length.toLong
    @inline override def unit: Int = 1
    @inline override def endian = Endianness.BigEndian
    override def copy(size: Long): Base1BE = {
      require(0L <= size && size <= Int.MaxValue.toLong)
      val array = new Array[Byte](size.toInt)
      Array.copy(this.array, 0, array, 0, math.min(this.array.length, array.length))
      new Base1BE(array)
    }
    @inline override def loadByte(address: Long): Byte = array(address.toInt)
    @inline override def storeByte(address: Long, value: Byte): Unit = array(address.toInt) = value
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
    override def toString: String = "Base1BE"+"("+ size +")"
  }
  
  /** An allocator for big-endian data backed by a `Byte` array. */
  object Base1BE extends BaseAllocator[Byte] {
    override def MaxSize: Long = Int.MaxValue.toLong
    override def alloc[T](count: Long)(implicit unit: ValType[T]): Base1BE = apply(unit.size * count)
    override def apply(size: Long): Base1BE = {
      require(0L <= size && size <= MaxSize)
      val array = new Array[Byte](size.toInt)
      new Base1BE(array)
    }
    def unapply(data: Base1BE): Some[Array[Byte]] = Some(data.array)
    override def wrap(array: Array[Byte]): Base1BE = new Base1BE(array)
    override def toString: String = "Base1BE"
  }
  
  /** Little-endian `Byte` array backed data. */
  final class Base1LE(val array: Array[Byte]) extends AnyVal with Base[Byte] {
    @inline override def size: Long = array.length.toLong
    @inline override def unit: Int = 1
    @inline override def endian = Endianness.LittleEndian
    override def copy(size: Long): Base1LE = {
      require(0L <= size && size <= Int.MaxValue.toLong)
      val array = new Array[Byte](size.toInt)
      Array.copy(this.array, 0, array, 0, math.min(this.array.length, array.length))
      new Base1LE(array)
    }
    @inline override def loadByte(address: Long): Byte = array(address.toInt)
    @inline override def storeByte(address: Long, value: Byte): Unit = array(address.toInt) = value
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
    override def toString: String = "Base1LE"+"("+ size +")"
  }
  
  /** An allocator for little-endian data backed by a `Byte` array. */
  object Base1LE extends BaseAllocator[Byte] {
    override def MaxSize: Long = Int.MaxValue.toLong
    override def alloc[T](count: Long)(implicit unit: ValType[T]): Base1LE = apply(unit.size * count)
    override def apply(size: Long): Base1LE = {
      require(0L <= size && size <= MaxSize)
      val array = new Array[Byte](size.toInt)
      new Base1LE(array)
    }
    def unapply(data: Base1LE): Some[Array[Byte]] = Some(data.array)
    override def wrap(array: Array[Byte]): Base1LE = new Base1LE(array)
    override def toString: String = "Base1LE"
  }
  
  /** Big-endian `Short` array backed data. Base-2 subaddresses identify bytes within words. */
  final class Base2BE(val array: Array[Short]) extends AnyVal with Base[Short] {
    @inline override def size: Long = array.length.toLong << 1
    @inline override def unit: Int = 2
    @inline override def endian = Endianness.BigEndian
    override def copy(size: Long): Base2BE = {
      require(0L <= size && size <= (Int.MaxValue.toLong << 1))
      val array = new Array[Short]((align(2L)(size) >> 1).toInt)
      Array.copy(this.array, 0, array, 0, math.min(this.array.length, array.length))
      new Base2BE(array)
    }
    override def loadByte(address: Long): Byte = {
      val i = (address >> 1).toInt
      val j = ((address.toInt & 1) ^ 1) << 3
      (array(i) >>> j).toByte
    }
    override def storeByte(address: Long, value: Byte) {
      val i = (address >> 1).toInt
      val j = ((address.toInt & 1) ^ 1) << 3
      array(i) = ((array(i) & ~(0xFF << j)) | ((value & 0xFF) << j)).toShort
    }
    @inline override def loadShort(address: Long): Short = array((address >> 1).toInt)
    @inline override def storeShort(address: Long, value: Short): Unit = array((address >> 1).toInt) = value
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
    override def toString: String = "Base2BE"+"("+ size +")"
  }
  
  /** An allocator for big-endian data backed by a `Short` array. */
  object Base2BE extends BaseAllocator[Short] {
    override def MaxSize: Long = Int.MaxValue.toLong << 1
    override def alloc[T](count: Long)(implicit unit: ValType[T]): Base2BE = apply(unit.size * count)
    override def apply(size: Long): Base2BE = {
      require(0L <= size && size <= MaxSize)
      val array = new Array[Short]((align(2L)(size) >> 1).toInt)
      new Base2BE(array)
    }
    def unapply(data: Base2BE): Some[Array[Short]] = Some(data.array)
    override def wrap(array: Array[Short]): Base2BE = new Base2BE(array)
    override def toString: String = "Base2BE"
  }
  
  /** Little-endian `Short` array backed data. Base-2 subaddresses identify bytes within words. */
  final class Base2LE(val array: Array[Short]) extends AnyVal with Base[Short] {
    @inline override def size: Long = array.length.toLong << 1
    @inline override def unit: Int = 2
    @inline override def endian = Endianness.LittleEndian
    override def copy(size: Long): Base2LE = {
      require(0L <= size && size <= (Int.MaxValue.toLong << 1))
      val array = new Array[Short]((align(2L)(size) >> 1).toInt)
      Array.copy(this.array, 0, array, 0, math.min(this.array.length, array.length))
      new Base2LE(array)
    }
    override def loadByte(address: Long): Byte = {
      val i = (address >> 1).toInt
      val j = (address.toInt & 1) << 3
      (array(i) >>> j).toByte
    }
    override def storeByte(address: Long, value: Byte) {
      val i = (address >> 1).toInt
      val j = (address.toInt & 1) << 3
      array(i) = ((array(i) & ~(0xFF << j)) | ((value & 0xFF) << j)).toShort
    }
    @inline override def loadShort(address: Long): Short = array((address >> 1).toInt)
    @inline override def storeShort(address: Long, value: Short): Unit = array((address >> 1).toInt) = value
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
    override def toString: String = "Base2LE"+"("+ size +")"
  }
  
  /** An allocator for little-endian data backed by a `Short` array. */
  object Base2LE extends BaseAllocator[Short] {
    override def MaxSize: Long = Int.MaxValue.toLong << 1
    override def alloc[T](count: Long)(implicit unit: ValType[T]): Base2LE = apply(unit.size * count)
    override def apply(size: Long): Base2LE = {
      require(0L <= size && size <= MaxSize)
      val array = new Array[Short]((align(2L)(size) >> 1).toInt)
      new Base2LE(array)
    }
    def unapply(data: Base2LE): Some[Array[Short]] = Some(data.array)
    override def wrap(array: Array[Short]): Base2LE = new Base2LE(array)
    override def toString: String = "Base2LE"
  }
  
  /** Big-endian `Int` array backed data. Base-4 subaddresses identify bytes within words. */
  final class Base4BE(val array: Array[Int]) extends AnyVal with Base[Int] {
    @inline override def size: Long = array.length.toLong << 2
    @inline override def unit: Int = 4
    @inline override def endian = Endianness.BigEndian
    override def copy(size: Long): Base4BE = {
      require(0L <= size && size <= (Int.MaxValue.toLong << 2))
      val array = new Array[Int]((align(4L)(size) >> 2).toInt)
      Array.copy(this.array, 0, array, 0, math.min(this.array.length, array.length))
      new Base4BE(array)
    }
    override def loadByte(address: Long): Byte = {
      val i = (address >> 2).toInt
      val j = ((address.toInt & 3) ^ 3) << 3
      (array(i) >>> j).toByte
    }
    override def storeByte(address: Long, value: Byte) {
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
    @inline override def loadInt(address: Long): Int = array((address >> 2).toInt)
    @inline override def storeInt(address: Long, value: Int): Unit = array((address >> 2).toInt) = value
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
    override def toString: String = "Base4BE"+"("+ size +")"
  }
  
  /** An allocator for big-endian data backed by an `Int` array. */
  object Base4BE extends BaseAllocator[Int] {
    override def MaxSize: Long = Int.MaxValue.toLong << 2
    override def alloc[T](count: Long)(implicit unit: ValType[T]): Base4BE = apply(unit.size * count)
    override def apply(size: Long): Base4BE = {
      require(0L <= size && size <= MaxSize)
      val array = new Array[Int]((align(4L)(size) >> 2).toInt)
      new Base4BE(array)
    }
    def unapply(data: Base4BE): Some[Array[Int]] = Some(data.array)
    override def wrap(array: Array[Int]): Base4BE = new Base4BE(array)
    override def toString: String = "Base4BE"
  }
  
  /** Little-endian `Int` array backed data. Base-4 subaddresses identify bytes within words. */
  final class Base4LE(val array: Array[Int]) extends AnyVal with Base[Int] {
    @inline override def size: Long = array.length.toLong << 2
    @inline override def unit: Int = 4
    @inline override def endian = Endianness.LittleEndian
    override def copy(size: Long): Base4LE = {
      require(0L <= size && size <= (Int.MaxValue.toLong << 2))
      val array = new Array[Int]((align(4L)(size) >> 2).toInt)
      Array.copy(this.array, 0, array, 0, math.min(this.array.length, array.length))
      new Base4LE(array)
    }
    override def loadByte(address: Long): Byte = {
      val i = (address >> 2).toInt
      val j = (address.toInt & 3) << 3
      (array(i) >>> j).toByte
    }
    override def storeByte(address: Long, value: Byte) {
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
    @inline override def loadInt(address: Long): Int = array((address >> 2).toInt)
    @inline override def storeInt(address: Long, value: Int): Unit = array((address >> 2).toInt) = value
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
    override def toString: String = "Base4LE"+"("+ size +")"
  }
  
  /** An allocator for little-endian data backed by an `Int` array. */
  object Base4LE extends BaseAllocator[Int] {
    override def MaxSize: Long = Int.MaxValue.toLong << 2
    override def alloc[T](count: Long)(implicit unit: ValType[T]): Base4LE = apply(unit.size * count)
    override def apply(size: Long): Base4LE = {
      require(0L <= size && size <= MaxSize)
      val array = new Array[Int]((align(4L)(size) >> 2).toInt)
      new Base4LE(array)
    }
    def unapply(data: Base4LE): Some[Array[Int]] = Some(data.array)
    override def wrap(array: Array[Int]): Base4LE = new Base4LE(array)
    override def toString: String = "Base4LE"
  }
  
  /** Big-endian `Long` array backed data. Base-8 subaddresses identify bytes within words. */
  final class Base8BE(val array: Array[Long]) extends AnyVal with Base[Long] {
    @inline override def size: Long = array.length.toLong << 3
    @inline override def unit: Int = 8
    @inline override def endian = Endianness.BigEndian
    override def copy(size: Long): Base8BE = {
      require(0L <= size && size <= (Int.MaxValue.toLong << 3))
      val array = new Array[Long]((align(8L)(size) >> 3).toInt)
      Array.copy(this.array, 0, array, 0, math.min(this.array.length, array.length))
      new Base8BE(array)
    }
    override def loadByte(address: Long): Byte = {
      val i = (address >> 3).toInt
      val j = ((address.toInt & 7) ^ 7) << 3
      (array(i) >>> j).toByte
    }
    override def storeByte(address: Long, value: Byte) {
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
    @inline override def loadLong(address: Long): Long = array((address >> 3).toInt)
    @inline override def storeLong(address: Long, value: Long): Unit = array((address >> 3).toInt) = value
    override def toString: String = "Base8BE"+"("+ size +")"
  }
  
  /** An allocator for big-endian data backed by a `Long` array. */
  object Base8BE extends BaseAllocator[Long] {
    override def MaxSize: Long = Int.MaxValue.toLong << 3
    override def alloc[T](count: Long)(implicit unit: ValType[T]): Base8BE = apply(unit.size * count)
    override def apply(size: Long): Base8BE = {
      require(0L <= size && size <= MaxSize)
      val array = new Array[Long]((align(8L)(size) >> 3).toInt)
      new Base8BE(array)
    }
    def unapply(data: Base8BE): Some[Array[Long]] = Some(data.array)
    override def wrap(array: Array[Long]): Base8BE = new Base8BE(array)
    override def toString: String = "Base8BE"
  }
  
  /** Little-endian `Long` array backed data. Base-8 subaddresses identify bytes within words. */
  final class Base8LE(val array: Array[Long]) extends AnyVal with Base[Long] {
    @inline override def size: Long = array.length.toLong << 3
    @inline override def unit: Int = 8
    @inline override def endian = Endianness.LittleEndian
    override def copy(size: Long): Base8LE = {
      require(0L <= size && size <= (Int.MaxValue.toLong << 3))
      val array = new Array[Long]((align(8L)(size) >> 3).toInt)
      Array.copy(this.array, 0, array, 0, math.min(this.array.length, array.length))
      new Base8LE(array)
    }
    override def loadByte(address: Long): Byte = {
      val i = (address >> 3).toInt
      val j = (address.toInt & 7) << 3
      (array(i) >>> j).toByte
    }
    override def storeByte(address: Long, value: Byte) {
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
    @inline override def loadLong(address: Long): Long = array((address >> 3).toInt)
    @inline override def storeLong(address: Long, value: Long): Unit = array((address >> 3).toInt) = value
    override def toString: String = "Base8LE"+"("+ size +")"
  }
  
  /** An allocator for little-endian data backed by a `Long` array. */
  object Base8LE extends BaseAllocator[Long] {
    override def MaxSize: Long = Int.MaxValue.toLong << 3
    override def alloc[T](count: Long)(implicit unit: ValType[T]): Base8LE = apply(unit.size * count)
    override def apply(size: Long): Base8LE = {
      require(0L <= size && size <= MaxSize)
      val array = new Array[Long]((align(8L)(size) >> 3).toInt)
      new Base8LE(array)
    }
    override def wrap(array: Array[Long]): Base8LE = new Base8LE(array)
    def unapply(data: Base8LE): Some[Array[Long]] = Some(data.array)
    override def toString: String = "Base8LE"
  }
  
  /** `ByteBuffer` backed Data. */
  final class Buffer(val buffer: java.nio.ByteBuffer) extends AnyVal with Data {
    @inline override def size: Long = buffer.capacity.toLong
    @inline override def unit: Int = 1
    override def endian: Endianness = buffer.order match {
      case java.nio.ByteOrder.BIG_ENDIAN    => BigEndian
      case java.nio.ByteOrder.LITTLE_ENDIAN => LittleEndian
    }
    override def copy(size: Long): Buffer = {
      require(0 <= size && size <= Int.MaxValue.toLong)
      val dst = java.nio.ByteBuffer.allocateDirect(size.toInt)
      val src = buffer.duplicate
      src.position(0).limit(math.min(src.capacity, dst.capacity))
      dst.put(src)
      dst.clear()
      new Buffer(dst)
    }
    @inline override def loadByte(address: Long): Byte = buffer.get(address.toInt)
    @inline override def storeByte(address: Long, value: Byte): Unit = buffer.put(address.toInt, value)
    @inline override def loadUnalignedShort(address: Long): Short = buffer.getShort(address.toInt)
    @inline override def storeUnalignedShort(address: Long, value: Short): Unit = buffer.putShort(address.toInt, value)
    @inline override def loadUnalignedInt(address: Long): Int = buffer.getInt(address.toInt)
    @inline override def storeUnalignedInt(address: Long, value: Int): Unit = buffer.putInt(address.toInt, value)
    @inline override def loadUnalignedLong(address: Long): Long = buffer.getLong(address.toInt)
    @inline override def storeUnalignedLong(address: Long, value: Long): Unit = buffer.putLong(address.toInt, value)
    @inline override def loadUnalignedChar(address: Long): Char = buffer.getChar(address.toInt)
    @inline override def storeUnalignedChar(address: Long, value: Char): Unit = buffer.putChar(address.toInt, value)
    @inline override def loadUnalignedFloat(address: Long): Float = buffer.getFloat(address.toInt)
    @inline override def storeUnalignedFloat(address: Long, value: Float): Unit = buffer.putFloat(address.toInt, value)
    @inline override def loadUnalignedDouble(address: Long): Double = buffer.getDouble(address.toInt)
    @inline override def storeUnalignedDouble(address: Long, value: Double): Unit = buffer.putDouble(address.toInt, value)
    override def toString: String = endian match {
      case BigEndian    => "BufferBE"+"("+ size +")"
      case LittleEndian => "BufferLE"+"("+ size +")"
    }
  }
  
  /** An allocator for native-endian data backed by a `ByteBuffer`. */
  object Buffer extends Allocator {
    override def MaxSize: Long = Int.MaxValue.toLong
    override def alloc[T](count: Long)(implicit unit: ValType[T]): Buffer = apply(unit.size * count)
    override def apply(size: Long): Buffer = {
      require(0L <= size && size <= MaxSize)
      new Buffer(java.nio.ByteBuffer.allocateDirect(size.toInt))
    }
    def unapply(data: Buffer): Option[java.nio.ByteBuffer] = if (data.endian eq NativeEndian) Some(data.buffer) else None
    def wrap(buffer: java.nio.ByteBuffer): Buffer = new Buffer(buffer)
    override def toString: String = "Buffer"
  }
  
  /** An allocator for big-endian data backed by a `ByteBuffer`. */
  object BufferBE extends Allocator {
    override def MaxSize: Long = Int.MaxValue.toLong
    override def alloc[T](count: Long)(implicit unit: ValType[T]): Buffer = apply(unit.size * count)
    override def apply(size: Long): Buffer = {
      require(0L <= size && size <= MaxSize)
      new Buffer(java.nio.ByteBuffer.allocateDirect(size.toInt).order(java.nio.ByteOrder.BIG_ENDIAN))
    }
    def unapply(data: Buffer): Option[java.nio.ByteBuffer] = if (data.endian eq BigEndian) Some(data.buffer) else None
    override def toString: String = "BufferBE"
  }
  
  /** An allocator for little-endian data backed by a `ByteBuffer`. */
  object BufferLE extends Allocator {
    override def MaxSize: Long = Int.MaxValue.toLong
    override def alloc[T](count: Long)(implicit unit: ValType[T]): Buffer = apply(unit.size * count)
    override def apply(size: Long): Buffer = {
      require(0L <= size && size <= MaxSize)
      new Buffer(java.nio.ByteBuffer.allocateDirect(size.toInt).order(java.nio.ByteOrder.LITTLE_ENDIAN))
    }
    def unapply(data: Buffer): Option[java.nio.ByteBuffer] = if (data.endian eq LittleEndian) Some(data.buffer) else None
    override def toString: String = "BufferLE"
  }
}
