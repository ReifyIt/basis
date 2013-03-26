/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.memory

import basis.util._

/** A byte-addressed memory region.
  * 
  * ==Address Space==
  * 
  * `Data` objects have a 64-bit ''address space'' ranging from `0` until `size`.
  * Each ''address'' in the space identifies a unique storage location for a
  * single `Byte` value. Multi-byte values occupy multiple storage locations and
  * thus have multiple addresses–one address per byte. The lowest address of a
  * multi-byte sequence canonically refers to the whole byte sequence.
  * 
  * N-byte divisible addresses are said to be N-byte ''aligned''. Using aligned
  * addresses reduces some multi-byte memory accesses to single array operations,
  * which can improve performance. Alignment sensitive allocators, such as the
  * default [[Data$ Data]] allocator, try to allocate memory backed by a primitive
  * array whose element size matches the alignment of the struct values it will
  * store. This allocation strategy, combined with proper address alignment,
  * enables an optimal code path when serializaing many values.
  * 
  * Aligned memory accesses truncate unaligned addresses to their required alignment.
  * 
  * ==Value Types==
  * 
  * `Data` objects store structured value types. [[Struct Structs]] model
  * value types as transformations between instance types and fixed-length
  * byte sequences, with a restriction on address alignment.
  * 
  * Primitive types have dedicated `load` and `store` methods, with multi-byte
  * primitives declaring ''aligned'' and ''unaligned'' variants. A `Data` object's
  * `endian` property specifies the byte order used to interpret multi-byte values.
  * 
  * @example {{{
  * scala> val data = Data.alloc[Int](1L) // allocate data for a single Int value.
  * data: basis.memory.Data = Data4LE(4) // the runtime Data class may vary.
  * 
  * scala> data.storeInt(0L, 0xCAFEBABE) // store an Int value to address 0.
  * 
  * scala> data.loadInt(0L).toHexString // load an Int value from address 0.
  * res1: String = cafebabe
  * 
  * scala> data.loadByte(0L).toHexString // load the low byte of the Int value.
  * res2: String = ffffffbe // the least significant byte comes first in this case.
  * 
  * scala> data.loadShort(2L).toHexString // load the high bytes of the Int value.
  * res3: String = ffffcafe // toHexString sign extends the result to an Int.
  * 
  * scala> data.loadShort(1L).toHexString // load an unaligned address.
  * res4: String = ffffbabe // the address was truncated, oops.
  * 
  * scala> data.loadUnalignedShort(1L).toHexString // load the unaligned middle bytes of the Int value.
  * res5: String = fffffeba
  * }}}
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * 
  * @groupname  General     General properties
  * @groupprio  General     1
  * 
  * @groupname  Aligned     Loading and storing aligned primitive values
  * @groupprio  Aligned     2
  * 
  * @groupname  Unaligned   Loading and storing unaligned primitive values
  * @groupprio  Unaligned   3
  * 
  * @groupname  Volatile    Loading and storing volatile values
  * @groupprio  Volatile    4
  * 
  * @groupname  Compound    Loading and storing compound values
  * @groupprio  Compound    5
  * 
  * @groupname  Aggregate   Loading and storing arrays of values
  * @groupprio  Aggregate   6
  * 
  * @groupname  Bulk        Bulk transfer operations
  * @groupprio  Bulk        7
  */
abstract class Data {
  /** Returns the size in bytes of the address space.
    * @group General */
  def size: Long
  
  /** Returns the internal word size.
    * @group General */
  def unit: Int
  
  /** Returns the internal byte order.
    * @group General */
  def endian: Endianness
  
  /** Returns `true` if this data supports volatile semantics; returns `false`
    * if volatile operations do not guarantee coherency.
    * @group General */
  def isCoherent: Boolean = false
  
  /** Returns a resized copy of this data.
    * 
    * @param  size  the number of bytes to copy.
    * @return the copied data.
    * @group  Bulk
    */
  def copy(size: Long = this.size): Data
  
  /** Returns a pointer to an address in this data.
    * @group General */
  def addressOf(address: Long): Pointer = new DataPointer(this, address)
  
  /** Returns a pointer to this data.
    * @group General */
  def addressOf: Pointer = addressOf(0L)
  
  /** Loads a single byte.
    * 
    * @param  address   the address to load.
    * @return the loaded `Byte` value.
    * @group  Aligned
    */
  def loadByte(address: Long): Byte
  
  /** Stores a single byte.
    * 
    * @param  address   the storage address.
    * @param  value     the `Byte` value to store.
    * @group  Aligned
    */
  def storeByte(address: Long, value: Byte): Unit
  
  /** Loads a 2-byte `endian` ordered word as a native-endian `Short` value.
    * Truncates `address` to 2-byte alignment.
    * 
    * @param  address   the 2-byte aligned address to load.
    * @return the loaded `Short` value.
    * @group  Aligned
    */
  def loadShort(address: Long): Short =
    loadUnalignedShort(address & -2L)
  
  /** Stores a native-endian `Short` value as a 2-byte `endian` ordered word.
    * Truncates `address` to 2-byte alignment.
    * 
    * @param  address   the 2-byte aligned storage address.
    * @param  value     the `Short` value to store.
    * @group  Aligned
    */
  def storeShort(address: Long, value: Short): Unit =
    storeUnalignedShort(address & -2L, value)
  
  /** Loads a 4-byte `endian` ordered word as a native-endian `Int` value.
    * Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned address to load.
    * @return the loaded `Int` value.
    * @group  Aligned
    */
  def loadInt(address: Long): Int =
    loadUnalignedInt(address & -4L)
  
  /** Stores a native-endian `Int` value as a 4-byte `endian` ordered word.
    * Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned storage address.
    * @param  value     the `Int` value to store.
    * @group  Aligned
    */
  def storeInt(address: Long, value: Int): Unit =
    storeUnalignedInt(address & -4L, value)
  
  /** Loads an 8-byte `endian` ordered word as a native-endian `Long` value.
    * Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned address to load.
    * @return the loaded `Long` value.
    * @group  Aligned
    */ 
  def loadLong(address: Long): Long =
    loadUnalignedLong(address & -8L)
  
  /** Store a native-endian `Long` value as an 8-byte `endian` ordered word.
    * Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned storage address.
    * @param  value     the `Long` value to store.
    * @group  Aligned
    */
  def storeLong(address: Long, value: Long): Unit =
    storeUnalignedLong(address & -8L, value)
  
  /** Loads a 4-byte `endian` ordered word as a native-endian `Float` value.
    * Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned address to load.
    * @return the loaded `Float` value.
    * @group  Aligned
    */
  def loadFloat(address: Long): Float =
    loadInt(address).toFloatBits
  
  /** Stores a native-endian `Float` value as a 4-byte `endian` ordered word.
    * Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned storage address.
    * @param  value     the `Float` value to store.
    * @group  Aligned
    */
  def storeFloat(address: Long, value: Float): Unit =
    storeInt(address, value.toIntBits)
  
  /** Loads an 8-byte `endian` ordered word as a native-endian `Double` value.
    * Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned address to load.
    * @return the loaded `Double` value.
    * @group  Aligned
    */
  def loadDouble(address: Long): Double =
    loadLong(address).toDoubleBits
  
  /** Stores a native-endian `Double` value as an 8-byte `endian` ordered word.
    * Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned storage address.
    * @param  value     the `Double` value to store.
    * @group  Aligned
    */
  def storeDouble(address: Long, value: Double): Unit =
    storeLong(address, value.toLongBits)
  
  /** Loads a 2-byte `endian` ordered word as a native-endian `Short` value.
    * 
    * @param  address   the unaligned address to load.
    * @return the loaded `Short` value.
    * @group  Unaligned
    */
  def loadUnalignedShort(address: Long): Short = {
    if (endian eq BigEndian) {
      ((loadByte(address)              << 8) |
       (loadByte(address + 1L) & 0xFF)).toShort
    }
    else if (endian eq LittleEndian) {
      ((loadByte(address)      & 0xFF)       |
       (loadByte(address + 1L)         << 8)).toShort
    }
    else throw new MatchError(endian)
  }
  
  /** Stores a native-endian `Short` value as a 2-byte `endian` ordered word.
    * 
    * @param  address   the unaligned storage address.
    * @param  value     the `Short` value to store.
    * @group  Unaligned
    */
  def storeUnalignedShort(address: Long, value: Short) {
    if (endian eq BigEndian) {
      storeByte(address,      (value >> 8).toByte)
      storeByte(address + 1L,  value.toByte)
    }
    else if (endian eq LittleEndian) {
      storeByte(address,       value.toByte)
      storeByte(address + 1L, (value >> 8).toByte)
    }
    else throw new MatchError(endian)
  }
  
  /** Loads a 4-byte `endian` ordered word as a native-endian `Int` value.
    * 
    * @param  address   the unaligned address to load.
    * @return the loaded `Int` value.
    * @group  Unaligned
    */
  def loadUnalignedInt(address: Long): Int = {
    if (endian eq BigEndian) {
       (loadByte(address)              << 24) |
      ((loadByte(address + 1L) & 0xFF) << 16) |
      ((loadByte(address + 2L) & 0xFF) <<  8) |
       (loadByte(address + 3L) & 0xFF)
    }
    else if (endian eq LittleEndian) {
       (loadByte(address)      & 0xFF)        |
      ((loadByte(address + 1L) & 0xFF) <<  8) |
      ((loadByte(address + 2L) & 0xFF) << 16) |
       (loadByte(address + 3L)         << 24)
    }
    else throw new MatchError(endian)
  }
  
  /** Stores a native-endian `Int` value as a 4-byte `endian` ordered word.
    * 
    * @param  address   the unaligned storage address.
    * @param  value     the `Int` value to store.
    * @group  Unaligned
    */
  def storeUnalignedInt(address: Long, value: Int) {
    if (endian eq BigEndian) {
      storeByte(address,      (value >> 24).toByte)
      storeByte(address + 1L, (value >> 16).toByte)
      storeByte(address + 2L, (value >>  8).toByte)
      storeByte(address + 3L,  value.toByte)
    }
    else if (endian eq LittleEndian) {
      storeByte(address,       value.toByte)
      storeByte(address + 1L, (value >>  8).toByte)
      storeByte(address + 2L, (value >> 16).toByte)
      storeByte(address + 3L, (value >> 24).toByte)
    }
    else throw new MatchError(endian)
  }
  
  /** Loads an 8-byte `endian` ordered word as a native-endian `Long` value.
    * 
    * @param  address   the unaligned address to load.
    * @return the loaded `Long` value.
    * @group  Unaligned
    */
  def loadUnalignedLong(address: Long): Long = {
    if (endian eq BigEndian) {
       (loadByte(address).toLong              << 56) |
      ((loadByte(address + 1L) & 0xFF).toLong << 48) |
      ((loadByte(address + 2L) & 0xFF).toLong << 40) |
      ((loadByte(address + 3L) & 0xFF).toLong << 32) |
      ((loadByte(address + 4L) & 0xFF).toLong << 24) |
      ((loadByte(address + 5L) & 0xFF).toLong << 16) |
      ((loadByte(address + 6L) & 0xFF).toLong <<  8) |
       (loadByte(address + 7L) & 0xFF).toLong
    }
    else if (endian eq LittleEndian) {
       (loadByte(address)      & 0xFF).toLong        |
      ((loadByte(address + 1L) & 0xFF).toLong <<  8) |
      ((loadByte(address + 2L) & 0xFF).toLong << 16) |
      ((loadByte(address + 3L) & 0xFF).toLong << 24) |
      ((loadByte(address + 4L) & 0xFF).toLong << 32) |
      ((loadByte(address + 5L) & 0xFF).toLong << 40) |
      ((loadByte(address + 6L) & 0xFF).toLong << 48) |
       (loadByte(address + 7L).toLong         << 56)
    }
    else throw new MatchError(endian)
  }
  
  /** Stores a native-endian `Long` value as an 8-byte `endian` ordered word.
    * 
    * @param  address   the unaligned storage address.
    * @param  value     the `Long` value to store.
    * @group  Unaligned
    */
  def storeUnalignedLong(address: Long, value: Long) {
    if (endian eq BigEndian) {
      storeByte(address,      (value >> 56).toByte)
      storeByte(address + 1L, (value >> 48).toByte)
      storeByte(address + 2L, (value >> 40).toByte)
      storeByte(address + 3L, (value >> 32).toByte)
      storeByte(address + 4L, (value >> 24).toByte)
      storeByte(address + 5L, (value >> 16).toByte)
      storeByte(address + 6L, (value >>  8).toByte)
      storeByte(address + 7L,  value.toByte)
    }
    else if (endian eq LittleEndian) {
      storeByte(address,       value.toByte)
      storeByte(address + 1L, (value >>  8).toByte)
      storeByte(address + 2L, (value >> 16).toByte)
      storeByte(address + 3L, (value >> 24).toByte)
      storeByte(address + 4L, (value >> 32).toByte)
      storeByte(address + 5L, (value >> 40).toByte)
      storeByte(address + 6L, (value >> 48).toByte)
      storeByte(address + 7L, (value >> 56).toByte)
    }
    else throw new MatchError(endian)
  }
  
  /** Loads a 4-byte `endian` ordered word as a native-endian `Float` value.
    * 
    * @param  address   the unaligned address to load.
    * @return the loaded `Float` value.
    * @group  Unaligned
    */
  def loadUnalignedFloat(address: Long): Float =
    loadUnalignedInt(address).toFloatBits
  
  /** Stores a native-endian `Float` value as a 4-byte `endian` ordered word.
    * 
    * @param  address   the unaligned storage address.
    * @param  value     the `Float` value to store.
    * @group  Unaligned
    */
  def storeUnalignedFloat(address: Long, value: Float): Unit =
    storeUnalignedInt(address, value.toIntBits)
  
  /** Loads an 8-byte `endian` ordered word as a native-endian `Double` value.
    * 
    * @param  address   the unaligned address to load.
    * @return the loaded `Double` value.
    * @group  Unaligned
    */
  def loadUnalignedDouble(address: Long): Double =
    loadUnalignedLong(address).toDoubleBits
  
  /** Stores a native-endian `Double` value as an 8-byte `endian` ordered word.
    * 
    * @param  address   the unaligned storage address.
    * @param  value     the `Double` value to store.
    * @group  Unaligned
    */
  def storeUnalignedDouble(address: Long, value: Double): Unit =
    storeUnalignedLong(address, value.toLongBits)
  
  /** Loads a single byte with volatile semantics if `isCoherent`.
    * 
    * @param  address   the address to load.
    * @return the loaded `Byte` value.
    * @group  Volatile
    */
  def loadVolatileByte(address: Long): Byte =
    loadByte(address)
  
  /** Stores a single byte with volatile semantics if `isCoherent`.
    * 
    * @param  address   the storage address.
    * @param  value     the `Byte` value to store.
    * @group  Volatile
    */
  def storeVolatileByte(address: Long, value: Byte): Unit =
    storeByte(address, value)
  
  /** Loads a 2-byte `endian` ordered word as a native-endian `Short` value with
    * volatile semantics if `isCoherent`. Truncates `address` to 2-byte alignment.
    * 
    * @param  address   the 2-byte aligned address to load.
    * @return the loaded `Short` value.
    * @group  Volatile
    */
  def loadVolatileShort(address: Long): Short =
    loadShort(address)
  
  /** Stores a native-endian `Short` value as a 2-byte `endian` ordered word with
    * volatile semantics if `isCoherent`. Truncates `address` to 2-byte alignment.
    * 
    * @param  address   the 2-byte aligned storage address.
    * @param  value     the `Short` value to store.
    * @group  Volatile
    */
  def storeVolatileShort(address: Long, value: Short): Unit =
    storeShort(address, value)
  
  /** Loads a 4-byte `endian` ordered word as a native-endian `Int` value with
    * volatile semantics if `isCoherent`. Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned address to load.
    * @return the loaded `Int` value.
    * @group  Volatile
    */
  def loadVolatileInt(address: Long): Int =
    loadInt(address)
  
  /** Stores a native-endian `Int` value as a 4-byte `endian` ordered word with
    * volatile semantics if `isCoherent`. Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned storage address.
    * @param  value     the `Int` value to store.
    * @group  Volatile
    */
  def storeVolatileInt(address: Long, value: Int): Unit =
    storeInt(address, value)
  
  /** Loads an 8-byte `endian` ordered word as a native-endian `Long` value with
    * volatile semantics if `isCoherent`. Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned address to load.
    * @return the loaded `Long` value.
    * @group  Volatile
    */ 
  def loadVolatileLong(address: Long): Long =
    loadLong(address)
  
  /** Store a native-endian `Long` value as an 8-byte `endian` ordered word with
    * volatile semantics if `isCoherent`. Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned storage address.
    * @param  value     the `Long` value to store.
    * @group  Volatile
    */
  def storeVolatileLong(address: Long, value: Long): Unit =
    storeLong(address, value)
  
  /** Loads a 4-byte `endian` ordered word as a native-endian `Float` value with
    * volatile semantics if `isCoherent`. Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned address to load.
    * @return the loaded `Float` value.
    * @group  Volatile
    */
  def loadVolatileFloat(address: Long): Float =
    loadFloat(address)
  
  /** Stores a native-endian `Float` value as a 4-byte `endian` ordered word with
    * volatile semantics if `isCoherent`. Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned storage address.
    * @param  value     the `Float` value to store.
    * @group  Volatile
    */
  def storeVolatileFloat(address: Long, value: Float): Unit =
    storeFloat(address, value)
  
  /** Loads an 8-byte `endian` ordered word as a native-endian `Double` value with
    * volatile semantics if `isCoherent`. Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned address to load.
    * @return the loaded `Double` value.
    * @group  Volatile
    */
  def loadVolatileDouble(address: Long): Double =
    loadDouble(address)
  
  /** Stores a native-endian `Double` value as an 8-byte `endian` ordered word with
    * volatile semantics if `isCoherent`. Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned storage address.
    * @param  value     the `Double` value to store.
    * @group  Volatile
    */
  def storeVolatileDouble(address: Long, value: Double): Unit =
    storeDouble(address, value)
  
  /** Swaps a 4-byte `endian` ordered word containing an expected native-endian
    * `Int` value with another native-endian `Int` value; the swap happens
    * atomically if `isCoherent`, or does not happen at all if the comparison fails.
    * 
    * @param  address   the 4-byte aligned address to compare-and-swap.
    * @param  expected  the old `Int` value to replace.
    * @param  value     the new `Int` value to store.
    * @return `true` if the swap succeeded, otherwise `false` if the swap failed.
    * @group  Volatile
    */
  def compareAndSwapInt(address: Long, expected: Int, value: Int): Boolean =
    loadInt(address) == expected && { storeInt(address, value); true }
  
  /** Swaps an 8-byte `endian` ordered word containing an expected native-endian
    * `Long` value with another native-endian `Long` value; the swap happens
    * atomically if `isCoherent`, or does not happen at all if the compatison fails.
    * 
    * @param  address   the 8-byte aligned address to compare-and-swap.
    * @param  expected  the old `Long` value to replace.
    * @param  value     the new `Long` value to store.
    * @return `true` if the swap succeeded, otherwise `false` if the swap failed.
    * @group  Volatile
    */
  def compareAndSwapLong(address: Long, expected: Long, value: Long): Boolean =
    loadLong(address) == expected && { storeLong(address, value); true }
  
  /** Swaps a 4-byte `endian` ordered word containing an expected native-endian
    * `Float` value with another native-endian `Float` value; the swap happens
    * atomically if `isCoherent`, or does not happen at all if the comparison fails.
    * 
    * @param  address   the 4-byte aligned address to compare-and-swap.
    * @param  expected  the old `Float` value to replace.
    * @param  value     the new `Float` value to store.
    * @return `true` if the swap succeeded, otherwise `false` if the swap failed.
    * @group Volatile
    */
  def compareAndSwapFloat(address: Long, expected: Float, value: Float): Boolean =
    compareAndSwapInt(address, expected.toIntBits, value.toIntBits)
  
  /** Swaps an 8-byte `endian` ordered word containing an expected native-endian
    * `Double` value with another native-endian `Double` value; the swap happens
    * atomically if `isCoherent`, or does not happen at all if the comparison fails.
    * 
    * @param  address   the 8-byte aligned address to compare-and-swap.
    * @param  expected  the old `Double` value to replace.
    * @param  value     the new `Double` value to store.
    * @return `true` if the swap succeeded, otherwise `false` if the swap failed.
    * @group  Volatile
    */
  def compareAndSwapDouble(address: Long, expected: Double, value: Double): Boolean =
    compareAndSwapLong(address, expected.toLongBits, value.toLongBits)
  
  /** Loads an instance from a struct value.
    * 
    * @tparam T         the instance type to load.
    * @param  address   the aligned address to load.
    * @param  T         the implicit struct type to load.
    * @return the loaded instance.
    * @group  Compound
    */
  def load[T](address: Long)(implicit T: Struct[T]): T = macro DataMacros.load[T]
  
  /** Stores an instance as a struct value.
    * 
    * @tparam T         the instance type to store.
    * @param  address   the aligned storage address.
    * @param  value     the instance to store.
    * @param  T         the implicit struct type to store.
    * @group  Compound
    */
  def store[T](address: Long, value: T)(implicit T: Struct[T]): Unit = macro DataMacros.store[T]
  
  /** Loads and unpacks a struct as two values.
    * @group Compound */
  def load2[T1, T2, R]
      (address: Long)
      (f: (T1, T2) => R)
      (implicit T1: Struct[T1], T2: Struct[T2])
    : R = macro DataMacros.load2[T1, T2, R]
  
  /** Packs and stores two values as a struct.
    * @group Compound */
  def store2[T1, T2]
      (address: Long)
      (value1: T1, value2: T2)
      (implicit T1: Struct[T1], T2: Struct[T2])
    : Unit = macro DataMacros.store2[T1, T2]
  
  /** Loads and unpacks a struct as three values.
    * @group Compound */
  def load3[T1, T2, T3, R]
      (address: Long)
      (f: (T1, T2, T3) => R)
      (implicit T1: Struct[T1], T2: Struct[T2], T3: Struct[T3])
    : R = macro DataMacros.load3[T1, T2, T3, R]
  
  /** Packs and stores three values as a struct.
    * @group Compound */
  def store3[T1, T2, T3]
      (address: Long)
      (value1: T1, value2: T2, value3: T3)
      (implicit T1: Struct[T1], T2: Struct[T2], T3: Struct[T3])
    : Unit = macro DataMacros.store3[T1, T2, T3]
  
  /** Loads and unpacks a struct as four values.
    * @group Compound */
  def load4[T1, T2, T3, T4, R]
      (address: Long)
      (f: (T1, T2, T3, T4) => R)
      (implicit T1: Struct[T1], T2: Struct[T2], T3: Struct[T3], T4: Struct[T4])
    : R = macro DataMacros.load4[T1, T2, T3, T4, R]
  
  /** Packs and stores four values as a struct.
    * @group Compound */
  def store4[T1, T2, T3, T4]
      (address: Long)
      (value1: T1, value2: T2, value3: T3, value4: T4)
      (implicit T1: Struct[T1], T2: Struct[T2], T3: Struct[T3], T4: Struct[T4])
    : Unit = macro DataMacros.store4[T1, T2, T3, T4]
  
  /** Loads a sequence of struct values into a new array.
    * 
    * @tparam T         the instance type to load.
    * @param  address   the aligned address to load.
    * @param  count     the number of values to load.
    * @param  T         the implicit struct type to load.
    * @return the loaded array of instance values.
    * @group  Aggregate
    */
  def loadArray[T]
      (address: Long, count: Int)
      (implicit T: Struct[T])
    : Array[T] = {
    val array = T.newArray(count)
    copyToArray[T](address, array, 0, count)
    array
  }
  
  /** Copies a sequence of loaded struct values to an array slice.
    * 
    * @tparam T         the instance type to load.
    * @param  address   the aligned address to load.
    * @param  array     the array to copy to.
    * @param  start     the offset to copy to in the array.
    * @param  count     the number of values to copy.
    * @param  T         the implicit struct type to load.
    * @group  Aggregate
    */
  def copyToArray[T]
      (address: Long, array: Array[T], start: Int, count: Int)
      (implicit T: Struct[T]) {
    val end = start + count
    var p = address
    var i = start
    while (i < end) {
      array(i) = T.load(this, p)
      p += T.size
      i += 1
    }
  }
  
  /** Stores an array slice as a sequence of struct values.
    * 
    * @tparam T         the instance type to store.
    * @param  address   the aligned storage address.
    * @param  array     the array to store from.
    * @param  start     the offset to store from in the array.
    * @param  count     the number of values to store.
    * @param  T         the implicit struct type to store.
    * @group  Aggregate
    */
  def storeArray[T]
      (address: Long, array: Array[T], start: Int, count: Int)
      (implicit T: Struct[T]) {
    val end = start + count
    var p = address
    var i = start
    while (i < end) {
      T.store(this, p, array(i))
      p += T.size
      i += 1
    }
  }
  
  /** Moves a byte range to a new, potentially overlapping address.
    * 
    * @param  fromAddress   the address to copy from.
    * @param  toAddress     the address to copy to.
    * @param  size          the number of bytes to copy.
    * @group  Bulk
    */
  def move(fromAddress: Long, toAddress: Long, size: Long) {
    val fromLimit = fromAddress + size
    val toLimit = toAddress + size
    if (fromAddress == toAddress) ()
    else if (fromAddress >= toAddress || fromLimit <= toAddress) {
      var p = fromAddress
      var q = toAddress
      if (unit == 8 && (size & 7L) == 0L && (p & 7L) == 0L && (q & 7L) == 0L) {
        while (q < toLimit) {
          storeLong(q, loadLong(p))
          p += 8L
          q += 8L
        }
      }
      else if (unit == 4 && (size & 3L) == 0L && (p & 3L) == 0L && (q & 3L) == 0L) {
        while (q < toLimit) {
          storeInt(q, loadInt(p))
          p += 4L
          q += 4L
        }
      }
      else if (unit == 2 && (size & 1L) == 0L && (p & 1L) == 0L && (q & 1L) == 0L) {
        while (q < toLimit) {
          storeShort(q, loadShort(p))
          p += 2L
          q += 2L
        }
      }
      else {
        while (q < toLimit) {
          storeByte(q, loadByte(p))
          p += 1L
          q += 1L
        }
      }
    }
    else {
      var p = fromLimit - 1L
      var q = toLimit - 1L
      if (unit == 8 && (size & 7L) == 0L && (p & 7L) == 0L && (q & 7L) == 0L) {
        while (q >= toAddress) {
          storeLong(q, loadLong(p))
          p -= 8L
          q -= 8L
        }
      }
      else if (unit == 4 && (size & 3L) == 0L && (p & 3L) == 0L && (q & 3L) == 0L) {
        while (q >= toAddress) {
          storeInt(q, loadInt(p))
          p -= 4L
          q -= 4L
        }
      }
      else if (unit == 2 && (size & 1L) == 0L && (p & 1L) == 0L && (q & 1L) == 0L) {
        while (q >= toAddress) {
          storeShort(q, loadShort(p))
          p -= 2L
          q -= 2L
        }
      }
      else {
        while (q >= toAddress) {
          storeByte(q, loadByte(p))
          p -= 1L
          q -= 1L
        }
      }
    }
  }
  
  /** Zeros a range of addresses.
    * 
    * @param  fromAddress   the lower bound of the address range.
    * @param  untilAddress  the excluded upper bound of the address range.
    * @group  Bulk
    */
  def clear(fromAddress: Long, untilAddress: Long) {
    var p = fromAddress
    if (unit == 8 && (fromAddress & 7L) == 0L && (untilAddress & 7L) == 0L) {
      while (p < untilAddress) {
        storeLong(p, 0L)
        p += 8L
      }
    }
    else if (unit == 4 && (fromAddress & 3L) == 0L && (untilAddress & 3L) == 0L) {
      while (p < untilAddress) {
        storeInt(p, 0)
        p += 4L
      }
    }
    else if (unit == 2 && (fromAddress & 1L) == 0L && (untilAddress & 1L) == 0L) {
      while (p < untilAddress) {
        storeShort(p, 0.toShort)
        p += 2L
      }
    }
    else {
      while (p < untilAddress) {
        storeByte(p, 0.toByte)
        p += 1L
      }
    }
  }
}

/** An allocator for native-endian data backed by a primitive array. */
object Data extends Allocator {
  override def MaxSize: Long = Int.MaxValue.toLong << 3
  
  override def alloc[T](count: Long)(implicit T: Struct[T]): Data = {
    val size = T.size * count
    if (size <= Int.MaxValue.toLong) T.alignment match {
      case 1L => Data1(size)
      case 2L => Data2(size)
      case 4L => Data4(size)
      case _  => Data8(size)
    }
    else if (size <= (Int.MaxValue.toLong << 1)) T.alignment match {
      case 1L | 2L => Data2(size)
      case 4L      => Data4(size)
      case _       => Data8(size)
    }
    else if (size <= (Int.MaxValue.toLong << 2)) T.alignment match {
      case 1L | 2L | 4L => Data4(size)
      case _            => Data8(size)
    }
    else Data8(size)
  }
  
  override def apply(size: Long): Data = alloc[Byte](size)
  
  /** Copies a byte sequence from one `Data` object to another.
    * 
    * @param  from          the source data.
    * @param  fromAddress   the source address.
    * @param  to            the destination data.
    * @param  toAddress     the destination address.
    * @param  size          the number of bytes to copy.
    */
  def copy(from: Data, fromAddress: Long, to: Data, toAddress: Long, size: Long) {
    val limit = toAddress + size
    var p = fromAddress
    var q = toAddress
    if ((from.unit >= 8 || to.unit >= 8) && from.endian == to.endian &&
        (size & 7L) == 0L && (p & 7L) == 0L && (q & 7L) == 0L) {
      while (q < limit) {
        to.storeLong(q, from.loadLong(p))
        p += 8L
        q += 8L
      }
    }
    else if ((from.unit >= 4 || to.unit >= 4) && from.endian == to.endian &&
             (size & 3L) == 0L && (p & 3L) == 0L && (q & 3L) == 0L) {
      while (q < limit) {
        to.storeInt(q, from.loadInt(p))
        p += 4L
        q += 4L
      }
    }
    else if ((from.unit >= 2 || to.unit >= 2) && from.endian == to.endian &&
             (size & 1L) == 0L && (p & 1L) == 0L && (q & 1L) == 0L) {
      while (q < limit) {
        to.storeShort(q, from.loadShort(p))
        p += 2L
        q += 2L
      }
    }
    else {
      while (q < limit) {
        to.storeByte(q, from.loadByte(p))
        p += 1L
        q += 1L
      }
    }
  }
  
  override def toString: String = "Data"
}
