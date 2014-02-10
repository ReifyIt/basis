//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

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
  *
  * @groupname  Pointer     Pointer creation
  * @groupprio  Pointer     8
  */
abstract class Data extends Loader with Storer {
  /** Returns `true` if this data supports volatile semantics; returns `false`
    * if volatile operations do not guarantee coherency.
    * @group General */
  override def isCoherent: Boolean = false

  /** Returns `true` if this data can load up to `address`.
    * @group General */
  override def canLoad(address: Long): Boolean = 0L <= address && address < size

  /** Returns `true` if this data can store up to `address`.
    * @group General */
  override def canStore(address: Long): Boolean = 0L <= address && address < size

  /** Returns the size in bytes of the address space.
    * @group General */
  def size: Long

  /** Returns the internal word size.
    * @group General */
  def unit: Int

  /** Returns a resized copy of this data.
    *
    * @param  size  the number of bytes to copy.
    * @return the copied data.
    * @group  Bulk
    */
  def copy(size: Long = this.size): Data

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

  /** Moves a byte range to a new, potentially overlapping address.
    *
    * @param  fromAddress   the address to copy from.
    * @param  toAddress     the address to copy to.
    * @param  size          the number of bytes to copy.
    * @group  Bulk
    */
  def move(fromAddress: Long, toAddress: Long, size: Long): Unit = {
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
  def clear(fromAddress: Long, untilAddress: Long): Unit = {
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

  def pointer(address: Long = 0L): Pointer = new DataPointer(this, address)
}

/** An allocator for native-endian data backed by a primitive array. */
object Data extends Allocator {
  override def MaxSize: Long = Int.MaxValue.toLong << 3

  override def Endian: Endianness = NativeEndian

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
  def copy(from: Data, fromAddress: Long, to: Data, toAddress: Long, size: Long): Unit = {
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
