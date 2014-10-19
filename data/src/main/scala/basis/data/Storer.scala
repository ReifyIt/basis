//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._

trait Storer extends Any with ByteOrder[Endianness] {
  /** Returns the size in bytes of the address space.
    * @group General */
  def size: Long

  /** Returns an `endian` ordered view of the address space.
    * @group General */
  def as[E <: Endianness](endian: E): Storer with ByteOrder[E]

  /** Stores a single byte.
    *
    * @param  address   the storage address.
    * @param  value     the `Byte` value to store.
    * @group  Unaligned
    */
  def storeByte(address: Long, value: Byte): Unit

  /** Stores a native-endian `Short` value as a 2-byte `endian` ordered word.
    *
    * @param  address   the storage address.
    * @param  value     the `Short` value to store.
    * @group  Unaligned
    */
  def storeShort(address: Long, value: Short): Unit = {
    if (endian.isBig) {
      storeByte(address     , (value >> 8).toByte)
      storeByte(address + 1L, (value     ).toByte)
    }
    else if (endian.isLittle) {
      storeByte(address     , (value     ).toByte)
      storeByte(address + 1L, (value >> 8).toByte)
    }
    else throw new MatchError(endian)
  }

  /** Stores a native-endian `Int` value as a 4-byte `endian` ordered word.
    *
    * @param  address   the storage address.
    * @param  value     the `Int` value to store.
    * @group  Unaligned
    */
  def storeInt(address: Long, value: Int): Unit = {
    if (endian.isBig) {
      storeByte(address     , (value >> 24).toByte)
      storeByte(address + 1L, (value >> 16).toByte)
      storeByte(address + 2L, (value >>  8).toByte)
      storeByte(address + 3L, (value      ).toByte)
    }
    else if (endian.isLittle) {
      storeByte(address     , (value      ).toByte)
      storeByte(address + 1L, (value >>  8).toByte)
      storeByte(address + 2L, (value >> 16).toByte)
      storeByte(address + 3L, (value >> 24).toByte)
    }
    else throw new MatchError(endian)
  }

  /** Stores a native-endian `Long` value as an 8-byte `endian` ordered word.
    *
    * @param  address   the storage address.
    * @param  value     the `Long` value to store.
    * @group  Unaligned
    */
  def storeLong(address: Long, value: Long): Unit = {
    if (endian.isBig) {
      storeByte(address     , (value >> 56).toByte)
      storeByte(address + 1L, (value >> 48).toByte)
      storeByte(address + 2L, (value >> 40).toByte)
      storeByte(address + 3L, (value >> 32).toByte)
      storeByte(address + 4L, (value >> 24).toByte)
      storeByte(address + 5L, (value >> 16).toByte)
      storeByte(address + 6L, (value >>  8).toByte)
      storeByte(address + 7L, (value      ).toByte)
    }
    else if (endian.isLittle) {
      storeByte(address     , (value      ).toByte)
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

  /** Stores a native-endian `Float` value as a 4-byte `endian` ordered word.
    *
    * @param  address   the storage address.
    * @param  value     the `Float` value to store.
    * @group  Unaligned
    */
  def storeFloat(address: Long, value: Float): Unit = storeInt(address, value.toRawIntBits)

  /** Stores a native-endian `Double` value as an 8-byte `endian` ordered word.
    *
    * @param  address   the storage address.
    * @param  value     the `Double` value to store.
    * @group  Unaligned
    */
  def storeDouble(address: Long, value: Double): Unit = storeLong(address, value.toRawLongBits)

  /** Stores a native-endian `Short` value as a 2-byte `endian` ordered word.
    * Truncates `address` to 2-byte alignment.
    *
    * @param  address   the 2-byte aligned storage address.
    * @param  value     the `Short` value to store.
    * @group  Aligned
    */
  def storeAlignedShort(address: Long, value: Short): Unit = storeShort(address & -2L, value)

  /** Stores a native-endian `Int` value as a 4-byte `endian` ordered word.
    * Truncates `address` to 4-byte alignment.
    *
    * @param  address   the 4-byte aligned storage address.
    * @param  value     the `Int` value to store.
    * @group  Aligned
    */
  def storeAlignedInt(address: Long, value: Int): Unit = storeInt(address & -4L, value)

  /** Store a native-endian `Long` value as an 8-byte `endian` ordered word.
    * Truncates `address` to 8-byte alignment.
    *
    * @param  address   the 8-byte aligned storage address.
    * @param  value     the `Long` value to store.
    * @group  Aligned
    */
  def storeAlignedLong(address: Long, value: Long): Unit = storeLong(address & -8L, value)

  /** Stores a native-endian `Float` value as a 4-byte `endian` ordered word.
    * Truncates `address` to 4-byte alignment.
    *
    * @param  address   the 4-byte aligned storage address.
    * @param  value     the `Float` value to store.
    * @group  Aligned
    */
  def storeAlignedFloat(address: Long, value: Float): Unit = storeFloat(address & ~4L, value)

  /** Stores a native-endian `Double` value as an 8-byte `endian` ordered word.
    * Truncates `address` to 8-byte alignment.
    *
    * @param  address   the 8-byte aligned storage address.
    * @param  value     the `Double` value to store.
    * @group  Aligned
    */
  def storeAlignedDouble(address: Long, value: Double): Unit = storeDouble(address & ~8L, value)

  /** Stores a single byte with volatile semantics if `isCoherent`.
    *
    * @param  address   the storage address.
    * @param  value     the `Byte` value to store.
    * @group  Volatile
    */
  def storeVolatileByte(address: Long, value: Byte): Unit = storeByte(address, value)

  /** Stores a native-endian `Short` value as a 2-byte `endian` ordered word with
    * volatile semantics if `isCoherent`. Truncates `address` to 2-byte alignment.
    *
    * @param  address   the 2-byte aligned storage address.
    * @param  value     the `Short` value to store.
    * @group  Volatile
    */
  def storeVolatileShort(address: Long, value: Short): Unit = storeAlignedShort(address, value)

  /** Stores a native-endian `Int` value as a 4-byte `endian` ordered word with
    * volatile semantics if `isCoherent`. Truncates `address` to 4-byte alignment.
    *
    * @param  address   the 4-byte aligned storage address.
    * @param  value     the `Int` value to store.
    * @group  Volatile
    */
  def storeVolatileInt(address: Long, value: Int): Unit = storeAlignedInt(address, value)

  /** Store a native-endian `Long` value as an 8-byte `endian` ordered word with
    * volatile semantics if `isCoherent`. Truncates `address` to 8-byte alignment.
    *
    * @param  address   the 8-byte aligned storage address.
    * @param  value     the `Long` value to store.
    * @group  Volatile
    */
  def storeVolatileLong(address: Long, value: Long): Unit = storeAlignedLong(address, value)

  /** Stores a native-endian `Float` value as a 4-byte `endian` ordered word with
    * volatile semantics if `isCoherent`. Truncates `address` to 4-byte alignment.
    *
    * @param  address   the 4-byte aligned storage address.
    * @param  value     the `Float` value to store.
    * @group  Volatile
    */
  def storeVolatileFloat(address: Long, value: Float): Unit = storeAlignedFloat(address, value)

  /** Stores a native-endian `Double` value as an 8-byte `endian` ordered word with
    * volatile semantics if `isCoherent`. Truncates `address` to 8-byte alignment.
    *
    * @param  address   the 8-byte aligned storage address.
    * @param  value     the `Double` value to store.
    * @group  Volatile
    */
  def storeVolatileDouble(address: Long, value: Double): Unit = storeAlignedDouble(address, value)
}
