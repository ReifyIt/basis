//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

import basis.util._

trait Storer {
  /** Returns the internal byte order.
    * @group General */
  def endian: Endianness

  /** Returns `true` if this supports volatile semantics; returns `false`
    * if volatile operations do not guarantee coherency.
    * @group General */
  def isCoherent: Boolean = false

  /** Returns `true` if this can store the next `offset` addresses.
    * @group General */
  def canStore(offset: Long): Boolean

  /** Stores a single byte.
    *
    * @param  offset  the relative storage address.
    * @param  value   the `Byte` value to store.
    * @group  Aligned
    */
  def storeByte(offset: Long, value: Byte): Unit

  /** Stores a native-endian `Short` value as a 2-byte `endian` ordered word.
    * Truncates `offset` to 2-byte alignment.
    *
    * @param  offset  the 2-byte aligned relative storage address.
    * @param  value   the `Short` value to store.
    * @group  Aligned
    */
  def storeShort(offset: Long, value: Short): Unit =
    storeUnalignedShort(offset & -2L, value)

  /** Stores a native-endian `Int` value as a 4-byte `endian` ordered word.
    * Truncates `offset` to 4-byte alignment.
    *
    * @param  offset  the 4-byte aligned relative storage address.
    * @param  value   the `Int` value to store.
    * @group  Aligned
    */
  def storeInt(offset: Long, value: Int): Unit =
    storeUnalignedInt(offset & -4L, value)

  /** Store a native-endian `Long` value as an 8-byte `endian` ordered word.
    * Truncates `offset` to 8-byte alignment.
    *
    * @param  offset  the 8-byte aligned relative storage address.
    * @param  value   the `Long` value to store.
    * @group  Aligned
    */
  def storeLong(offset: Long, value: Long): Unit =
    storeUnalignedLong(offset & -8L, value)

  /** Stores a native-endian `Float` value as a 4-byte `endian` ordered word.
    * Truncates `offset` to 4-byte alignment.
    *
    * @param  offset  the 4-byte aligned relative storage address.
    * @param  value   the `Float` value to store.
    * @group  Aligned
    */
  def storeFloat(offset: Long, value: Float): Unit =
    storeInt(offset, value.toIntBits)

  /** Stores a native-endian `Double` value as an 8-byte `endian` ordered word.
    * Truncates `offset` to 8-byte alignment.
    *
    * @param  offset  the 8-byte aligned relative storage address.
    * @param  value   the `Double` value to store.
    * @group  Aligned
    */
  def storeDouble(offset: Long, value: Double): Unit =
    storeLong(offset, value.toLongBits)

  /** Stores a native-endian `Short` value as a 2-byte `endian` ordered word.
    *
    * @param  offset  the unaligned relative storage address.
    * @param  value   the `Short` value to store.
    * @group  Unaligned
    */
  def storeUnalignedShort(offset: Long, value: Short): Unit = {
    if (endian eq BigEndian) {
      storeByte(offset,      (value >> 8).toByte)
      storeByte(offset + 1L,  value.toByte)
    }
    else if (endian eq LittleEndian) {
      storeByte(offset,       value.toByte)
      storeByte(offset + 1L, (value >> 8).toByte)
    }
    else throw new MatchError(endian)
  }

  /** Stores a native-endian `Int` value as a 4-byte `endian` ordered word.
    *
    * @param  offset  the unaligned relative storage address.
    * @param  value   the `Int` value to store.
    * @group  Unaligned
    */
  def storeUnalignedInt(offset: Long, value: Int): Unit = {
    if (endian eq BigEndian) {
      storeByte(offset,      (value >> 24).toByte)
      storeByte(offset + 1L, (value >> 16).toByte)
      storeByte(offset + 2L, (value >>  8).toByte)
      storeByte(offset + 3L,  value.toByte)
    }
    else if (endian eq LittleEndian) {
      storeByte(offset,       value.toByte)
      storeByte(offset + 1L, (value >>  8).toByte)
      storeByte(offset + 2L, (value >> 16).toByte)
      storeByte(offset + 3L, (value >> 24).toByte)
    }
    else throw new MatchError(endian)
  }

  /** Stores a native-endian `Long` value as an 8-byte `endian` ordered word.
    *
    * @param  offset  the unaligned relative storage address.
    * @param  value   the `Long` value to store.
    * @group  Unaligned
    */
  def storeUnalignedLong(offset: Long, value: Long): Unit = {
    if (endian eq BigEndian) {
      storeByte(offset,      (value >> 56).toByte)
      storeByte(offset + 1L, (value >> 48).toByte)
      storeByte(offset + 2L, (value >> 40).toByte)
      storeByte(offset + 3L, (value >> 32).toByte)
      storeByte(offset + 4L, (value >> 24).toByte)
      storeByte(offset + 5L, (value >> 16).toByte)
      storeByte(offset + 6L, (value >>  8).toByte)
      storeByte(offset + 7L,  value.toByte)
    }
    else if (endian eq LittleEndian) {
      storeByte(offset,       value.toByte)
      storeByte(offset + 1L, (value >>  8).toByte)
      storeByte(offset + 2L, (value >> 16).toByte)
      storeByte(offset + 3L, (value >> 24).toByte)
      storeByte(offset + 4L, (value >> 32).toByte)
      storeByte(offset + 5L, (value >> 40).toByte)
      storeByte(offset + 6L, (value >> 48).toByte)
      storeByte(offset + 7L, (value >> 56).toByte)
    }
    else throw new MatchError(endian)
  }

  /** Stores a native-endian `Float` value as a 4-byte `endian` ordered word.
    *
    * @param  offset  the unaligned relative storage address.
    * @param  value   the `Float` value to store.
    * @group  Unaligned
    */
  def storeUnalignedFloat(offset: Long, value: Float): Unit =
    storeUnalignedInt(offset, value.toIntBits)

  /** Stores a native-endian `Double` value as an 8-byte `endian` ordered word.
    *
    * @param  offset  the unaligned relative storage address.
    * @param  value   the `Double` value to store.
    * @group  Unaligned
    */
  def storeUnalignedDouble(offset: Long, value: Double): Unit =
    storeUnalignedLong(offset, value.toLongBits)

  /** Stores a single byte with volatile semantics if `isCoherent`.
    *
    * @param  offset  the relative storage address.
    * @param  value   the `Byte` value to store.
    * @group  Volatile
    */
  def storeVolatileByte(offset: Long, value: Byte): Unit =
    storeByte(offset, value)

  /** Stores a native-endian `Short` value as a 2-byte `endian` ordered word with
    * volatile semantics if `isCoherent`. Truncates `offset` to 2-byte alignment.
    *
    * @param  offset  the 2-byte aligned relative storage address.
    * @param  value   the `Short` value to store.
    * @group  Volatile
    */
  def storeVolatileShort(offset: Long, value: Short): Unit =
    storeShort(offset, value)

  /** Stores a native-endian `Int` value as a 4-byte `endian` ordered word with
    * volatile semantics if `isCoherent`. Truncates `offset` to 4-byte alignment.
    *
    * @param  offset  the 4-byte aligned relative storage address.
    * @param  value   the `Int` value to store.
    * @group  Volatile
    */
  def storeVolatileInt(offset: Long, value: Int): Unit =
    storeInt(offset, value)

  /** Store a native-endian `Long` value as an 8-byte `endian` ordered word with
    * volatile semantics if `isCoherent`. Truncates `offset` to 8-byte alignment.
    *
    * @param  offset  the 8-byte aligned relative storage address.
    * @param  value   the `Long` value to store.
    * @group  Volatile
    */
  def storeVolatileLong(offset: Long, value: Long): Unit =
    storeLong(offset, value)

  /** Stores a native-endian `Float` value as a 4-byte `endian` ordered word with
    * volatile semantics if `isCoherent`. Truncates `offset` to 4-byte alignment.
    *
    * @param  offset  the 4-byte aligned relative storage address.
    * @param  value   the `Float` value to store.
    * @group  Volatile
    */
  def storeVolatileFloat(offset: Long, value: Float): Unit =
    storeFloat(offset, value)

  /** Stores a native-endian `Double` value as an 8-byte `endian` ordered word with
    * volatile semantics if `isCoherent`. Truncates `offset` to 8-byte alignment.
    *
    * @param  offset  the 8-byte aligned relative storage address.
    * @param  value   the `Double` value to store.
    * @group  Volatile
    */
  def storeVolatileDouble(offset: Long, value: Double): Unit =
    storeDouble(offset, value)

  /** Stores an instance as a struct value.
    *
    * @tparam T       the instance type to store.
    * @param  offset  the aligned relative storage address.
    * @param  value   the instance to store.
    * @param  T       the implicit struct type to store.
    * @group  Compound
    */
  def store[T](offset: Long, value: T)(implicit T: Struct[T]): Unit =
    T.store(this, offset, value)

  /** Stores an array slice as a sequence of struct values.
    *
    * @tparam T       the instance type to store.
    * @param  offset  the aligned relative storage address.
    * @param  array   the array to store from.
    * @param  start   the offset to store from in the array.
    * @param  count   the number of values to store.
    * @param  T       the implicit struct type to store.
    * @group  Aggregate
    */
  def storeArray[T](offset: Long, array: Array[T], start: Int, count: Int)(implicit T: Struct[T]): Unit = {
    val end = start + count
    var p = offset
    var i = start
    while (i < end) {
      T.store(this, p, array(i))
      p += T.size
      i += 1
    }
  }
}
