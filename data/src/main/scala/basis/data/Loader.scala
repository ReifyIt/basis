//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis._
import basis.text._
import basis.util._

trait Loader extends Any with Equals with Family[Loader] with ByteOrder[Endianness] {
  /** Returns the size in bytes of the address space.
    * @group General */
  def size: Long

  /** Loads a single byte.
    *
    * @param  address   the address to load.
    * @return the loaded `Byte` value.
    * @group  Unaligned
    */
  def loadByte(address: Long): Byte

  /** Loads a 2-byte `endian` ordered word as a native-endian `Short` value.
    *
    * @param  address   the address to load.
    * @return the loaded `Short` value.
    * @group  Unaligned
    */
  def loadShort(address: Long): Short = {
    if (endian.isBig)
      ((loadByte(address     )       ) << 8) |
      ((loadByte(address + 1L) & 0xFF)     )
    else if (endian.isLittle)
      ((loadByte(address     ) & 0xFF)     ) |
      ((loadByte(address + 1L)       ) << 8)
    else throw new MatchError(endian)
  }.toShort

  /** Loads a 4-byte `endian` ordered word as a native-endian `Int` value.
    *
    * @param  address   the address to load.
    * @return the loaded `Int` value.
    * @group  Unaligned
    */
  def loadInt(address: Long): Int = {
    if (endian.isBig)
      ((loadByte(address     )       ) << 24) |
      ((loadByte(address + 1L) & 0xFF) << 16) |
      ((loadByte(address + 2L) & 0xFF) <<  8) |
      ((loadByte(address + 3L) & 0xFF)      )
    else if (endian.isLittle)
      ((loadByte(address     ) & 0xFF)      ) |
      ((loadByte(address + 1L) & 0xFF) <<  8) |
      ((loadByte(address + 2L) & 0xFF) << 16) |
      ((loadByte(address + 3L)       ) << 24)
    else throw new MatchError(endian)
  }

  /** Loads an 8-byte `endian` ordered word as a native-endian `Long` value.
    *
    * @param  address   the address to load.
    * @return the loaded `Long` value.
    * @group  Unaligned
    */
  def loadLong(address: Long): Long = {
    if (endian.isBig)
      ((loadByte(address     )       ).toLong << 56) |
      ((loadByte(address + 1L) & 0xFF).toLong << 48) |
      ((loadByte(address + 2L) & 0xFF).toLong << 40) |
      ((loadByte(address + 3L) & 0xFF).toLong << 32) |
      ((loadByte(address + 4L) & 0xFF).toLong << 24) |
      ((loadByte(address + 5L) & 0xFF).toLong << 16) |
      ((loadByte(address + 6L) & 0xFF).toLong <<  8) |
      ((loadByte(address + 7L) & 0xFF).toLong      )
    else if (endian.isLittle)
      ((loadByte(address     ) & 0xFF).toLong      ) |
      ((loadByte(address + 1L) & 0xFF).toLong <<  8) |
      ((loadByte(address + 2L) & 0xFF).toLong << 16) |
      ((loadByte(address + 3L) & 0xFF).toLong << 24) |
      ((loadByte(address + 4L) & 0xFF).toLong << 32) |
      ((loadByte(address + 5L) & 0xFF).toLong << 40) |
      ((loadByte(address + 6L) & 0xFF).toLong << 48) |
      ((loadByte(address + 7L)       ).toLong << 56)
    else throw new MatchError(endian)
  }

  /** Loads a 4-byte `endian` ordered word as a native-endian `Float` value.
    *
    * @param  address   the address to load.
    * @return the loaded `Float` value.
    * @group  Unaligned
    */
  def loadFloat(address: Long): Float = loadInt(address).toFloatBits

  /** Loads an 8-byte `endian` ordered word as a native-endian `Double` value.
    *
    * @param  address   the address to load.
    * @return the loaded `Double` value.
    * @group  Unaligned
    */
  def loadDouble(address: Long): Double = loadLong(address).toDoubleBits

  /** Loads a 2-byte `endian` ordered word as a native-endian `Short` value.
    * Truncates `address` to 2-byte alignment.
    *
    * @param  address   the 2-byte aligned address to load.
    * @return the loaded `Short` value.
    * @group  Aligned
    */
  def loadAlignedShort(address: Long): Short = loadShort(address & -2L)

  /** Loads a 4-byte `endian` ordered word as a native-endian `Int` value.
    * Truncates `address` to 4-byte alignment.
    *
    * @param  address   the 4-byte aligned address to load.
    * @return the loaded `Int` value.
    * @group  Aligned
    */
  def loadAlignedInt(address: Long): Int = loadInt(address & -4L)

  /** Loads an 8-byte `endian` ordered word as a native-endian `Long` value.
    * Truncates `address` to 8-byte alignment.
    *
    * @param  address   the 8-byte aligned address to load.
    * @return the loaded `Long` value.
    * @group  Aligned
    */
  def loadAlignedLong(address: Long): Long = loadLong(address & -8L)

  /** Loads a 4-byte `endian` ordered word as a native-endian `Float` value.
    * Truncates `address` to 4-byte alignment.
    *
    * @param  address   the 4-byte aligned address to load.
    * @return the loaded `Float` value.
    * @group  Aligned
    */
  def loadAlignedFloat(address: Long): Float = loadAlignedInt(address).toFloatBits

  /** Loads an 8-byte `endian` ordered word as a native-endian `Double` value.
    * Truncates `address` to 8-byte alignment.
    *
    * @param  address   the 8-byte aligned address to load.
    * @return the loaded `Double` value.
    * @group  Aligned
    */
  def loadAlignedDouble(address: Long): Double = loadAlignedLong(address).toDoubleBits

  /** Loads a single byte with volatile semantics if `isCoherent`.
    *
    * @param  address   the address to load.
    * @return the loaded `Byte` value.
    * @group  Volatile
    */
  def loadVolatileByte(address: Long): Byte = loadByte(address)

  /** Loads a 2-byte `endian` ordered word as a native-endian `Short` value with
    * volatile semantics if `isCoherent`. Truncates `address` to 2-byte alignment.
    *
    * @param  address   the 2-byte aligned address to load.
    * @return the loaded `Short` value.
    * @group  Volatile
    */
  def loadVolatileShort(address: Long): Short = loadAlignedShort(address)

  /** Loads a 4-byte `endian` ordered word as a native-endian `Int` value with
    * volatile semantics if `isCoherent`. Truncates `address` to 4-byte alignment.
    *
    * @param  address   the 4-byte aligned address to load.
    * @return the loaded `Int` value.
    * @group  Volatile
    */
  def loadVolatileInt(address: Long): Int = loadAlignedInt(address)

  /** Loads an 8-byte `endian` ordered word as a native-endian `Long` value with
    * volatile semantics if `isCoherent`. Truncates `address` to 8-byte alignment.
    *
    * @param  address   the 8-byte aligned address to load.
    * @return the loaded `Long` value.
    * @group  Volatile
    */
  def loadVolatileLong(address: Long): Long = loadAlignedLong(address)

  /** Loads a 4-byte `endian` ordered word as a native-endian `Float` value with
    * volatile semantics if `isCoherent`. Truncates `address` to 4-byte alignment.
    *
    * @param  address   the 4-byte aligned address to load.
    * @return the loaded `Float` value.
    * @group  Volatile
    */
  def loadVolatileFloat(address: Long): Float = loadAlignedFloat(address)

  /** Loads an 8-byte `endian` ordered word as a native-endian `Double` value with
    * volatile semantics if `isCoherent`. Truncates `address` to 8-byte alignment.
    *
    * @param  address   the 8-byte aligned address to load.
    * @return the loaded `Double` value.
    * @group  Volatile
    */
  def loadVolatileDouble(address: Long): Double = loadAlignedDouble(address)

  /** Returns a `Reader` starting at `address`.
    * @group General */
  def reader(address: Long): Reader with ByteOrder[Endian]

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Loader]

  override def equals(other: Any): Boolean = other.isInstanceOf[Loader] && {
    val that = other.asInstanceOf[Loader]
    val n = size
    that.canEqual(this) && n == that.size && {
      var i = 0L
      while (i < n && loadByte(i) == that.loadByte(i)) i += 1L
      i == n
    }
  }

  override def hashCode: Int = {
    import basis.util.MurmurHash3._
    var h = seed[Loader]
    var i = 0L
    val n = size
    while (i < n) {
      h = mix(h, loadByte(i).toInt)
      i += 1L
    }
    mash(h)
  }

  override def toString: String = {
    val s = UString.Builder
    s.append(stringPrefix)
    s.append('(')
    s.append('\"')
    new LoaderOps(this).writeBase64(s)
    s.append('\"')
    s.append(')')
    s.state.toString
  }

  protected def stringPrefix: String = getClass.getSimpleName
}

object Loader extends ByteOrder[NativeEndian] with DataFactory[Loader with ByteOrder[NativeEndian]] {
  override def endian: NativeEndian = NativeEndian

  override val empty: Loader with ByteOrder[NativeEndian] = FingerTrieData.empty

  implicit override def Framer: Framer with ByteOrder[NativeEndian] with State[Loader with ByteOrder[NativeEndian]] = FingerTrieData.Framer

  override def toString: String = "Loader"
}
