/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.data

/** A mutable sequence of 8-bit memory cells.
  * 
  * ==Address space==
  * 
  * `Mem` has a 64-bit ''address space'' ranging from `0` until `size`. Each
  * ''address'' in the space identifies a unique storage location for a single
  * `Byte` value. Multi-byte values occupy multiple storage locations and thus
  * have multiple addresses–one address per byte. The lowest address of a
  * multi-byte sequence canonically refers to the whole byte sequence.
  * 
  * ==Data values==
  * 
  * `Mem` stores structured value types. ''Value type'' in this context stands
  * for an isomorphism between Scala values and fixed-length byte sequences,
  * with a possible restriction on address alignment.
  * 
  * ===Primitive values===
  * 
  * Primitive value types have dedicated `load` and `store` methods. Multi-byte
  * primitives have ''aligned'' and ''unaligned'' variants. The memory's
  * `endian` property determines the byte order of multi-byte values.
  * 
  * ===Struct values===
  * 
  * [[basis.data.ValType]] typeclasses abstract over value types. Generic
  * `load` and `store` methods delegate to the implicit struct typeclass
  * associated with each method's type parameter.
  * 
  * ==Alignment==
  * 
  * N-byte divisible addresses are said to be N-byte ''aligned''. Using aligned
  * addresses reduces some multi-byte memory accesses to single array operations,
  * which can noticeably improve performance. Alignment sensitive allocators
  * such as the default `Block` allocator try to allocate memory backed by a
  * primitive array whose element size matches the alignment of the unit struct
  * passed to the allocator. This allocation strategy makes possible the
  * performance benefit of using aligned addresses.
  * 
  * Aligned memory accesses truncate unaligned addresses to the required alignment.
  * 
  * @author Chris Sachs
  * 
  * @example {{{
  * scala> val mem = Mem.alloc[Int](1L) // allocate memory for a single Int value.
  * mem: basis.memory.Mem = Mem4LE(4) // Mem class will vary by architecture.
  * 
  * scala> mem.storeInt(0L, 0xCAFEBABE) // store an Int value to address 0.
  * 
  * scala> mem.loadInt(0L).toHexString // load an Int value from address 0.
  * res1: java.lang.String = cafebabe
  * 
  * scala> mem.loadByte(0L).toHexString // load the low byte of the Int value.
  * res2: java.lang.String = ffffffbe // the least significant byte comes first in this case.
  * 
  * scala> mem.loadShort(2L).toHexString // load the high bytes of the Int value.
  * res3: java.lang.String = ffffcafe // toHexString sign extends the result to an Int.
  * 
  * scala> mem.loadShort(1L).toHexString // load an unaligned address.
  * res4: java.lang.String = ffffbabe // the address was truncated, oops.
  * 
  * scala> mem.loadUnalignedShort(1L).toHexString // load the unaligned middle bytes of the Int value.
  * res5: java.lang.String = fffffeba
  * }}}
  */
trait Mem extends Any {
  import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
  import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
  
  /** Returns the number of addressable bytes in the address space. */
  def size: Long
  
  /** Returns the internal word size. */
  def unit: Int
  
  /** Returns the memory's internal byte order. */
  def endian: ByteOrder
  
  /** Returns a resized copy of this memory.
    * 
    * @param  size  the number of bytes to copy.
    * @return the copied memory.
    */
  def copy(size: Long = this.size): Mem
  
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
    loadUnalignedShort(address & -2L)
  
  /** Stores a native-endian `Short` value as a 2-byte `endian` ordered word.
    * Truncates `address` to 2-byte alignment.
    * 
    * @param  address   the 2-byte aligned storage address.
    * @param  value     the `Short` value to store.
    */
  def storeShort(address: Long, value: Short): Unit =
    storeUnalignedShort(address & -2L, value)
  
  /** Loads a 4-byte `endian` ordered word as a native-endian `Int` value.
    * Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned address to load.
    * @return the loaded `Int` value.
    */
  def loadInt(address: Long): Int =
    loadUnalignedInt(address & -4L)
  
  /** Stores a native-endian `Int` value as a 4-byte `endian` ordered word.
    * Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned storage address.
    * @param  value     the `Int` value to store.
    */
  def storeInt(address: Long, value: Int): Unit =
    storeUnalignedInt(address & -4L, value)
  
  /** Loads an 8-byte `endian` ordered word as a native-endian `Long` value.
    * Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned address to load.
    * @return the loaded `Long` value.
    */ 
  def loadLong(address: Long): Long =
    loadUnalignedLong(address & -8L)
  
  /** Store a native-endian `Long` value as an 8-byte `endian` ordered word.
    * Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned storage address.
    * @param  value     the `Long` value to store.
    */
  def storeLong(address: Long, value: Long): Unit =
    storeUnalignedLong(address & -8L, value)
  
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
  def loadUnalignedShort(address: Long): Short = {
    if (endian eq BigEndian) {
      ((loadByte(address)              << 8) |
       (loadByte(address + 1L) & 0xFF)).toShort
    }
    else if (endian eq LittleEndian) {
      ((loadByte(address)      & 0xFF)       |
       (loadByte(address + 1L)         << 8)).toShort
    }
    else throw new scala.MatchError(endian)
  }
  
  /** Stores a native-endian `Short` value as a 2-byte `endian` ordered word.
    * 
    * @param  address   the unaligned storage address.
    * @param  value     the `Short` value to store.
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
    else throw new scala.MatchError(endian)
  }
  
  /** Loads a 4-byte `endian` ordered word as a native-endian `Int` value.
    * 
    * @param  address   the unaligned address to load.
    * @return the loaded `Int` value.
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
    else throw new scala.MatchError(endian)
  }
  
  /** Stores a native-endian `Int` value as a 4-byte `endian` ordered word.
    * 
    * @param  address   the unaligned storage address.
    * @param  value     the `Int` value to store.
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
    else throw new scala.MatchError(endian)
  }
  
  /** Loads an 8-byte `endian` ordered word as a native-endian `Long` value.
    * 
    * @param  address   the unaligned address to load.
    * @return the loaded `Long` value.
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
    else throw new scala.MatchError(endian)
  }
  
  /** Stores a native-endian `Long` value as an 8-byte `endian` ordered word.
    * 
    * @param  address   the unaligned storage address.
    * @param  value     the `Long` value to store.
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
    else throw new scala.MatchError(endian)
  }
  
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
  
  /** Moves a byte sequence to a different, potentially overlapping address.
    * 
    * @param  fromAddress   the address to copy from.
    * @param  toAddress     the address to copy to.
    * @param  size          the number of bytes to copy.
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

/** An allocator for native-endian memory backed by a primitive array. */
object Mem extends Allocator {
  implicit def MemOps(self: Mem): MemOps = new MemOps(self)
  
  override def MaxSize: Long = scala.Int.MaxValue.toLong << 3
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Mem = {
    val size = unit.size * count
    if (size <= scala.Int.MaxValue.toLong) unit.alignment match {
      case 1L => Mem1(size)
      case 2L => Mem2(size)
      case 4L => Mem4(size)
      case _  => Mem8(size)
    }
    else if (size <= (scala.Int.MaxValue.toLong << 1)) unit.alignment match {
      case 1L | 2L => Mem2(size)
      case 4L      => Mem4(size)
      case _       => Mem8(size)
    }
    else if (size <= (scala.Int.MaxValue.toLong << 2)) unit.alignment match {
      case 1L | 2L | 4L => Mem4(size)
      case _            => Mem8(size)
    }
    else Mem8(size)
  }
  
  override def apply(size: Long): Mem = alloc[Byte](size)
  
  override def toString: java.lang.String = "Mem"
  
  /** Copies a byte sequence from one memory object to another.
    * 
    * @param  from          the source memory.
    * @param  fromAddress   the source memory address.
    * @param  to            the destination memory.
    * @param  toAddress     the destination memory address.
    * @param  size          the number of bytes to copy.
    */
  def copy(from: Mem, fromAddress: Long, to: Mem, toAddress: Long, size: Long) {
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
}
