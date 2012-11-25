/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis.memory

/** A mutable byte sequence with supporting low-level memory operations.
  * 
  * ==Address space==
  * 
  * `Data` objects have a 64-bit ''address space'' ranging from `0` until `size`.
  * Each ''address'' in the space identifies a unique storage location for a
  * single `Byte` value. Multi-byte values occupy multiple storage locations and
  * thus have multiple addresses–one address per byte. The lowest address of a
  * multi-byte sequence canonically refers to the whole byte sequence.
  * 
  * ==Value types==
  * 
  * `Data` objects store structured value types. In this context, ''Value type''
  * means an isomorphism between Scala values and fixed-length byte sequences,
  * with a possible restriction on address alignment.
  * 
  * ===Primitive value types===
  * 
  * Primitive value types have dedicated `load` and `store` methods. Multi-byte
  * primitives have ''aligned'' and ''unaligned'' variants. The data's
  * `endian` property determines the byte order of multi-byte values.
  * 
  * ===Derived value types===
  * 
  * [[ValType]] instances abstract over value types. Generic `load` and `store`
  * methods delegate to implicitly supplied `ValType` instances.
  * 
  * ==Address alignment==
  * 
  * N-byte divisible addresses are said to be N-byte ''aligned''. Using aligned
  * addresses reduces some multi-byte data accesses to single array operations,
  * which can improve performance. Alignment sensitive allocators such as the
  * default `Data` allocator try to allocate data backed by a primitive array
  * whose element size matches the alignment of an implicitly supplied unit
  * struct. This allocation strategy combined with proper address alignment
  * enables optimal code paths.
  * 
  * Aligned data accesses truncate unaligned addresses to the required alignment.
  * 
  * @example {{{
  * scala> val data = Data.alloc[Int](1L) // allocate data for a single Int value.
  * data: basis.memory.Data = Data4LE(4) // Data class will vary by architecture.
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
  * @groupname  general     Memory properties
  * @groupprio  general     -10
  * 
  * @groupname  aligned     Loading and storing aligned primitive values
  * @groupprio  aligned     -9
  * 
  * @groupname  unaligned   Loading and storing unaligned primitive values
  * @groupprio  unaligned   -8
  * 
  * @groupname  compound    Loading and storing compound values
  * @groupprio  compound    -7
  * 
  * @groupname  array       Loading and storing arrays of values
  * @groupprio  array       -6
  * 
  * @groupname  bulk        Bulk transfer operations
  * @groupprio  bulk        -5
  */
abstract class Data {
  import java.lang.Float.{floatToRawIntBits, intBitsToFloat}
  import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
  
  /** Returns the number of bytes in this data's address space.
    * @group general */
  def size: Long
  
  /** Returns this data's internal word size.
    * @group general */
  def unit: Int
  
  /** Returns this data's internal byte order.
    * @group general */
  def endian: ByteOrder
  
  /** Returns a resized copy of this data.
    * 
    * @param  size  the number of bytes to copy.
    * @return the copied data.
    * @group  bulk
    */
  def copy(size: Long = this.size): Data
  
  /** Loads a single byte.
    * 
    * @param  address   the address to load.
    * @return the loaded `Byte` value.
    * @group  aligned
    */
  def loadByte(address: Long): Byte
  
  /** Stores a single byte.
    * 
    * @param  address   the storage address.
    * @param  value     the `Byte` value to store.
    * @group  aligned
    */
  def storeByte(address: Long, value: Byte): Unit
  
  /** Loads a 2-byte `endian` ordered word as a native-endian `Short` value.
    * Truncates `address` to 2-byte alignment.
    * 
    * @param  address   the 2-byte aligned address to load.
    * @return the loaded `Short` value.
    * @group  aligned
    */
  def loadShort(address: Long): Short =
    loadUnalignedShort(address & -2L)
  
  /** Stores a native-endian `Short` value as a 2-byte `endian` ordered word.
    * Truncates `address` to 2-byte alignment.
    * 
    * @param  address   the 2-byte aligned storage address.
    * @param  value     the `Short` value to store.
    * @group  aligned
    */
  def storeShort(address: Long, value: Short): Unit =
    storeUnalignedShort(address & -2L, value)
  
  /** Loads a 4-byte `endian` ordered word as a native-endian `Int` value.
    * Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned address to load.
    * @return the loaded `Int` value.
    * @group  aligned
    */
  def loadInt(address: Long): Int =
    loadUnalignedInt(address & -4L)
  
  /** Stores a native-endian `Int` value as a 4-byte `endian` ordered word.
    * Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned storage address.
    * @param  value     the `Int` value to store.
    * @group  aligned
    */
  def storeInt(address: Long, value: Int): Unit =
    storeUnalignedInt(address & -4L, value)
  
  /** Loads an 8-byte `endian` ordered word as a native-endian `Long` value.
    * Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned address to load.
    * @return the loaded `Long` value.
    * @group  aligned
    */ 
  def loadLong(address: Long): Long =
    loadUnalignedLong(address & -8L)
  
  /** Store a native-endian `Long` value as an 8-byte `endian` ordered word.
    * Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned storage address.
    * @param  value     the `Long` value to store.
    * @group  aligned
    */
  def storeLong(address: Long, value: Long): Unit =
    storeUnalignedLong(address & -8L, value)
  
  /** Loads a 4-byte `endian` ordered word as a native-endian `Float` value.
    * Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned address to load.
    * @return the loaded `Float` value.
    * @group  aligned
    */
  def loadFloat(address: Long): Float =
    intBitsToFloat(loadInt(address))
  
  /** Stores a native-endian `Float` value as a 4-byte `endian` ordered word.
    * Truncates `address` to 4-byte alignment.
    * 
    * @param  address   the 4-byte aligned storage address.
    * @param  value     the `Float` value to store.
    * @group  aligned
    */
  def storeFloat(address: Long, value: Float): Unit =
    storeInt(address, floatToRawIntBits(value))
  
  /** Loads an 8-byte `endian` ordered word as a native-endian `Double` value.
    * Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned address to load.
    * @return the loaded `Double` value.
    * @group  aligned
    */
  def loadDouble(address: Long): Double =
    longBitsToDouble(loadLong(address))
  
  /** Stores a native-endian `Double` value as an 8-byte `endian` ordered word.
    * Truncates `address` to 8-byte alignment.
    * 
    * @param  address   the 8-byte aligned storage address.
    * @param  value     the `Double` value to store.
    * @group  aligned
    */
  def storeDouble(address: Long, value: Double): Unit =
    storeLong(address, doubleToRawLongBits(value))
  
  /** Loads a 2-byte `endian` ordered word as a native-endian `Short` value.
    * 
    * @param  address   the unaligned address to load.
    * @return the loaded `Short` value.
    * @group  unaligned
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
    * @group  unaligned
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
    * @group  unaligned
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
    * @group  unaligned
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
    * @group  unaligned
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
    * @group  unaligned
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
    * @group  unaligned
    */
  def loadUnalignedFloat(address: Long): Float =
    intBitsToFloat(loadUnalignedInt(address))
  
  /** Stores a native-endian `Float` value as a 4-byte `endian` ordered word.
    * 
    * @param  address   the unaligned storage address.
    * @param  value     the `Float` value to store.
    * @group  unaligned
    */
  def storeUnalignedFloat(address: Long, value: Float): Unit =
    storeUnalignedInt(address, floatToRawIntBits(value))
  
  /** Loads an 8-byte `endian` ordered word as a native-endian `Double` value.
    * 
    * @param  address   the unaligned address to load.
    * @return the loaded `Double` value.
    * @group  unaligned
    */
  def loadUnalignedDouble(address: Long): Double =
    longBitsToDouble(loadUnalignedLong(address))
  
  /** Stores a native-endian `Double` value as an 8-byte `endian` ordered word.
    * 
    * @param  address   the unaligned storage address.
    * @param  value     the `Double` value to store.
    * @group  unaligned
    */
  def storeUnalignedDouble(address: Long, value: Double): Unit =
    storeUnalignedLong(address, doubleToRawLongBits(value))
  
  /** Loads an instance from a data value.
    * 
    * @tparam T         the instance type to load.
    * @param  address   the aligned address to load.
    * @param  field     the implicit value type to load.
    * @return the loaded instance.
    * @group  compound
    */
  def load[T](address: Long)(implicit field: ValType[T]): T =
    macro DataMacros.load[T]
  
  /** Stores an instance as a data value.
    * 
    * @tparam T         the instance type to store.
    * @param  address   the aligned storage address.
    * @param  value     the instance to store.
    * @param  field     the implicit value type to store.
    * @group  compound
    */
  def store[T](address: Long, value: T)(implicit field: ValType[T]): Unit =
    macro DataMacros.store[T]
  
  /** Loads and unpacks a struct as two values.
    * @group compound */
  def load2[T1, T2, R](address: Long)
      (f: (T1, T2) => R)
      (implicit field1: ValType[T1], field2: ValType[T2]): R =
    macro DataMacros.load2[T1, T2, R]
  
  /** Packs and stores two values as a struct.
    * @group compound */
  def store2[T1, T2](address: Long)
      (value1: T1, value2: T2)
      (implicit field1: ValType[T1], field2: ValType[T2]): Unit =
    macro DataMacros.store2[T1, T2]
  
  /** Loads and unpacks a struct as three values.
    * @group compound */
  def load3[T1, T2, T3, R](address: Long)
      (f: (T1, T2, T3) => R)
      (implicit field1: ValType[T1], field2: ValType[T2],
                field3: ValType[T3]): R =
    macro DataMacros.load3[T1, T2, T3, R]
  
  /** Packs and stores three values as a struct.
    * @group compound */
  def store3[T1, T2, T3](address: Long)
      (value1: T1, value2: T2, value3: T3)
      (implicit field1: ValType[T1], field2: ValType[T2],
                field3: ValType[T3]): Unit =
    macro DataMacros.store3[T1, T2, T3]
  
  /** Loads and unpacks a struct as four values.
    * @group compound */
  def load4[T1, T2, T3, T4, R](address: Long)
      (f: (T1, T2, T3, T4) => R)
      (implicit field1: ValType[T1], field2: ValType[T2],
                field3: ValType[T3], field4: ValType[T4]): R =
    macro DataMacros.load4[T1, T2, T3, T4, R]
  
  /** Packs and stores four values as a struct.
    * @group compound */
  def store4[T1, T2, T3, T4](address: Long)
      (value1: T1, value2: T2, value3: T3, value4: T4)
      (implicit field1: ValType[T1], field2: ValType[T2],
                field3: ValType[T3], field4: ValType[T4]): Unit =
    macro DataMacros.store4[T1, T2, T3, T4]
  
  /** Loads a sequence of data values into a new array.
    * 
    * @tparam T         the instance type to load.
    * @param  address   the aligned address to load.
    * @param  count     the number of values to load.
    * @param  field     the implicit value type to load.
    * @param  tag       the reflective type of the array to load.
    * @return the loaded array of instance values.
    * @group  array
    */
  def loadArray[T]
      (address: Long, count: Int)
      (implicit field: ValType[T], tag: scala.reflect.ClassTag[T])
    : Array[T] = {
    val array = tag.newArray(count)
    copyToArray[T](address, array, 0, count)
    array
  }
  
  /** Copies a sequence of loaded data values to an array slice.
    * 
    * @tparam T         the instance type to load.
    * @param  address   the aligned address to load.
    * @param  array     the array to copy to.
    * @param  start     the lower bound of the array slice to copy to.
    * @param  count     the number of values to copy.
    * @param  field     the implicit value type to load.
    * @group  array
    */
  def copyToArray[T]
      (address: Long, array: Array[T], start: Int, count: Int)
      (implicit field: ValType[T]) {
    val end = start + count
    var p = address
    var i = start
    while (i < end) {
      array(i) = field.load(this, p)
      p += field.size
      i += 1
    }
  }
  
  /** Stores an array slice as a sequence of data values.
    * 
    * @tparam T         the instance type to store.
    * @param  address   the aligned storage address.
    * @param  array     the array to store from.
    * @param  start     the lower bound of the array slice to store from.
    * @param  count     the number of values to store.
    * @param  field     the implicit value type to store.
    * @group  array
    */
  def storeArray[T]
      (address: Long, array: Array[T], start: Int, count: Int)
      (implicit field: ValType[T]) {
    val end = start + count
    var p = address
    var i = start
    while (i < end) {
      field.store(this, p, array(i))
      p += field.size
      i += 1
    }
  }
  
  /** Moves a byte range to a new, potentially overlapping address.
    * 
    * @param  fromAddress   the address to copy from.
    * @param  toAddress     the address to copy to.
    * @param  size          the number of bytes to copy.
    * @group  bulk
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
    * @group  bulk
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
  
  override def alloc[T](count: Long)(implicit unit: ValType[T]): Data = {
    val size = unit.size * count
    if (size <= Int.MaxValue.toLong) unit.alignment match {
      case 1L => Data1(size)
      case 2L => Data2(size)
      case 4L => Data4(size)
      case _  => Data8(size)
    }
    else if (size <= (Int.MaxValue.toLong << 1)) unit.alignment match {
      case 1L | 2L => Data2(size)
      case 4L      => Data4(size)
      case _       => Data8(size)
    }
    else if (size <= (Int.MaxValue.toLong << 2)) unit.alignment match {
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
