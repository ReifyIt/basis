/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.memory

import basis.runtime._
import basis.util._

import scala.annotation.implicitNotFound

/** A typeclass for binary data structures.
  * 
  * A struct's `alignment`, `offset` and `size` constitute its ''frame''; the
  * alignment must evenly divide the offset and size. Structs must never access
  * more than `size - 1` bytes beyond a given address, and should assume proper
  * alignment of provided addresses.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.0
  * 
  * @tparam T   This struct's instance type.
  */
@implicitNotFound("No available Struct for type ${T}.")
abstract class Struct[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) T] extends ClassHint[T] {
  /** Returns the power-of-two alignment of this struct's frame. This alignment
    * must evenly divide all storage addresses used for values of this type. */
  def alignment: Long
  
  /** Returns the size in bytes of this struct's frame. The struct's alignment
    * must evenly divide this size. */
  def size: Long
  
  /** Loads a struct instance from data.
    * 
    * @param  data      the data to load from.
    * @param  address   the aligned address in `data` to load from.
    * @return the loaded instance.
    */
  def load(data: Data, address: Long): T
  
  /** Stores a struct instance as data.
    * 
    * @param  data      the data to store to.
    * @param  address   the aligned address in `data` to store to.
    * @param  value     the instance to store.
    */
  def store(data: Data, address: Long, value: T): Unit
  
  /** Reads a struct instance from a pointer.
    * 
    * @param  pointer   the pointer to load from.
    * @return the loaded instance.
    */
  def read(pointer: Pointer): T = Predef.???
  
  /** Writes a struct instance to a pointer.
    * 
    * @param  pointer   the pointer to store to.
    * @param  value     the instance to store.
    */
  def write(pointer: Pointer, value: T): Unit = Predef.???
}

/** A factory for builtin [[Struct structs]].
  * 
  * @groupname  Implicit    Implicit struct selection
  * @groupprio  Implicit    1
  * 
  * @groupname  Aligned     Aligned primitive structs
  * @groupprio  Aligned     2
  * 
  * @groupname  Unaligned   Unaligned primitive structs
  * @groupprio  Unaligned   3
  * 
  * @groupname  Volatile    Volatile primitive structs
  * @groupprio  Volatile    4
  * 
  * @groupname  Composite   Composite structs
  * @groupprio  Composite   5
  */
object Struct {
  import Predef.classOf
  import MurmurHash3._
  
  /** Returns the implicit struct for a given instance type.
    * @group Implicit */
  def apply[T](implicit T: Struct[T]): T.type = T
  
  /** A `Byte` struct.
    * @group Aligned */
  implicit object PackedByte extends Struct[Byte] {
    override def alignment: Long = 1L
    override def size: Long = 1L
    override def load(data: Data, address: Long): Byte = data.loadByte(address)
    override def store(data: Data, address: Long, value: Byte): Unit = data.storeByte(address, value)
    override def read(pointer: Pointer): Byte = pointer.readByte()
    override def write(pointer: Pointer, value: Byte): Unit = pointer.writeByte(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Byte.TYPE
    override def newArray(length: Int): Array[Byte] = new Array[Byte](length)
    override def toString: String = "PackedByte"
  }
  
  /** An unaligned `Short` struct.
    * @group Unaligned */
  object PackedShort extends Struct[Short] {
    override def alignment: Long = 1L
    override def size: Long = 2L
    override def load(data: Data, address: Long): Short = data.loadUnalignedShort(address)
    override def store(data: Data, address: Long, value: Short): Unit = data.storeUnalignedShort(address, value)
    override def read(pointer: Pointer): Short = pointer.readUnalignedShort()
    override def write(pointer: Pointer, value: Short): Unit = pointer.writeUnalignedShort(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Short.TYPE
    override def newArray(length: Int): Array[Short] = new Array[Short](length)
    override def toString: String = "PackedShort"
  }
  
  /** An unaligned `Int` struct.
    * @group Unaligned */
  object PackedInt extends Struct[Int] {
    override def alignment: Long = 1L
    override def size: Long = 4L
    override def load(data: Data, address: Long): Int = data.loadUnalignedInt(address)
    override def store(data: Data, address: Long, value: Int): Unit = data.storeUnalignedInt(address, value)
    override def read(pointer: Pointer): Int = pointer.readUnalignedInt()
    override def write(pointer: Pointer, value: Int): Unit = pointer.writeUnalignedInt(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Integer.TYPE
    override def newArray(length: Int): Array[Int] = new Array[Int](length)
    override def toString: String = "PackedInt"
  }
  
  /** An unaligned `Long` struct.
    * @group Unaligned */
  object PackedLong extends Struct[Long] {
    override def alignment: Long = 1L
    override def size: Long = 8L
    override def load(data: Data, address: Long): Long = data.loadUnalignedLong(address)
    override def store(data: Data, address: Long, value: Long): Unit = data.storeUnalignedLong(address, value)
    override def read(pointer: Pointer): Long = pointer.readUnalignedLong()
    override def write(pointer: Pointer, value: Long): Unit = pointer.writeUnalignedLong(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Long.TYPE
    override def newArray(length: Int): Array[Long] = new Array[Long](length)
    override def toString: String = "PackedLong"
  }
  
  /** An unaligned `Float` struct.
    * @group Unaligned */
  object PackedFloat extends Struct[Float] {
    override def alignment: Long = 1L
    override def size: Long = 4L
    override def load(data: Data, address: Long): Float = data.loadUnalignedFloat(address)
    override def store(data: Data, address: Long, value: Float): Unit = data.storeUnalignedFloat(address, value)
    override def read(pointer: Pointer): Float = pointer.readUnalignedFloat()
    override def write(pointer: Pointer, value: Float): Unit = pointer.writeUnalignedFloat(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Float.TYPE
    override def newArray(length: Int): Array[Float] = new Array[Float](length)
    override def toString: String = "PackedFloat"
  }
  
  /** An unaligned `Double` struct.
    * @group Unaligned */
  object PackedDouble extends Struct[Double] {
    override def alignment: Long = 1L
    override def size: Long = 8L
    override def load(data: Data, address: Long): Double = data.loadUnalignedDouble(address)
    override def store(data: Data, address: Long, value: Double): Unit = data.storeUnalignedDouble(address, value)
    override def read(pointer: Pointer): Double = pointer.readUnalignedDouble()
    override def write(pointer: Pointer, value: Double): Unit = pointer.writeUnalignedDouble(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Double.TYPE
    override def newArray(length: Int): Array[Double] = new Array[Double](length)
    override def toString: String = "PackedDouble"
  }
  
  /** A `Boolean` struct.
    * @group Aligned */
  implicit object PackedBoolean extends Struct[Boolean] {
    override def alignment: Long = 1L
    override def size: Long = 1L
    override def load(data: Data, address: Long): Boolean = decode(data.loadByte(address))
    override def store(data: Data, address: Long, value: Boolean): Unit = data.storeByte(address, encode(value))
    override def read(pointer: Pointer): Boolean = decode(pointer.readByte())
    override def write(pointer: Pointer, value: Boolean): Unit = pointer.writeByte(encode(value))
    @inline private[this] def decode(value: Byte): Boolean = value == 0
    @inline private[this] def encode(value: Boolean): Byte = if (value) 0.toByte else -1.toByte
    override def runtimeClass: java.lang.Class[_] = java.lang.Boolean.TYPE
    override def newArray(length: Int): Array[Boolean] = new Array[Boolean](length)
    override def toString: String = "PackedBoolean"
  }
  
  /** An aligned `Short` struct.
    * @group Aligned */
  implicit object PaddedShort extends Struct[Short] {
    override def alignment: Long = 2L
    override def size: Long = 2L
    override def load(data: Data, address: Long): Short = data.loadShort(address)
    override def store(data: Data, address: Long, value: Short): Unit = data.storeShort(address, value)
    override def read(pointer: Pointer): Short = pointer.readShort()
    override def write(pointer: Pointer, value: Short): Unit = pointer.writeShort(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Short.TYPE
    override def newArray(length: Int): Array[Short] = new Array[Short](length)
    override def toString: String = "PaddedShort"
  }
  
  /** An aligned `Int` struct.
    * @group Aligned */
  implicit object PaddedInt extends Struct[Int] {
    override def alignment: Long = 4L
    override def size: Long = 4L
    override def load(data: Data, address: Long): Int = data.loadInt(address)
    override def store(data: Data, address: Long, value: Int): Unit = data.storeInt(address, value)
    override def read(pointer: Pointer): Int = pointer.readInt()
    override def write(pointer: Pointer, value: Int): Unit = pointer.writeInt(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Integer.TYPE
    override def newArray(length: Int): Array[Int] = new Array[Int](length)
    override def toString: String = "PaddedInt"
  }
  
  /** An aligned `Long` struct.
    * @group Aligned */
  implicit object PaddedLong extends Struct[Long] {
    override def alignment: Long = 8L
    override def size: Long = 8L
    override def load(data: Data, address: Long): Long = data.loadLong(address)
    override def store(data: Data, address: Long, value: Long): Unit = data.storeLong(address, value)
    override def read(pointer: Pointer): Long = pointer.readLong()
    override def write(pointer: Pointer, value: Long): Unit = pointer.writeLong(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Long.TYPE
    override def newArray(length: Int): Array[Long] = new Array[Long](length)
    override def toString: String = "PaddedLong"
  }
  
  /** An aligned `Float` struct.
    * @group Aligned */
  implicit object PaddedFloat extends Struct[Float] {
    override def alignment: Long = 4L
    override def size: Long = 4L
    override def load(data: Data, address: Long): Float = data.loadFloat(address)
    override def store(data: Data, address: Long, value: Float): Unit = data.storeFloat(address, value)
    override def read(pointer: Pointer): Float = pointer.readFloat()
    override def write(pointer: Pointer, value: Float): Unit = pointer.writeFloat(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Float.TYPE
    override def newArray(length: Int): Array[Float] = new Array[Float](length)
    override def toString: String = "PaddedFloat"
  }
  
  /** An aligned `Double` struct.
    * @group Aligned */
  implicit object PaddedDouble extends Struct[Double] {
    override def alignment: Long = 8L
    override def size: Long = 8L
    override def load(data: Data, address: Long): Double = data.loadDouble(address)
    override def store(data: Data, address: Long, value: Double): Unit = data.storeDouble(address, value)
    override def read(pointer: Pointer): Double = pointer.readDouble()
    override def write(pointer: Pointer, value: Double): Unit = pointer.writeDouble(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Double.TYPE
    override def newArray(length: Int): Array[Double] = new Array[Double](length)
    override def toString: String = "PaddedDouble"
  }
  
  /** A `Byte` struct that supports volatile semantics when stored in coherent data.
    * @group Volatile */
  object VolatileByte extends Struct[Byte] {
    override def alignment: Long = 1L
    override def size: Long = 1L
    override def load(data: Data, address: Long): Byte = data.loadVolatileByte(address)
    override def store(data: Data, address: Long, value: Byte): Unit = data.storeVolatileByte(address, value)
    override def read(pointer: Pointer): Byte = pointer.readByte()
    override def write(pointer: Pointer, value: Byte): Unit = pointer.writeByte(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Byte.TYPE
    override def newArray(length: Int): Array[Byte] = new Array[Byte](length)
    override def toString: String = "VolatileByte"
  }
  
  /** An aligned `Short` struct that supports volatile semantics when stored in coherent data.
    * @group Volatile */
  object VolatileShort extends Struct[Short] {
    override def alignment: Long = 2L
    override def size: Long = 2L
    override def load(data: Data, address: Long): Short = data.loadVolatileShort(address)
    override def store(data: Data, address: Long, value: Short): Unit = data.storeVolatileShort(address, value)
    override def read(pointer: Pointer): Short = pointer.readShort()
    override def write(pointer: Pointer, value: Short): Unit = pointer.writeShort(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Short.TYPE
    override def newArray(length: Int): Array[Short] = new Array[Short](length)
    override def toString: String = "VolatileShort"
  }
  
  /** An aligned `Int` struct that supports volatile semantics when stored in coherent data.
    * @group Volatile */
  object VolatileInt extends Struct[Int] {
    override def alignment: Long = 4L
    override def size: Long = 4L
    override def load(data: Data, address: Long): Int = data.loadVolatileInt(address)
    override def store(data: Data, address: Long, value: Int): Unit = data.storeVolatileInt(address, value)
    override def read(pointer: Pointer): Int = pointer.readInt()
    override def write(pointer: Pointer, value: Int): Unit = pointer.writeInt(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Integer.TYPE
    override def newArray(length: Int): Array[Int] = new Array[Int](length)
    override def toString: String = "VolatileInt"
  }
  
  /** An aligned `Long` struct that supports volatile semantics when stored in coherent data.
    * @group Volatile */
  object VolatileLong extends Struct[Long] {
    override def alignment: Long = 8L
    override def size: Long = 8L
    override def load(data: Data, address: Long): Long = data.loadVolatileLong(address)
    override def store(data: Data, address: Long, value: Long): Unit = data.storeVolatileLong(address, value)
    override def read(pointer: Pointer): Long = pointer.readLong()
    override def write(pointer: Pointer, value: Long): Unit = pointer.writeLong(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Long.TYPE
    override def newArray(length: Int): Array[Long] = new Array[Long](length)
    override def toString: String = "VolatileLong"
  }
  
  /** An aligned `Float` struct that supports volatile semantics when stored in coherent data.
    * @group Volatile */
  object VolatileFloat extends Struct[Float] {
    override def alignment: Long = 4L
    override def size: Long = 4L
    override def load(data: Data, address: Long): Float = data.loadVolatileFloat(address)
    override def store(data: Data, address: Long, value: Float): Unit = data.storeVolatileFloat(address, value)
    override def read(pointer: Pointer): Float = pointer.readFloat()
    override def write(pointer: Pointer, value: Float): Unit = pointer.writeFloat(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Float.TYPE
    override def newArray(length: Int): Array[Float] = new Array[Float](length)
    override def toString: String = "VolatileFloat"
  }
  
  /** An aligned `Double` struct that supports volatile semantics when stored in coherent data.
    * @group Volatile */
  object VolatileDouble extends Struct[Double] {
    override def alignment: Long = 8L
    override def size: Long = 8L
    override def load(data: Data, address: Long): Double = data.loadVolatileDouble(address)
    override def store(data: Data, address: Long, value: Double): Unit = data.storeVolatileDouble(address, value)
    override def read(pointer: Pointer): Double = pointer.readDouble()
    override def write(pointer: Pointer, value: Double): Unit = pointer.writeDouble(value)
    override def runtimeClass: java.lang.Class[_] = java.lang.Double.TYPE
    override def newArray(length: Int): Array[Double] = new Array[Double](length)
    override def toString: String = "VolatileDouble"
  }
  
  /** A struct for 2-tuples of struct instances.
    * @group Composite */
  final class Record2
      [@specialized(Int, Long, Double, Boolean) T1, @specialized(Int, Long, Double, Boolean) T2]
      (implicit protected override val T1: Struct[T1], protected override val T2: Struct[T2])
    extends Struct[(T1, T2)] with Reified2 {
    private[this] val offset2: Long = align(T1.size, T2.alignment)
    override val alignment: Long = T1.alignment max T2.alignment
    override val size: Long = align(offset2 + T2.size, alignment)
    override def load(data: Data, address: Long): (T1, T2) = (
      T1.load(data, address),
      T2.load(data, address + offset2)
    )
    override def store(data: Data, address: Long, tuple: (T1, T2)) {
      T1.store(data, address,           tuple._1)
      T2.store(data, address + offset2, tuple._2)
    }
    override def read(pointer: Pointer): (T1, T2) = (
      T1.read(pointer),
      { pointer.align(T2.alignment); T2.read(pointer) }
    )
    override def write(pointer: Pointer, tuple: (T1, T2)) {
      T1.write(pointer, tuple._1)
      pointer.align(T2.alignment); T2.write(pointer, tuple._2)
    }
    override def runtimeClass: java.lang.Class[_] = classOf[(T1, T2)]
    override def newArray(length: Int): Array[(T1, T2)] = new Array[(T1, T2)](length)
    override def canEqual(other: Any): Boolean = other.isInstanceOf[Record2[_, _]]
    override def equals(other: Any): Boolean = other match {
      case that: Record2[_, _] => T1.equals(that.T1) && T2.equals(that.T2)
      case _ => false
    }
    override def hashCode: Int = mash(mix(mix(seed[Record2[_, _]], T1.hashCode), T2.hashCode))
    override def toString: String = "("+ T1 +", "+ T2 +")"
  }
  
  /** Returns a struct for a 2-tuple of struct instances.
    * @group Composite */
  implicit def Record2
      [@specialized(Int, Long, Double, Boolean) T1, @specialized(Int, Long, Double, Boolean) T2]
      (implicit T1: Struct[T1], T2: Struct[T2])
    : Record2[T1, T2] = new Record2
  
  /** A struct for 3-tuples of struct instances.
    * @group Composite */
  final class Record3[T1, T2, T3]
      (implicit protected override val T1: Struct[T1], protected override val T2: Struct[T2],
                protected override val T3: Struct[T3])
    extends Struct[(T1, T2, T3)] with Reified3 {
    private[this] val offset2: Long = align(T1.size, T2.alignment)
    private[this] val offset3: Long = align(offset2 + T2.size, T3.alignment)
    override val alignment: Long = T1.alignment max T2.alignment max T3.alignment
    override val size: Long = align(offset3 + T3.size, alignment)
    override def load(data: Data, address: Long): (T1, T2, T3) = (
      T1.load(data, address),
      T2.load(data, address + offset2),
      T3.load(data, address + offset3)
    )
    override def store(data: Data, address: Long, tuple: (T1, T2, T3)) {
      T1.store(data, address,           tuple._1)
      T2.store(data, address + offset2, tuple._2)
      T3.store(data, address + offset3, tuple._3)
    }
    override def read(pointer: Pointer): (T1, T2, T3) = (
      T1.read(pointer),
      { pointer.align(T2.alignment); T2.read(pointer) },
      { pointer.align(T3.alignment); T3.read(pointer) }
    )
    override def write(pointer: Pointer, tuple: (T1, T2, T3)) {
      T1.write(pointer, tuple._1)
      pointer.align(T2.alignment); T2.write(pointer, tuple._2)
      pointer.align(T3.alignment); T3.write(pointer, tuple._3)
    }
    override def runtimeClass: java.lang.Class[_] = classOf[(T1, T2, T3)]
    override def newArray(length: Int): Array[(T1, T2, T3)] = new Array[(T1, T2, T3)](length)
    override def canEqual(other: Any): Boolean = other.isInstanceOf[Record3[_, _, _]]
    override def equals(other: Any): Boolean = other match {
      case that: Record3[_, _, _] => T1.equals(that.T1) && T2.equals(that.T2) && T3.equals(that.T3)
      case _ => false
    }
    override def hashCode: Int = mash(mix(mix(mix(seed[Record3[_, _, _]], T1.hashCode), T2.hashCode), T3.hashCode))
    override def toString: String = "("+ T1 +", "+ T2 +", "+ T3 +")"
  }
  
  /** Returns a struct for a 3-tuple of struct instances.
    * @group Composite */
  implicit def Record3[T1, T2, T3]
      (implicit T1: Struct[T1], T2: Struct[T2], T3: Struct[T3])
    : Record3[T1, T2, T3] = new Record3
  
  /** A struct for 4-tuples of struct instances.
    * @group Composite */
  final class Record4[T1, T2, T3, T4]
      (implicit protected override val T1: Struct[T1], protected override val T2: Struct[T2],
                protected override val T3: Struct[T3], protected override val T4: Struct[T4])
    extends Struct[(T1, T2, T3, T4)] with Reified4 {
    private[this] val offset2: Long = align(T1.size, T2.alignment)
    private[this] val offset3: Long = align(offset2 + T2.size, T3.alignment)
    private[this] val offset4: Long = align(offset3 + T3.size, T4.alignment)
    override val alignment: Long = T1.alignment max T2.alignment max T3.alignment max T4.alignment
    override val size: Long = align(offset4 + T4.size, alignment)
    override def load(data: Data, address: Long): (T1, T2, T3, T4) = (
      T1.load(data, address),
      T2.load(data, address + offset2),
      T3.load(data, address + offset3),
      T4.load(data, address + offset4)
    )
    override def store(data: Data, address: Long, tuple: (T1, T2, T3, T4)) {
      T1.store(data, address,           tuple._1)
      T2.store(data, address + offset2, tuple._2)
      T3.store(data, address + offset3, tuple._3)
      T4.store(data, address + offset4, tuple._4)
    }
    override def read(pointer: Pointer): (T1, T2, T3, T4) = (
      T1.read(pointer),
      { pointer.align(T2.alignment); T2.read(pointer) },
      { pointer.align(T3.alignment); T3.read(pointer) },
      { pointer.align(T4.alignment); T4.read(pointer) }
    )
    override def write(pointer: Pointer, tuple: (T1, T2, T3, T4)) {
      T1.write(pointer, tuple._1)
      pointer.align(T2.alignment); T2.write(pointer, tuple._2)
      pointer.align(T3.alignment); T3.write(pointer, tuple._3)
      pointer.align(T4.alignment); T4.write(pointer, tuple._4)
    }
    override def runtimeClass: java.lang.Class[_] = classOf[(T1, T2, T3, T4)]
    override def newArray(length: Int): Array[(T1, T2, T3, T4)] = new Array[(T1, T2, T3, T4)](length)
    override def canEqual(other: Any): Boolean = other.isInstanceOf[Record4[_, _, _, _]]
    override def equals(other: Any): Boolean = other match {
      case that: Record4[_, _, _, _] =>
        T1.equals(that.T1) && T2.equals(that.T2) && T3.equals(that.T3) && T4.equals(that.T4)
      case _ => false
    }
    override def hashCode: Int =
      mash(mix(mix(mix(mix(seed[Record4[_, _, _, _]], T1.hashCode), T2.hashCode), T3.hashCode), T4.hashCode))
    override def toString: String = "("+ T1 +", "+ T2 +", "+ T3 +", "+ T4 +")"
  }
  
  /** Returns a struct for a 4-tuple of struct instances.
    * @group Composite */
  implicit def Record4[T1, T2, T3, T4]
      (implicit T1: Struct[T1], T2: Struct[T2], T3: Struct[T3], T4: Struct[T4])
    : Record4[T1, T2, T3, T4] = new Record4
}
