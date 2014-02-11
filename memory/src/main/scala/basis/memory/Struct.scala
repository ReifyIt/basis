//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.memory

import basis.util._
import scala.annotation._

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
@implicitNotFound("No implicit Struct available for type ${T}.")
abstract class Struct[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) T] {
  /** Returns the power-of-two alignment of this struct's frame. This alignment
    * must evenly divide all storage addresses used for values of this type. */
  def alignment: Long

  /** Returns the size in bytes of this struct's frame. The struct's alignment
    * must evenly divide this size. */
  def size: Long

  /** Loads a struct instance from data.
    *
    * @param  data    the data to load from.
    * @param  offset  the aligned offset to load from.
    * @return the loaded instance.
    */
  def load(data: Loader, offset: Long): T

  /** Stores a struct instance as data.
    *
    * @param  data    the data to store to.
    * @param  offset  the aligned offset to store to.
    * @param  value   the instance to store.
    */
  def store(data: Storer, offset: Long, value: T): Unit

  def newArray(length: Int): Array[T]
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
  import MurmurHash3._

  /** Returns the implicit struct for a given instance type.
    * @group Implicit */
  def apply[T](implicit T: Struct[T]): T.type = T

  /** A `Byte` struct.
    * @group Aligned */
  implicit object PackedByte extends Struct[Byte] {
    override def alignment: Long = 1L
    override def size: Long = 1L
    override def load(data: Loader, offset: Long): Byte = data.loadByte(offset)
    override def store(data: Storer, offset: Long, value: Byte): Unit = data.storeByte(offset, value)
    override def newArray(length: Int): Array[Byte] = new Array[Byte](length)
    override def toString: String = "PackedByte"
  }

  /** An unaligned `Short` struct.
    * @group Unaligned */
  object PackedShort extends Struct[Short] {
    override def alignment: Long = 1L
    override def size: Long = 2L
    override def load(data: Loader, offset: Long): Short = data.loadUnalignedShort(offset)
    override def store(data: Storer, offset: Long, value: Short): Unit = data.storeUnalignedShort(offset, value)
    override def newArray(length: Int): Array[Short] = new Array[Short](length)
    override def toString: String = "PackedShort"
  }

  /** An unaligned `Int` struct.
    * @group Unaligned */
  object PackedInt extends Struct[Int] {
    override def alignment: Long = 1L
    override def size: Long = 4L
    override def load(data: Loader, offset: Long): Int = data.loadUnalignedInt(offset)
    override def store(data: Storer, offset: Long, value: Int): Unit = data.storeUnalignedInt(offset, value)
    override def newArray(length: Int): Array[Int] = new Array[Int](length)
    override def toString: String = "PackedInt"
  }

  /** An unaligned `Long` struct.
    * @group Unaligned */
  object PackedLong extends Struct[Long] {
    override def alignment: Long = 1L
    override def size: Long = 8L
    override def load(data: Loader, offset: Long): Long = data.loadUnalignedLong(offset)
    override def store(data: Storer, offset: Long, value: Long): Unit = data.storeUnalignedLong(offset, value)
    override def newArray(length: Int): Array[Long] = new Array[Long](length)
    override def toString: String = "PackedLong"
  }

  /** An unaligned `Float` struct.
    * @group Unaligned */
  object PackedFloat extends Struct[Float] {
    override def alignment: Long = 1L
    override def size: Long = 4L
    override def load(data: Loader, offset: Long): Float = data.loadUnalignedFloat(offset)
    override def store(data: Storer, offset: Long, value: Float): Unit = data.storeUnalignedFloat(offset, value)
    override def newArray(length: Int): Array[Float] = new Array[Float](length)
    override def toString: String = "PackedFloat"
  }

  /** An unaligned `Double` struct.
    * @group Unaligned */
  object PackedDouble extends Struct[Double] {
    override def alignment: Long = 1L
    override def size: Long = 8L
    override def load(data: Loader, offset: Long): Double = data.loadUnalignedDouble(offset)
    override def store(data: Storer, offset: Long, value: Double): Unit = data.storeUnalignedDouble(offset, value)
    override def newArray(length: Int): Array[Double] = new Array[Double](length)
    override def toString: String = "PackedDouble"
  }

  /** A `Boolean` struct.
    * @group Aligned */
  implicit object PackedBoolean extends Struct[Boolean] {
    override def alignment: Long = 1L
    override def size: Long = 1L
    override def load(data: Loader, offset: Long): Boolean = decode(data.loadByte(offset))
    override def store(data: Storer, offset: Long, value: Boolean): Unit = data.storeByte(offset, encode(value))
    @inline private[this] def decode(value: Byte): Boolean = value == 0
    @inline private[this] def encode(value: Boolean): Byte = if (value) 0.toByte else -1.toByte
    override def newArray(length: Int): Array[Boolean] = new Array[Boolean](length)
    override def toString: String = "PackedBoolean"
  }

  /** An aligned `Short` struct.
    * @group Aligned */
  implicit object PaddedShort extends Struct[Short] {
    override def alignment: Long = 2L
    override def size: Long = 2L
    override def load(data: Loader, offset: Long): Short = data.loadShort(offset)
    override def store(data: Storer, offset: Long, value: Short): Unit = data.storeShort(offset, value)
    override def newArray(length: Int): Array[Short] = new Array[Short](length)
    override def toString: String = "PaddedShort"
  }

  /** An aligned `Int` struct.
    * @group Aligned */
  implicit object PaddedInt extends Struct[Int] {
    override def alignment: Long = 4L
    override def size: Long = 4L
    override def load(data: Loader, offset: Long): Int = data.loadInt(offset)
    override def store(data: Storer, offset: Long, value: Int): Unit = data.storeInt(offset, value)
    override def newArray(length: Int): Array[Int] = new Array[Int](length)
    override def toString: String = "PaddedInt"
  }

  /** An aligned `Long` struct.
    * @group Aligned */
  implicit object PaddedLong extends Struct[Long] {
    override def alignment: Long = 8L
    override def size: Long = 8L
    override def load(data: Loader, offset: Long): Long = data.loadLong(offset)
    override def store(data: Storer, offset: Long, value: Long): Unit = data.storeLong(offset, value)
    override def newArray(length: Int): Array[Long] = new Array[Long](length)
    override def toString: String = "PaddedLong"
  }

  /** An aligned `Float` struct.
    * @group Aligned */
  implicit object PaddedFloat extends Struct[Float] {
    override def alignment: Long = 4L
    override def size: Long = 4L
    override def load(data: Loader, offset: Long): Float = data.loadFloat(offset)
    override def store(data: Storer, offset: Long, value: Float): Unit = data.storeFloat(offset, value)
    override def newArray(length: Int): Array[Float] = new Array[Float](length)
    override def toString: String = "PaddedFloat"
  }

  /** An aligned `Double` struct.
    * @group Aligned */
  implicit object PaddedDouble extends Struct[Double] {
    override def alignment: Long = 8L
    override def size: Long = 8L
    override def load(data: Loader, offset: Long): Double = data.loadDouble(offset)
    override def store(data: Storer, offset: Long, value: Double): Unit = data.storeDouble(offset, value)
    override def newArray(length: Int): Array[Double] = new Array[Double](length)
    override def toString: String = "PaddedDouble"
  }

  /** A `Byte` struct that supports volatile semantics when stored in coherent data.
    * @group Volatile */
  object VolatileByte extends Struct[Byte] {
    override def alignment: Long = 1L
    override def size: Long = 1L
    override def load(data: Loader, offset: Long): Byte = data.loadVolatileByte(offset)
    override def store(data: Storer, offset: Long, value: Byte): Unit = data.storeVolatileByte(offset, value)
    override def newArray(length: Int): Array[Byte] = new Array[Byte](length)
    override def toString: String = "VolatileByte"
  }

  /** An aligned `Short` struct that supports volatile semantics when stored in coherent data.
    * @group Volatile */
  object VolatileShort extends Struct[Short] {
    override def alignment: Long = 2L
    override def size: Long = 2L
    override def load(data: Loader, offset: Long): Short = data.loadVolatileShort(offset)
    override def store(data: Storer, offset: Long, value: Short): Unit = data.storeVolatileShort(offset, value)
    override def newArray(length: Int): Array[Short] = new Array[Short](length)
    override def toString: String = "VolatileShort"
  }

  /** An aligned `Int` struct that supports volatile semantics when stored in coherent data.
    * @group Volatile */
  object VolatileInt extends Struct[Int] {
    override def alignment: Long = 4L
    override def size: Long = 4L
    override def load(data: Loader, offset: Long): Int = data.loadVolatileInt(offset)
    override def store(data: Storer, offset: Long, value: Int): Unit = data.storeVolatileInt(offset, value)
    override def newArray(length: Int): Array[Int] = new Array[Int](length)
    override def toString: String = "VolatileInt"
  }

  /** An aligned `Long` struct that supports volatile semantics when stored in coherent data.
    * @group Volatile */
  object VolatileLong extends Struct[Long] {
    override def alignment: Long = 8L
    override def size: Long = 8L
    override def load(data: Loader, offset: Long): Long = data.loadVolatileLong(offset)
    override def store(data: Storer, offset: Long, value: Long): Unit = data.storeVolatileLong(offset, value)
    override def newArray(length: Int): Array[Long] = new Array[Long](length)
    override def toString: String = "VolatileLong"
  }

  /** An aligned `Float` struct that supports volatile semantics when stored in coherent data.
    * @group Volatile */
  object VolatileFloat extends Struct[Float] {
    override def alignment: Long = 4L
    override def size: Long = 4L
    override def load(data: Loader, offset: Long): Float = data.loadVolatileFloat(offset)
    override def store(data: Storer, offset: Long, value: Float): Unit = data.storeVolatileFloat(offset, value)
    override def newArray(length: Int): Array[Float] = new Array[Float](length)
    override def toString: String = "VolatileFloat"
  }

  /** An aligned `Double` struct that supports volatile semantics when stored in coherent data.
    * @group Volatile */
  object VolatileDouble extends Struct[Double] {
    override def alignment: Long = 8L
    override def size: Long = 8L
    override def load(data: Loader, offset: Long): Double = data.loadVolatileDouble(offset)
    override def store(data: Storer, offset: Long, value: Double): Unit = data.storeVolatileDouble(offset, value)
    override def newArray(length: Int): Array[Double] = new Array[Double](length)
    override def toString: String = "VolatileDouble"
  }

  /** A `Boolean` struct that supports volatile semantics when stored in coherent data.
    * @group Volatile */
  object VolatileBoolean extends Struct[Boolean] {
    override def alignment: Long = 1L
    override def size: Long = 1L
    override def load(data: Loader, offset: Long): Boolean = decode(data.loadVolatileByte(offset))
    override def store(data: Storer, offset: Long, value: Boolean): Unit = data.storeVolatileByte(offset, encode(value))
    @inline private[this] def decode(value: Byte): Boolean = value == 0
    @inline private[this] def encode(value: Boolean): Byte = if (value) 0.toByte else -1.toByte
    override def newArray(length: Int): Array[Boolean] = new Array[Boolean](length)
    override def toString: String = "VolatileBoolean"
  }

  /** A struct for 2-tuples of struct instances.
    * @group Composite */
  final class Record2
      [@specialized(Int, Long, Double, Boolean) T1, @specialized(Int, Long, Double, Boolean) T2]
      (implicit protected val T1: Struct[T1], protected val T2: Struct[T2])
    extends Struct[(T1, T2)] {
    private[this] val offset2: Long = align(T1.size, T2.alignment)
    override val alignment: Long = T1.alignment max T2.alignment
    override val size: Long = align(offset2 + T2.size, alignment)
    override def load(data: Loader, offset: Long): (T1, T2) = (
      T1.load(data, offset),
      T2.load(data, offset + offset2)
    )
    override def store(data: Storer, offset: Long, tuple: (T1, T2)): Unit = {
      T1.store(data, offset,           tuple._1)
      T2.store(data, offset + offset2, tuple._2)
    }
    override def newArray(length: Int): Array[(T1, T2)] = new Array[(T1, T2)](length)
    override def equals(other: Any): Boolean = other match {
      case that: Record2[_, _] => T1.equals(that.T1) && T2.equals(that.T2)
      case _ => false
    }
    override def hashCode: Int = mash(mix(mix(seed[Record2[_, _]], T1.hashCode), T2.hashCode))
    override def toString: String = "("+ T1 +", "+ T2 +")"
  }

  /** Returns a struct for a 2-tuple of struct instances.
    * @group Composite */
  object Record2 {
    implicit def apply[@specialized(Int, Long, Double, Boolean) T1, @specialized(Int, Long, Double, Boolean) T2]
        (implicit T1: Struct[T1], T2: Struct[T2])
      : Record2[T1, T2] = new Record2
  }

  /** A struct for 3-tuples of struct instances.
    * @group Composite */
  final class Record3[T1, T2, T3]
      (implicit protected val T1: Struct[T1], protected val T2: Struct[T2],
                protected val T3: Struct[T3])
    extends Struct[(T1, T2, T3)] {
    private[this] val offset2: Long = align(T1.size, T2.alignment)
    private[this] val offset3: Long = align(offset2 + T2.size, T3.alignment)
    override val alignment: Long = T1.alignment max T2.alignment max T3.alignment
    override val size: Long = align(offset3 + T3.size, alignment)
    override def load(data: Loader, offset: Long): (T1, T2, T3) = (
      T1.load(data, offset),
      T2.load(data, offset + offset2),
      T3.load(data, offset + offset3)
    )
    override def store(data: Storer, offset: Long, tuple: (T1, T2, T3)): Unit = {
      T1.store(data, offset,           tuple._1)
      T2.store(data, offset + offset2, tuple._2)
      T3.store(data, offset + offset3, tuple._3)
    }
    override def newArray(length: Int): Array[(T1, T2, T3)] = new Array[(T1, T2, T3)](length)
    override def equals(other: Any): Boolean = other match {
      case that: Record3[_, _, _] => T1.equals(that.T1) && T2.equals(that.T2) && T3.equals(that.T3)
      case _ => false
    }
    override def hashCode: Int = mash(mix(mix(mix(seed[Record3[_, _, _]], T1.hashCode), T2.hashCode), T3.hashCode))
    override def toString: String = "("+ T1 +", "+ T2 +", "+ T3 +")"
  }

  /** Returns a struct for a 3-tuple of struct instances.
    * @group Composite */
  object Record3 {
    implicit def apply[T1, T2, T3](implicit T1: Struct[T1], T2: Struct[T2], T3: Struct[T3]): Record3[T1, T2, T3] = new Record3
  }

  /** A struct for 4-tuples of struct instances.
    * @group Composite */
  final class Record4[T1, T2, T3, T4]
      (implicit protected val T1: Struct[T1], protected val T2: Struct[T2],
                protected val T3: Struct[T3], protected val T4: Struct[T4])
    extends Struct[(T1, T2, T3, T4)] {
    private[this] val offset2: Long = align(T1.size, T2.alignment)
    private[this] val offset3: Long = align(offset2 + T2.size, T3.alignment)
    private[this] val offset4: Long = align(offset3 + T3.size, T4.alignment)
    override val alignment: Long = T1.alignment max T2.alignment max T3.alignment max T4.alignment
    override val size: Long = align(offset4 + T4.size, alignment)
    override def load(data: Loader, offset: Long): (T1, T2, T3, T4) = (
      T1.load(data, offset),
      T2.load(data, offset + offset2),
      T3.load(data, offset + offset3),
      T4.load(data, offset + offset4)
    )
    override def store(data: Storer, offset: Long, tuple: (T1, T2, T3, T4)): Unit = {
      T1.store(data, offset,           tuple._1)
      T2.store(data, offset + offset2, tuple._2)
      T3.store(data, offset + offset3, tuple._3)
      T4.store(data, offset + offset4, tuple._4)
    }
    override def newArray(length: Int): Array[(T1, T2, T3, T4)] = new Array[(T1, T2, T3, T4)](length)
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
  object Record4 {
    implicit def apply[T1, T2, T3, T4]
      (implicit T1: Struct[T1], T2: Struct[T2], T3: Struct[T3], T4: Struct[T4])
    : Record4[T1, T2, T3, T4] = new Record4
  }
}
