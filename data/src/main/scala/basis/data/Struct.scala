//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._

trait Struct[@specialized(Byte, Short, Int, Long, Float, Double, Boolean) T] {
  def alignment: Long

  def size: Long

  def load(data: Loader, offset: Long): T

  def store(data: Storer, offset: Long, value: T): Unit
}

object Struct {
  def apply[T](implicit T: Struct[T]): T.type = T

  implicit lazy val PackedByte: Struct[Byte]       = new PackedByte
  lazy val PackedShort: Struct[Short]              = new PackedShort
  lazy val PackedInt: Struct[Int]                  = new PackedInt
  lazy val PackedLong: Struct[Long]                = new PackedLong
  lazy val PackedFloat: Struct[Float]              = new PackedFloat
  lazy val PackedDouble: Struct[Double]            = new PackedDouble
  implicit lazy val PackedBoolean: Struct[Boolean] = new PackedBoolean

  implicit lazy val PaddedShort: Struct[Short]     = new PaddedShort
  implicit lazy val PaddedInt: Struct[Int]         = new PaddedInt
  implicit lazy val PaddedLong: Struct[Long]       = new PaddedLong
  implicit lazy val PaddedFloat: Struct[Float]     = new PaddedFloat
  implicit lazy val PaddedDouble: Struct[Double]   = new PaddedDouble

  lazy val VolatileByte: Struct[Byte]              = new VolatileByte
  lazy val VolatileShort: Struct[Short]            = new VolatileShort
  lazy val VolatileInt: Struct[Int]                = new VolatileInt
  lazy val VolatileLong: Struct[Long]              = new VolatileLong
  lazy val VolatileFloat: Struct[Float]            = new VolatileFloat
  lazy val VolatileDouble: Struct[Double]          = new VolatileDouble
  lazy val VolatileBoolean: Struct[Boolean]        = new VolatileBoolean

  lazy val PackedShortBE: Struct[Short]            = new PackedShortBE
  lazy val PackedShortLE: Struct[Short]            = new PackedShortLE
  lazy val PackedIntBE: Struct[Int]                = new PackedIntBE
  lazy val PackedIntLE: Struct[Int]                = new PackedIntLE
  lazy val PackedLongBE: Struct[Long]              = new PackedLongBE
  lazy val PackedLongLE: Struct[Long]              = new PackedLongLE
  lazy val PackedFloatBE: Struct[Float]            = new PackedFloatBE
  lazy val PackedFloatLE: Struct[Float]            = new PackedFloatLE
  lazy val PackedDoubleBE: Struct[Double]          = new PackedDoubleBE
  lazy val PackedDoubleLE: Struct[Double]          = new PackedDoubleLE

  implicit def Tuple2[T1: Struct, T2: Struct]: Struct[(T1, T2)]                                 = new Tuple2[T1, T2]
  implicit def Tuple3[T1: Struct, T2: Struct, T3: Struct]: Struct[(T1, T2, T3)]                 = new Tuple3[T1, T2, T3]
  implicit def Tuple4[T1: Struct, T2: Struct, T3: Struct, T4: Struct]: Struct[(T1, T2, T3, T4)] = new Tuple4[T1, T2, T3, T4]

  private final class PackedByte extends Struct[Byte] {
    override def alignment: Long                                      = 1L
    override def size: Long                                           = 1L
    override def load(data: Loader, offset: Long): Byte               = data.loadByte(offset)
    override def store(data: Storer, offset: Long, value: Byte): Unit = data.storeByte(offset, value)
    override def toString: String                                     = "PackedByte"
  }

  private final class PackedShort extends Struct[Short] {
    override def alignment: Long                                       = 1L
    override def size: Long                                            = 2L
    override def load(data: Loader, offset: Long): Short               = data.loadShort(offset)
    override def store(data: Storer, offset: Long, value: Short): Unit = data.storeShort(offset, value)
    override def toString: String                                      = "PackedShort"
  }

  private final class PackedInt extends Struct[Int] {
    override def alignment: Long                                     = 1L
    override def size: Long                                          = 4L
    override def load(data: Loader, offset: Long): Int               = data.loadInt(offset)
    override def store(data: Storer, offset: Long, value: Int): Unit = data.storeInt(offset, value)
    override def toString: String                                    = "PackedInt"
  }

  private final class PackedLong extends Struct[Long] {
    override def alignment: Long                                      = 1L
    override def size: Long                                           = 8L
    override def load(data: Loader, offset: Long): Long               = data.loadLong(offset)
    override def store(data: Storer, offset: Long, value: Long): Unit = data.storeLong(offset, value)
    override def toString: String                                     = "PackedLong"
  }

  private final class PackedFloat extends Struct[Float] {
    override def alignment: Long                                       = 1L
    override def size: Long                                            = 4L
    override def load(data: Loader, offset: Long): Float               = data.loadFloat(offset)
    override def store(data: Storer, offset: Long, value: Float): Unit = data.storeFloat(offset, value)
    override def toString: String                                      = "PackedFloat"
  }

  private final class PackedDouble extends Struct[Double] {
    override def alignment: Long                                        = 1L
    override def size: Long                                             = 8L
    override def load(data: Loader, offset: Long): Double               = data.loadDouble(offset)
    override def store(data: Storer, offset: Long, value: Double): Unit = data.storeDouble(offset, value)
    override def toString: String                                       = "PackedDouble"
  }

  private final class PackedBoolean extends Struct[Boolean] {
    override def alignment: Long                                         = 1L
    override def size: Long                                              = 1L
    override def load(data: Loader, offset: Long): Boolean               = data.loadByte(offset) == 0
    override def store(data: Storer, offset: Long, value: Boolean): Unit = data.storeByte(offset, if (value) 0 else -1)
    override def toString: String                                        = "PackedBoolean"
  }

  private final class PaddedShort extends Struct[Short] {
    override def alignment: Long                                       = 2L
    override def size: Long                                            = 2L
    override def load(data: Loader, offset: Long): Short               = data.loadAlignedShort(offset)
    override def store(data: Storer, offset: Long, value: Short): Unit = data.storeAlignedShort(offset, value)
    override def toString: String                                      = "PaddedShort"
  }

  private final class PaddedInt extends Struct[Int] {
    override def alignment: Long                                     = 4L
    override def size: Long                                          = 4L
    override def load(data: Loader, offset: Long): Int               = data.loadAlignedInt(offset)
    override def store(data: Storer, offset: Long, value: Int): Unit = data.storeAlignedInt(offset, value)
    override def toString: String                                    = "PaddedInt"
  }

  private final class PaddedLong extends Struct[Long] {
    override def alignment: Long                                      = 8L
    override def size: Long                                           = 8L
    override def load(data: Loader, offset: Long): Long               = data.loadAlignedLong(offset)
    override def store(data: Storer, offset: Long, value: Long): Unit = data.storeAlignedLong(offset, value)
    override def toString: String                                     = "PaddedLong"
  }

  private final class PaddedFloat extends Struct[Float] {
    override def alignment: Long                                       = 4L
    override def size: Long                                            = 4L
    override def load(data: Loader, offset: Long): Float               = data.loadAlignedFloat(offset)
    override def store(data: Storer, offset: Long, value: Float): Unit = data.storeAlignedFloat(offset, value)
    override def toString: String                                      = "PaddedFloat"
  }

  private final class PaddedDouble extends Struct[Double] {
    override def alignment: Long                                        = 8L
    override def size: Long                                             = 8L
    override def load(data: Loader, offset: Long): Double               = data.loadAlignedDouble(offset)
    override def store(data: Storer, offset: Long, value: Double): Unit = data.storeAlignedDouble(offset, value)
    override def toString: String                                       = "PaddedDouble"
  }

  private final class VolatileByte extends Struct[Byte] {
    override def alignment: Long                                      = 1L
    override def size: Long                                           = 1L
    override def load(data: Loader, offset: Long): Byte               = data.loadVolatileByte(offset)
    override def store(data: Storer, offset: Long, value: Byte): Unit = data.storeVolatileByte(offset, value)
    override def toString: String                                     = "VolatileByte"
  }

  private final class VolatileShort extends Struct[Short] {
    override def alignment: Long                                       = 2L
    override def size: Long                                            = 2L
    override def load(data: Loader, offset: Long): Short               = data.loadVolatileShort(offset)
    override def store(data: Storer, offset: Long, value: Short): Unit = data.storeVolatileShort(offset, value)
    override def toString: String                                      = "VolatileShort"
  }

  private final class VolatileInt extends Struct[Int] {
    override def alignment: Long                                     = 4L
    override def size: Long                                          = 4L
    override def load(data: Loader, offset: Long): Int               = data.loadVolatileInt(offset)
    override def store(data: Storer, offset: Long, value: Int): Unit = data.storeVolatileInt(offset, value)
    override def toString: String                                    = "VolatileInt"
  }

  private final class VolatileLong extends Struct[Long] {
    override def alignment: Long                                      = 8L
    override def size: Long                                           = 8L
    override def load(data: Loader, offset: Long): Long               = data.loadVolatileLong(offset)
    override def store(data: Storer, offset: Long, value: Long): Unit = data.storeVolatileLong(offset, value)
    override def toString: String                                     = "VolatileLong"
  }

  private final class VolatileFloat extends Struct[Float] {
    override def alignment: Long                                       = 4L
    override def size: Long                                            = 4L
    override def load(data: Loader, offset: Long): Float               = data.loadVolatileFloat(offset)
    override def store(data: Storer, offset: Long, value: Float): Unit = data.storeVolatileFloat(offset, value)
    override def toString: String                                      = "VolatileFloat"
  }

  private final class VolatileDouble extends Struct[Double] {
    override def alignment: Long                                        = 8L
    override def size: Long                                             = 8L
    override def load(data: Loader, offset: Long): Double               = data.loadVolatileDouble(offset)
    override def store(data: Storer, offset: Long, value: Double): Unit = data.storeVolatileDouble(offset, value)
    override def toString: String                                       = "VolatileDouble"
  }

  private final class VolatileBoolean extends Struct[Boolean] {
    override def alignment: Long                                         = 1L
    override def size: Long                                              = 1L
    override def load(data: Loader, offset: Long): Boolean               = data.loadVolatileByte(offset) == 0
    override def store(data: Storer, offset: Long, value: Boolean): Unit = data.storeVolatileByte(offset, if (value) 0 else -1)
    override def toString: String                                        = "VolatileBoolean"
  }

  private final class PackedShortBE extends Struct[Short] {
    override def alignment: Long                                       = 1L
    override def size: Long                                            = 2L
    override def load(data: Loader, offset: Long): Short               = new LoaderOps(data).loadShortBE(offset)
    override def store(data: Storer, offset: Long, value: Short): Unit = new StorerOps(data).storeShortBE(offset, value)
    override def toString: String                                      = "PackedShortBE"
  }

  private final class PackedShortLE extends Struct[Short] {
    override def alignment: Long                                       = 1L
    override def size: Long                                            = 2L
    override def load(data: Loader, offset: Long): Short               = new LoaderOps(data).loadShortLE(offset)
    override def store(data: Storer, offset: Long, value: Short): Unit = new StorerOps(data).storeShortLE(offset, value)
    override def toString: String                                      = "PackedShortLE"
  }

  private final class PackedIntBE extends Struct[Int] {
    override def alignment: Long                                     = 1L
    override def size: Long                                          = 4L
    override def load(data: Loader, offset: Long): Int               = new LoaderOps(data).loadIntBE(offset)
    override def store(data: Storer, offset: Long, value: Int): Unit = new StorerOps(data).storeIntBE(offset, value)
    override def toString: String                                    = "PackedIntBE"
  }

  private final class PackedIntLE extends Struct[Int] {
    override def alignment: Long                                     = 1L
    override def size: Long                                          = 4L
    override def load(data: Loader, offset: Long): Int               = new LoaderOps(data).loadIntLE(offset)
    override def store(data: Storer, offset: Long, value: Int): Unit = new StorerOps(data).storeIntLE(offset, value)
    override def toString: String                                    = "PackedIntLE"
  }

  private final class PackedLongBE extends Struct[Long] {
    override def alignment: Long                                      = 1L
    override def size: Long                                           = 8L
    override def load(data: Loader, offset: Long): Long               = new LoaderOps(data).loadLongBE(offset)
    override def store(data: Storer, offset: Long, value: Long): Unit = new StorerOps(data).storeLongBE(offset, value)
    override def toString: String                                     = "PackedLongBE"
  }

  private final class PackedLongLE extends Struct[Long] {
    override def alignment: Long                                      = 1L
    override def size: Long                                           = 8L
    override def load(data: Loader, offset: Long): Long               = new LoaderOps(data).loadLongLE(offset)
    override def store(data: Storer, offset: Long, value: Long): Unit = new StorerOps(data).storeLongLE(offset, value)
    override def toString: String                                     = "PackedLongLE"
  }

  private final class PackedFloatBE extends Struct[Float] {
    override def alignment: Long                                       = 1L
    override def size: Long                                            = 4L
    override def load(data: Loader, offset: Long): Float               = new LoaderOps(data).loadFloatBE(offset)
    override def store(data: Storer, offset: Long, value: Float): Unit = new StorerOps(data).storeFloatBE(offset, value)
    override def toString: String                                      = "PackedFloatBE"
  }

  private final class PackedFloatLE extends Struct[Float] {
    override def alignment: Long                                       = 1L
    override def size: Long                                            = 4L
    override def load(data: Loader, offset: Long): Float               = new LoaderOps(data).loadFloatLE(offset)
    override def store(data: Storer, offset: Long, value: Float): Unit = new StorerOps(data).storeFloatLE(offset, value)
    override def toString: String                                      = "PackedFloatLE"
  }

  private final class PackedDoubleBE extends Struct[Double] {
    override def alignment: Long                                        = 1L
    override def size: Long                                             = 8L
    override def load(data: Loader, offset: Long): Double               = new LoaderOps(data).loadDoubleBE(offset)
    override def store(data: Storer, offset: Long, value: Double): Unit = new StorerOps(data).storeDoubleBE(offset, value)
    override def toString: String                                       = "PackedDoubleBE"
  }

  private final class PackedDoubleLE extends Struct[Double] {
    override def alignment: Long                                        = 1L
    override def size: Long                                             = 8L
    override def load(data: Loader, offset: Long): Double               = new LoaderOps(data).loadDoubleLE(offset)
    override def store(data: Storer, offset: Long, value: Double): Unit = new StorerOps(data).storeDoubleLE(offset, value)
    override def toString: String                                       = "PackedDoubleLE"
  }

  private final class Tuple2[T1, T2]
      (implicit protected val T1: Struct[T1], protected val T2: Struct[T2])
    extends Struct[(T1, T2)] {

    private[this] val offset2: Long = align(T1.size, T2.alignment)
    override val alignment: Long    = T1.alignment max T2.alignment
    override val size: Long         = align(offset2 + T2.size, alignment)

    override def load(data: Loader, offset: Long): (T1, T2) = {
      val _1 = T1.load(data, offset          )
      val _2 = T2.load(data, offset + offset2)
      (_1, _2)
    }

    override def store(data: Storer, offset: Long, tuple: (T1, T2)): Unit = {
      T1.store(data, offset          , tuple._1)
      T2.store(data, offset + offset2, tuple._2)
    }

    override def equals(other: Any): Boolean = other match {
      case that: Tuple2[_, _] => T1.equals(that.T1) && T2.equals(that.T2)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[Tuple2[_, _]], T1.hashCode), T2.hashCode))
    }

    override def toString: String = "Tuple2"+"("+ T1 +", "+ T2 +")"
  }

  private final class Tuple3[T1, T2, T3]
      (implicit protected val T1: Struct[T1], protected val T2: Struct[T2],
                protected val T3: Struct[T3])
    extends Struct[(T1, T2, T3)] {

    private[this] val offset2: Long = align(T1.size, T2.alignment)
    private[this] val offset3: Long = align(offset2 + T2.size, T3.alignment)
    override val alignment: Long    = T1.alignment max T2.alignment max T3.alignment
    override val size: Long         = align(offset3 + T3.size, alignment)

    override def load(data: Loader, offset: Long): (T1, T2, T3) = {
      val _1 = T1.load(data, offset          )
      val _2 = T2.load(data, offset + offset2)
      val _3 = T3.load(data, offset + offset3)
      (_1, _2, _3)
    }

    override def store(data: Storer, offset: Long, tuple: (T1, T2, T3)): Unit = {
      T1.store(data, offset          , tuple._1)
      T2.store(data, offset + offset2, tuple._2)
      T3.store(data, offset + offset3, tuple._3)
    }

    override def equals(other: Any): Boolean = other match {
      case that: Tuple3[_, _, _] => T1.equals(that.T1) && T2.equals(that.T2) && T3.equals(that.T3)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(mix(seed[Tuple3[_, _, _]], T1.hashCode), T2.hashCode), T3.hashCode))
    }

    override def toString: String = "Tuple3"+"("+ T1 +", "+ T2 +", "+ T3 +")"
  }

  private final class Tuple4[T1, T2, T3, T4]
      (implicit protected val T1: Struct[T1], protected val T2: Struct[T2],
                protected val T3: Struct[T3], protected val T4: Struct[T4])
    extends Struct[(T1, T2, T3, T4)] {

    private[this] val offset2: Long = align(T1.size, T2.alignment)
    private[this] val offset3: Long = align(offset2 + T2.size, T3.alignment)
    private[this] val offset4: Long = align(offset3 + T3.size, T4.alignment)
    override val alignment: Long    = T1.alignment max T2.alignment max T3.alignment max T4.alignment
    override val size: Long         = align(offset4 + T4.size, alignment)

    override def load(data: Loader, offset: Long): (T1, T2, T3, T4) = {
      val _1 = T1.load(data, offset          )
      val _2 = T2.load(data, offset + offset2)
      val _3 = T3.load(data, offset + offset3)
      val _4 = T4.load(data, offset + offset4)
      (_1, _2, _3, _4)
    }

    override def store(data: Storer, offset: Long, tuple: (T1, T2, T3, T4)): Unit = {
      T1.store(data, offset          , tuple._1)
      T2.store(data, offset + offset2, tuple._2)
      T3.store(data, offset + offset3, tuple._3)
      T4.store(data, offset + offset4, tuple._4)
    }

    override def equals(other: Any): Boolean = other match {
      case that: Tuple4[_, _, _, _] => T1.equals(that.T1) && T2.equals(that.T2) && T3.equals(that.T3) && T4.equals(that.T4)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(mix(mix(seed[Tuple4[_, _, _, _]], T1.hashCode), T2.hashCode), T3.hashCode), T4.hashCode))
    }

    override def toString: String = "Tuple4"+"("+ T1 +", "+ T2 +", "+ T3 +", "+ T4 +")"
  }
}
