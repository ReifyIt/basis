//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.text._
import basis.util._

trait Frame[@specialized(Byte, Short, Int, Long, Float, Double) T] {
  def read(data: Reader): T

  def write(data: Writer, value: T): Unit
}

object Frame {
  def apply[T](implicit T: Frame[T]): T.type = T

  implicit lazy val Int8: Frame[Byte]      = new Int8
  implicit lazy val Int16: Frame[Short]    = new Int16
  lazy val Int16BE: Frame[Short]           = new Int16BE
  lazy val Int16LE: Frame[Short]           = new Int16LE
  implicit lazy val Int32: Frame[Int]      = new Int32
  lazy val Int32BE: Frame[Int]             = new Int32BE
  lazy val Int32LE: Frame[Int]             = new Int32LE
  implicit lazy val Int64: Frame[Long]     = new Int64
  lazy val Int64BE: Frame[Long]            = new Int64BE
  lazy val Int64LE: Frame[Long]            = new Int64LE
  implicit lazy val Float32: Frame[Float]  = new Float32
  lazy val Float32BE: Frame[Float]         = new Float32BE
  lazy val Float32LE: Frame[Float]         = new Float32LE
  implicit lazy val Float64: Frame[Double] = new Float64
  lazy val Float64BE: Frame[Double]        = new Float64BE
  lazy val Float64LE: Frame[Double]        = new Float64LE
  implicit lazy val Bool: Frame[Boolean]   = new Bool

  implicit lazy val CString: Frame[String] = new CString

  implicit def Tuple2[T1: Frame, T2: Frame]: Frame[(T1, T2)]                               = new Tuple2[T1, T2]
  implicit def Tuple3[T1: Frame, T2: Frame, T3: Frame]: Frame[(T1, T2, T3)]                = new Tuple3[T1, T2, T3]
  implicit def Tuple4[T1: Frame, T2: Frame, T3: Frame, T4: Frame]: Frame[(T1, T2, T3, T4)] = new Tuple4[T1, T2, T3, T4]

  private final class Int8 extends Frame[Byte] {
    override def read(data: Reader): Byte               = data.readByte()
    override def write(data: Writer, value: Byte): Unit = data.writeByte(value)
    override def toString: String                       = "Int8"
  }

  private final class Int16 extends Frame[Short] {
    override def read(data: Reader): Short               = data.readShort()
    override def write(data: Writer, value: Short): Unit = data.writeShort(value)
    override def toString: String                        = "Int16"
  }

  private final class Int16BE extends Frame[Short] {
    override def read(data: Reader): Short               = new ReaderOps(data).readShortBE()
    override def write(data: Writer, value: Short): Unit = new WriterOps(data).writeShortBE(value)
    override def toString: String                        = "Int16BE"
  }

  private final class Int16LE extends Frame[Short] {
    override def read(data: Reader): Short               = new ReaderOps(data).readShortLE()
    override def write(data: Writer, value: Short): Unit = new WriterOps(data).writeShortLE(value)
    override def toString: String                        = "Int16LE"
  }

  private final class Int32 extends Frame[Int] {
    override def read(data: Reader): Int               = data.readInt()
    override def write(data: Writer, value: Int): Unit = data.writeInt(value)
    override def toString: String                      = "Int32"
  }

  private final class Int32BE extends Frame[Int] {
    override def read(data: Reader): Int               = new ReaderOps(data).readIntBE()
    override def write(data: Writer, value: Int): Unit = new WriterOps(data).writeIntBE(value)
    override def toString: String                      = "Int32BE"
  }

  private final class Int32LE extends Frame[Int] {
    override def read(data: Reader): Int               = new ReaderOps(data).readIntLE()
    override def write(data: Writer, value: Int): Unit = new WriterOps(data).writeIntLE(value)
    override def toString: String                      = "Int32LE"
  }

  private final class Int64 extends Frame[Long] {
    override def read(data: Reader): Long               = data.readLong()
    override def write(data: Writer, value: Long): Unit = data.writeLong(value)
    override def toString: String                       = "Int64"
  }

  private final class Int64BE extends Frame[Long] {
    override def read(data: Reader): Long               = new ReaderOps(data).readLongBE()
    override def write(data: Writer, value: Long): Unit = new WriterOps(data).writeLongBE(value)
    override def toString: String                       = "Int64BE"
  }

  private final class Int64LE extends Frame[Long] {
    override def read(data: Reader): Long               = new ReaderOps(data).readLongLE()
    override def write(data: Writer, value: Long): Unit = new WriterOps(data).writeLongLE(value)
    override def toString: String                       = "Int64LE"
  }

  private final class Float32 extends Frame[Float] {
    override def read(data: Reader): Float               = data.readFloat()
    override def write(data: Writer, value: Float): Unit = data.writeFloat(value)
    override def toString: String                        = "Float32"
  }

  private final class Float32BE extends Frame[Float] {
    override def read(data: Reader): Float               = new ReaderOps(data).readFloatBE()
    override def write(data: Writer, value: Float): Unit = new WriterOps(data).writeFloatBE(value)
    override def toString: String                        = "Float32BE"
  }

  private final class Float32LE extends Frame[Float] {
    override def read(data: Reader): Float               = new ReaderOps(data).readFloatLE()
    override def write(data: Writer, value: Float): Unit = new WriterOps(data).writeFloatLE(value)
    override def toString: String                        = "Float32LE"
  }

  private final class Float64 extends Frame[Double] {
    override def read(data: Reader): Double               = data.readDouble()
    override def write(data: Writer, value: Double): Unit = data.writeDouble(value)
    override def toString: String                         = "Float64"
  }

  private final class Float64BE extends Frame[Double] {
    override def read(data: Reader): Double               = new ReaderOps(data).readDoubleBE()
    override def write(data: Writer, value: Double): Unit = new WriterOps(data).writeDoubleBE(value)
    override def toString: String                         = "Float64BE"
  }

  private final class Float64LE extends Frame[Double] {
    override def read(data: Reader): Double               = new ReaderOps(data).readDoubleLE()
    override def write(data: Writer, value: Double): Unit = new WriterOps(data).writeDoubleLE(value)
    override def toString: String                         = "Float64LE"
  }

  private final class Bool extends Frame[Boolean] {
    override def read(data: Reader): Boolean               = data.readByte() == 0
    override def write(data: Writer, value: Boolean): Unit = data.writeByte(if (value) 0 else -1)
    override def toString: String                          = "Bool"
  }

  private final class CString extends Frame[String] {
    override def read(data: Reader): String = {
      val builder = UTF8.Decoder(UString.Builder)
      var b = data.readByte() & 0xFF
      while (b != 0) {
        builder.append(b)
        b = data.readByte() & 0xFF
      }
      builder.state.toString
    }

    override def write(data: Writer, value: String): Unit = {
      val cs = new UString(value).modifiedUTF8Iterator
      while (!cs.isEmpty) {
        data.writeByte(cs.head.toByte)
        cs.step()
      }
      data.writeByte(0)
    }

    override def toString: String = "CString"
  }

  private final class Tuple2[T1, T2]
      (implicit protected val T1: Frame[T1], protected val T2: Frame[T2])
    extends Frame[(T1, T2)] {

    override def read(data: Reader): (T1, T2) = {
      val _1 = T1.read(data)
      val _2 = T2.read(data)
      (_1, _2)
    }

    override def write(data: Writer, tuple: (T1, T2)): Unit = {
      T1.write(data, tuple._1)
      T2.write(data, tuple._2)
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
      (implicit protected val T1: Frame[T1], protected val T2: Frame[T2],
                protected val T3: Frame[T3])
    extends Frame[(T1, T2, T3)] {

    override def read(data: Reader): (T1, T2, T3) = {
      val _1 = T1.read(data)
      val _2 = T2.read(data)
      val _3 = T3.read(data)
      (_1, _2, _3)
    }

    override def write(data: Writer, tuple: (T1, T2, T3)): Unit = {
      T1.write(data, tuple._1)
      T2.write(data, tuple._2)
      T3.write(data, tuple._3)
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
      (implicit protected val T1: Frame[T1], protected val T2: Frame[T2],
                protected val T3: Frame[T3], protected val T4: Frame[T4])
    extends Frame[(T1, T2, T3, T4)] {

    override def read(data: Reader): (T1, T2, T3, T4) = {
      val _1 = T1.read(data)
      val _2 = T2.read(data)
      val _3 = T3.read(data)
      val _4 = T4.read(data)
      (_1, _2, _3, _4)
    }

    override def write(data: Writer, tuple: (T1, T2, T3, T4)): Unit = {
      T1.write(data, tuple._1)
      T2.write(data, tuple._2)
      T3.write(data, tuple._3)
      T4.write(data, tuple._4)
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
