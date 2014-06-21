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
  implicit lazy val Int32: Frame[Int]      = new Int32
  implicit lazy val Int64: Frame[Long]     = new Int64
  implicit lazy val Float32: Frame[Float]  = new Float32
  implicit lazy val Float64: Frame[Double] = new Float64
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

  private final class Int32 extends Frame[Int] {
    override def read(data: Reader): Int               = data.readInt()
    override def write(data: Writer, value: Int): Unit = data.writeInt(value)
    override def toString: String                      = "Int32"
  }

  private final class Int64 extends Frame[Long] {
    override def read(data: Reader): Long               = data.readLong()
    override def write(data: Writer, value: Long): Unit = data.writeLong(value)
    override def toString: String                       = "Int64"
  }

  private final class Float32 extends Frame[Float] {
    override def read(data: Reader): Float               = data.readFloat()
    override def write(data: Writer, value: Float): Unit = data.writeFloat(value)
    override def toString: String                        = "Float32"
  }

  private final class Float64 extends Frame[Double] {
    override def read(data: Reader): Double               = data.readDouble()
    override def write(data: Writer, value: Double): Unit = data.writeDouble(value)
    override def toString: String                         = "Float64"
  }

  private final class Bool extends Frame[Boolean] {
    override def read(data: Reader): Boolean               = data.readByte() == 0
    override def write(data: Writer, value: Boolean): Unit = data.writeByte(if (value) 0 else -1)
    override def toString: String                          = "Bool"
  }

  private final class CString extends Frame[String] {
    override def read(data: Reader): String = {
      val builder = UTF8.Builder(UString.Builder)
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
