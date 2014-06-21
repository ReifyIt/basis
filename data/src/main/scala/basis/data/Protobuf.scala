//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.util._
import scala.annotation._

object Protobuf {
  lazy val Variant: Frame[Long]  = new Variant
  lazy val Int32: Frame[Int]     = new Int32
  lazy val Int64: Frame[Long]    = new Int64
  lazy val UInt32: Frame[Int]    = new UInt32
  lazy val UInt64: Frame[Long]   = new UInt64
  lazy val SInt32: Frame[Int]    = new SInt32
  lazy val SInt64: Frame[Long]   = new SInt64
  lazy val Fixed32: Frame[Int]   = new Fixed32
  lazy val Fixed64: Frame[Long]  = new Fixed64
  lazy val SFixed32: Frame[Int]  = new SFixed32
  lazy val SFixed64: Frame[Long] = new SFixed64
  lazy val Float: Frame[Float]   = new Float32
  lazy val Double: Frame[Double] = new Float64
  lazy val Bool: Frame[Boolean]  = new Bool

  private final class Variant extends Frame[Long] {
    override def read(data: Reader): Long = readVariant(data)
    override def write(data: Writer, value: Long): Unit = writeVariant(data, value)
    override def toString: String = "Protobuf"+"."+"Variant"
  }

  private final class Int32 extends Frame[Int] {
    override def read(data: Reader): Int = readVariant(data).toInt
    override def write(data: Writer, value: Int): Unit = writeVariant(data, value.toLong)
    override def toString: String = "Protobuf"+"."+"Int32"
  }

  private final class Int64 extends Frame[Long] {
    override def read(data: Reader): Long = readVariant(data)
    override def write(data: Writer, value: Long): Unit = writeVariant(data, value.toLong)
    override def toString: String = "Protobuf"+"."+"Int64"
  }

  private final class UInt32 extends Frame[Int] {
    override def read(data: Reader): Int = readVariant(data).toInt
    override def write(data: Writer, value: Int): Unit = writeVariant(data, value.toLong)
    override def toString: String = "Protobuf"+"."+"UInt32"
  }

  private final class UInt64 extends Frame[Long] {
    override def read(data: Reader): Long = readVariant(data)
    override def write(data: Writer, value: Long): Unit = writeVariant(data, value.toLong)
    override def toString: String = "Protobuf"+"."+"UInt64"
  }

  private final class SInt32 extends Frame[Int] {
    override def read(data: Reader): Int = { val n = readVariant(data); ((n >> 1) ^ (n << 31)).toInt }
    override def write(data: Writer, value: Int): Unit = writeVariant(data, (value.toLong << 1) ^ (value.toLong >> 31))
    override def toString: String = "Protobuf"+"."+"SInt32"
  }

  private final class SInt64 extends Frame[Long] {
    override def read(data: Reader): Long = { val n = readVariant(data); (n >> 1) ^ (n << 63) }
    override def write(data: Writer, value: Long): Unit = writeVariant(data, (value << 1) ^ (value >> 63))
    override def toString: String = "Protobuf"+"."+"SInt64"
  }

  private final class Fixed32 extends Frame[Int] {
    override def read(data: Reader): Int = read32(data)
    override def write(data: Writer, value: Int): Unit = write32(data, value)
    override def toString: String = "Protobuf"+"."+"Fixed32"
  }

  private final class Fixed64 extends Frame[Long] {
    override def read(data: Reader): Long = read64(data)
    override def write(data: Writer, value: Long): Unit = write64(data, value)
    override def toString: String = "Protobuf"+"."+"Fixed64"
  }

  private final class SFixed32 extends Frame[Int] {
    override def read(data: Reader): Int = read32(data)
    override def write(data: Writer, value: Int): Unit = write32(data, value)
    override def toString: String = "Protobuf"+"."+"SFixed32"
  }

  private final class SFixed64 extends Frame[Long] {
    override def read(data: Reader): Long = read64(data)
    override def write(data: Writer, value: Long): Unit = write64(data, value)
    override def toString: String = "Protobuf"+"."+"SFixed64"
  }

  private final class Float32 extends Frame[Float] {
    override def read(data: Reader): Float = read32(data).toFloatBits
    override def write(data: Writer, value: Float): Unit = write32(data, value.toRawIntBits)
    override def toString: String = "Protobuf"+"."+"Float"
  }

  private final class Float64 extends Frame[Double] {
    override def read(data: Reader): Double = read64(data).toDoubleBits
    override def write(data: Writer, value: Double): Unit = write64(data, value.toRawLongBits)
    override def toString: String = "Protobuf"+"."+"Double"
  }

  private final class Bool extends Frame[Boolean] {
    override def read(data: Reader): Boolean = readVariant(data) != 0L
    override def write(data: Writer, value: Boolean): Unit = writeVariant(data, if (value) 1L else 0L)
    override def toString: String = "Protobuf"+"."+"Bool"
  }

  private final def readVariant(data: Reader): Long = {
    var value = 0L
    var b = 0
    var k = 0
    do {
      b = data.readByte().toInt
      value |= (b & 0x7F).toLong << k
      k += 7
    } while ((b & 0x80) != 0)
    value
  }

  @tailrec private final def writeVariant(data: Writer, value: Long): Unit = {
    val rest = value >>> 7
    if (rest == 0L) data.writeByte((value.toInt & 0x7F).toByte)
    else {
      data.writeByte(((value.toInt & 0x7F) | 0x80).toByte)
      writeVariant(data, rest)
    }
  }

  private final def read32(data: Reader): Int = {
    ((data.readByte() & 0xFF)      ) |
    ((data.readByte() & 0xFF) <<  8) |
    ((data.readByte() & 0xFF) << 16) |
    ((data.readByte()       ) << 24)
  }

  private final def write32(data: Writer, value: Int): Unit = {
    data.writeByte((value      ).toByte)
    data.writeByte((value >>  8).toByte)
    data.writeByte((value >> 16).toByte)
    data.writeByte((value >> 24).toByte)
  }

  private final def read64(data: Reader): Long = {
    ((data.readByte() & 0xFF).toLong      ) |
    ((data.readByte() & 0xFF).toLong <<  8) |
    ((data.readByte() & 0xFF).toLong << 16) |
    ((data.readByte() & 0xFF).toLong << 24) |
    ((data.readByte() & 0xFF).toLong << 32) |
    ((data.readByte() & 0xFF).toLong << 40) |
    ((data.readByte() & 0xFF).toLong << 48) |
    ((data.readByte()       ).toLong << 56)
  }

  private final def write64(data: Writer, value: Long): Unit = {
    data.writeByte((value      ).toByte)
    data.writeByte((value >>  8).toByte)
    data.writeByte((value >> 16).toByte)
    data.writeByte((value >> 24).toByte)
    data.writeByte((value >> 32).toByte)
    data.writeByte((value >> 40).toByte)
    data.writeByte((value >> 48).toByte)
    data.writeByte((value >> 56).toByte)
  }
}
