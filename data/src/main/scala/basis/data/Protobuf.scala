//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.data

import basis.collections._
import basis.text._
import basis.util._
import scala.annotation._
import scala.runtime._

trait Protobuf[@specialized(Int, Long, Float, Double) T] extends Frame[T] {
  def sizeOf(value: T): Int

  def wireType: Int
}

object Protobuf {
  def apply[T](implicit T: Protobuf[T]): T.type = T

  lazy val Varint: Protobuf[Long]            = new Varint
  implicit lazy val Int32: Protobuf[Int]     = new Int32
  implicit lazy val Int64: Protobuf[Long]    = new Int64
  lazy val UInt32: Protobuf[Int]             = new UInt32
  lazy val UInt64: Protobuf[Long]            = new UInt64
  lazy val SInt32: Protobuf[Int]             = new SInt32
  lazy val SInt64: Protobuf[Long]            = new SInt64
  lazy val Fixed32: Protobuf[Int]            = new Fixed32
  lazy val Fixed64: Protobuf[Long]           = new Fixed64
  lazy val SFixed32: Protobuf[Int]           = new SFixed32
  lazy val SFixed64: Protobuf[Long]          = new SFixed64
  implicit lazy val Float: Protobuf[Float]   = new Float32
  implicit lazy val Double: Protobuf[Double] = new Float64
  implicit lazy val Bool: Protobuf[Boolean]  = new Bool
  implicit lazy val String: Protobuf[String] = new Text

  def Field[@specialized(scala.Int, scala.Long, scala.Float, scala.Double) T](tag: Int)(implicit T: Protobuf[T]): Field[T] = new Field(tag)(T)

  def Union[T](fields: Field[S] forSome { type S <: T }*): Reader => Option[T] = {
    val builder = immutable.HashTrieMap.Builder[Int, Field[S] forSome { type S <: T }]
    val iter = fields.iterator
    while (iter.hasNext) {
      val field = iter.next()
      builder.append(field.tag -> field)
    }
    new Union[T](builder.state)
  }

  def Message(fields: (Field[S], S => Unit) forSome { type S }*): Reader => Unit = {
    val builder = immutable.HashTrieMap.Builder[Int, (Field[S], S => Unit) forSome { type S }]
    val iter = fields.iterator
    while (iter.hasNext) {
      val entry = iter.next()
      builder.append(entry._1.tag -> entry)
    }
    new Message(builder.state)
  }

  private final class Varint extends Protobuf[Long] {
    override def read(data: Reader): Long = readVarint(data)
    override def write(data: Writer, value: Long): Unit = writeVarint(data, value)
    override def sizeOf(value: Long): Int = sizeOfVarint(value)
    override def wireType: Int = 0
    override def toString: String = "Protobuf"+"."+"Varint"
  }

  private final class Int32 extends Protobuf[Int] {
    override def read(data: Reader): Int = readVarint(data).toInt
    override def write(data: Writer, value: Int): Unit = writeVarint(data, value.toLong)
    override def sizeOf(value: Int): Int = sizeOfVarint(value.toLong)
    override def wireType: Int = 0
    override def toString: String = "Protobuf"+"."+"Int32"
  }

  private final class Int64 extends Protobuf[Long] {
    override def read(data: Reader): Long = readVarint(data)
    override def write(data: Writer, value: Long): Unit = writeVarint(data, value)
    override def sizeOf(value: Long): Int = sizeOfVarint(value)
    override def wireType: Int = 0
    override def toString: String = "Protobuf"+"."+"Int64"
  }

  private final class UInt32 extends Protobuf[Int] {
    override def read(data: Reader): Int = readVarint(data).toInt
    override def write(data: Writer, value: Int): Unit = writeVarint(data, value.toLong)
    override def sizeOf(value: Int): Int = sizeOfVarint(value.toLong)
    override def wireType: Int = 0
    override def toString: String = "Protobuf"+"."+"UInt32"
  }

  private final class UInt64 extends Protobuf[Long] {
    override def read(data: Reader): Long = readVarint(data)
    override def write(data: Writer, value: Long): Unit = writeVarint(data, value.toLong)
    override def sizeOf(value: Long): Int = sizeOfVarint(value)
    override def wireType: Int = 0
    override def toString: String = "Protobuf"+"."+"UInt64"
  }

  private final class SInt32 extends Protobuf[Int] {
    override def read(data: Reader): Int = { val n = readVarint(data); ((n >> 1) ^ (n << 31)).toInt }
    override def write(data: Writer, value: Int): Unit = writeVarint(data, (value.toLong << 1) ^ (value.toLong >> 31))
    override def sizeOf(value: Int): Int = sizeOfVarint((value.toLong << 1) ^ (value.toLong >> 31))
    override def wireType: Int = 0
    override def toString: String = "Protobuf"+"."+"SInt32"
  }

  private final class SInt64 extends Protobuf[Long] {
    override def read(data: Reader): Long = { val n = readVarint(data); (n >> 1) ^ (n << 63) }
    override def write(data: Writer, value: Long): Unit = writeVarint(data, (value << 1) ^ (value >> 63))
    override def sizeOf(value: Long): Int = sizeOfVarint((value << 1) ^ (value >> 63))
    override def wireType: Int = 0
    override def toString: String = "Protobuf"+"."+"SInt64"
  }

  private final class Fixed32 extends Protobuf[Int] {
    override def read(data: Reader): Int = read32(data)
    override def write(data: Writer, value: Int): Unit = write32(data, value)
    override def sizeOf(value: Int): Int = 4
    override def wireType: Int = 5
    override def toString: String = "Protobuf"+"."+"Fixed32"
  }

  private final class Fixed64 extends Protobuf[Long] {
    override def read(data: Reader): Long = read64(data)
    override def write(data: Writer, value: Long): Unit = write64(data, value)
    override def sizeOf(value: Long): Int = 8
    override def wireType: Int = 1
    override def toString: String = "Protobuf"+"."+"Fixed64"
  }

  private final class SFixed32 extends Protobuf[Int] {
    override def read(data: Reader): Int = read32(data)
    override def write(data: Writer, value: Int): Unit = write32(data, value)
    override def sizeOf(value: Int): Int = 4
    override def wireType: Int = 5
    override def toString: String = "Protobuf"+"."+"SFixed32"
  }

  private final class SFixed64 extends Protobuf[Long] {
    override def read(data: Reader): Long = read64(data)
    override def write(data: Writer, value: Long): Unit = write64(data, value)
    override def sizeOf(value: Long): Int = 8
    override def wireType: Int = 1
    override def toString: String = "Protobuf"+"."+"SFixed64"
  }

  private final class Float32 extends Protobuf[Float] {
    override def read(data: Reader): Float = read32(data).toFloatBits
    override def write(data: Writer, value: Float): Unit = write32(data, value.toRawIntBits)
    override def sizeOf(value: Float): Int = 4
    override def wireType: Int = 5
    override def toString: String = "Protobuf"+"."+"Float"
  }

  private final class Float64 extends Protobuf[Double] {
    override def read(data: Reader): Double = read64(data).toDoubleBits
    override def write(data: Writer, value: Double): Unit = write64(data, value.toRawLongBits)
    override def sizeOf(value: Double): Int = 8
    override def wireType: Int = 1
    override def toString: String = "Protobuf"+"."+"Double"
  }

  private final class Bool extends Protobuf[Boolean] {
    override def read(data: Reader): Boolean = readVarint(data) != 0L
    override def write(data: Writer, value: Boolean): Unit = data.writeByte(if (value) 1.toByte else 0.toByte)
    override def sizeOf(value: Boolean): Int = 1
    override def wireType: Int = 0
    override def toString: String = "Protobuf"+"."+"Bool"
  }

  private final class Text extends Protobuf[String] {
    override def read(data: Reader): String = {
      val s = UTF8.Builder(UString.Builder)
      while (!data.isEOF) s.append(data.readByte & 0xFF)
      s.state.toString
    }

    override def write(data: Writer, value: String): Unit = {
      val cs = new UString(value).utf8Iterator
      while (!cs.isEmpty) {
        data.writeByte(cs.head.toByte)
        cs.step()
      }
    }

    override def sizeOf(value: String): Int = new UString(value).utf8Length

    override def wireType: Int = 2

    override def toString: String = "Protobuf"+"."+"String"
  }

  final class Field[@specialized(scala.Int, scala.Long, scala.Float, scala.Double) T](val tag: Int)(implicit val T: Protobuf[T]) extends Protobuf[T] {
    override def read(data: Reader): T = {
      val key = readVarint(data)
      if (key >>> 3 != tag) throw new RuntimeException(s"expected tag $tag, but found tag ${key >>> 3}")
      if ((key & 0x7) != T.wireType) throw new RuntimeException(s"expected wire type $wireType, but found wire type ${key & 0x7}")
      if (T.wireType == 2) T.read(data.take(readVarint(data)))
      else T.read(data)
    }

    override def write(data: Writer, value: T): Unit = {
      writeVarint(data, (tag << 3) | T.wireType & 0x7)
      if (T.wireType == 2) writeVarint(data, T.sizeOf(value))
      T.write(data, value)
    }

    override def sizeOf(value: T): Int = sizeOfVarint((tag << 3) | T.wireType) + T.sizeOf(value)

    override def wireType: Int = 2

    override def toString: String = "Protobuf"+"."+"Field"+"("+ tag +")"+"("+ T +")"
  }

  private final class Union[T](fields: immutable.HashTrieMap[Int, Field[S] forSome { type S <: T }]) extends AbstractFunction1[Reader, Option[T]] {
    override def apply(data: Reader): Option[T] = {
      val key = readVarint(data)
      val tag = (key >>> 3).toInt
      val wireType = key.toInt & 0x7
      if (fields.contains(tag)) Some {
        val field = fields(tag)
        if (tag != field.tag) throw new RuntimeException(s"expected tag ${field.tag}, but found tag $tag")
        if (wireType != field.T.wireType) throw new RuntimeException(s"expected wire type ${field.wireType}, but found wire type $wireType")
        if (wireType == 2) field.T.read(data.take(readVarint(data)))
        else field.T.read(data)
      }
      else {
        wireType match {
          case 0 => readVarint(data)
          case 1 => read64(data)
          case 2 => data.drop(readVarint(data))
          case 5 => read32(data)
          case _ => throw new RuntimeException("unknown wire type: " + wireType)
        }
        None
      }
    }

    override def toString: String = {
      val s = UString.Builder
      s.append("Protobuf"); s.append('.'); s.append("Union"); s.append('(')
      var i = 0
      for ((_, field) <- fields) {
        if (i > 0) s.append(", ")
        s.append(field.toString)
        i += 1
      }
      s.append(')')
      s.state.toString
    }
  }

  private final class Message(fields: immutable.HashTrieMap[Int, (Field[S], S => Unit) forSome { type S }]) extends AbstractFunction1[Reader, Unit] {
    override def apply(data: Reader): Unit = {
      while (!data.isEOF) {
        val key = readVarint(data)
        val tag = (key >>> 3).toInt
        val wireType = key.toInt & 0x7
        if (fields.contains(tag)) {
          val (field, action) = fields(tag)
          if (tag != field.tag) throw new RuntimeException(s"expected tag ${field.tag}, but found tag $tag")
          if (wireType != field.T.wireType) throw new RuntimeException(s"expected wire type ${field.wireType}, but found wire type $wireType")
          if (wireType == 2) action.asInstanceOf[Any => Unit](field.T.asInstanceOf[Protobuf[Any]].read(data.take(readVarint(data))))
          else action.asInstanceOf[Any => Unit](field.T.asInstanceOf[Protobuf[Any]].read(data))
        }
        else {
          wireType match {
            case 0 => readVarint(data)
            case 1 => read64(data)
            case 2 => data.drop(readVarint(data))
            case 5 => read32(data)
            case _ => throw new RuntimeException("unknown wire type: " + wireType)
          }
          ()
        }
      }
    }

    override def toString: String = "Protobuf"+"."+"Message"
  }

  private final def sizeOfVarint(value: Long): Int = (63 - value.countLeadingZeros) / 7 + 1

  private final def readVarint(data: Reader): Long = {
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

  @tailrec private final def writeVarint(data: Writer, value: Long): Unit = {
    val rest = value >>> 7
    if (rest == 0L) data.writeByte((value.toInt & 0x7F).toByte)
    else {
      data.writeByte(((value.toInt & 0x7F) | 0x80).toByte)
      writeVarint(data, rest)
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
