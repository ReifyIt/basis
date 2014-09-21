//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.proto

import basis._
import basis.collections._
import basis.data._
import basis.text._
import basis.util._
import scala.annotation._
import scala.reflect._
import scala.runtime._

trait Protobuf[@specialized(Protobuf.Specialized) T] extends Frame[T] {
  def sizeOf(value: T): Int

  def wireType: Int
}

object Protobuf {
  object WireType {
    final val Varint  = 0
    final val Fixed64 = 1
    final val Message = 2
    final val Fixed32 = 5
  }

  trait Field[@specialized(Protobuf.Specialized) T] extends Protobuf[T] {
    def tag: Int

    def tpe: Protobuf[T]

    def key: Long = (tag.toLong << 3) | (tpe.wireType & 0x7).toLong

    def unapply(key: Long): Boolean = this.key == key

    def readValue(data: Reader): T = {
      if (tpe.wireType == WireType.Message) tpe.read(data.take(readVarint(data)))
      else tpe.read(data)
    }

    def writeKey(data: Writer): Unit = {
      writeVarint(data, key)
    }

    def writeValue(data: Writer, value: T): Unit = {
      if (tpe.wireType == WireType.Message) writeVarint(data, tpe.sizeOf(value))
      tpe.write(data, value)
    }

    override def read(data: Reader): T = {
      val key = readVarint(data)
      if ((key >>> 3).toInt != tag) throw new ProtobufException(s"expected tag $tag, but found tag ${(key >>> 3).toInt}")
      if ((key.toInt & 0x7) != tpe.wireType) throw new ProtobufException(s"expected wire type $wireType, but found wire type ${key.toInt & 0x7}")
      readValue(data)
    }

    override def write(data: Writer, value: T): Unit = {
      writeKey(data)
      writeValue(data, value)
    }

    override def sizeOf(value: T): Int = {
      sizeOfVarint((tag << 3) | tpe.wireType)                                        +
      (if (tpe.wireType == WireType.Message) sizeOfVarint(tpe.sizeOf(value)) else 0) +
      tpe.sizeOf(value)
    }

    override def wireType: Int = WireType.Message
  }

  trait Message[T] extends Protobuf[T] {
    protected def aggregateFields: immutable.HashTrieMap[Long, Field[_]] = macro ProtobufMacros.aggregateFields

    override def wireType: Int = WireType.Message
  }

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
  implicit lazy val Unit: Protobuf[Unit]     = new Blank
  implicit lazy val String: Protobuf[String] = new Text

  implicit def Bytes[Data <: Loader](implicit Data: DataFactory[Data]): Protobuf[Data] = new Bytes()(Data)

  implicit def Perhaps[T](implicit T: Protobuf[T]): Protobuf[Maybe[T]] = new Perhaps()(T)

  def Repeated[CC[X] <: Container[X], T](implicit CC: generic.CollectionFactory[CC], T: Protobuf[T]): Protobuf[CC[T]] = new RepeatedCollection()(CC, T)
  def Repeated[CC[X] <: Container[X], T](implicit CC: generic.ArrayFactory[CC], T: Protobuf[T], TTag: ClassTag[T]): Protobuf[CC[T]] = new RepeatedArray()(CC, T, TTag)

  def Required[@specialized(Protobuf.Specialized) T](tag: Int)(implicit T: Protobuf[T]): Field[T]             = new Required(tag)(T)
  def Optional[@specialized(Protobuf.Specialized) T](tag: Int)(implicit T: Protobuf[T]): Field[Maybe[T]]      = new Optional(tag)(T)
  def Default[@specialized(Protobuf.Specialized) T](tag: Int, default: T)(implicit T: Protobuf[T]): Field[T]  = new Default(tag, default)(T)

  def Unknown[T](key: Long, default: T)(implicit T: Protobuf[T]): Field[T] = new Unknown(key, default)(T)
  def Unknown[T](tag: Int, wireType: Int, default: T)(implicit T: Protobuf[T]): Field[T] = new Unknown(tag, wireType, default)(T)
  def Unknown(key: Long): Field[Unit] = Unknown(key, ())(Unit)
  def Unknown(tag: Int, wireType: Int): Field[Unit] = Unknown(tag, wireType, ())(Unit)

  def Union[T](fields: Field[S] forSome { type S <: T }*): Reader => Maybe[T] = {
    val builder = immutable.HashTrieMap.Builder[Long, Field[S] forSome { type S <: T }]
    val iter = fields.iterator
    while (iter.hasNext) {
      val field = iter.next()
      builder.append(field.key -> field)
    }
    new Union[T](builder.state)
  }

  private final class Varint extends Protobuf[Long] {
    override def read(data: Reader): Long               = readVarint(data)
    override def write(data: Writer, value: Long): Unit = writeVarint(data, value)
    override def sizeOf(value: Long): Int               = sizeOfVarint(value)
    override def wireType: Int                          = WireType.Varint
    override def toString: String                       = "Protobuf"+"."+"Varint"
  }

  private final class Int32 extends Protobuf[Int] {
    override def read(data: Reader): Int               = readVarint(data).toInt
    override def write(data: Writer, value: Int): Unit = writeVarint(data, value.toLong)
    override def sizeOf(value: Int): Int               = sizeOfVarint(value.toLong)
    override def wireType: Int                         = WireType.Varint
    override def toString: String                      = "Protobuf"+"."+"Int32"
  }

  private final class Int64 extends Protobuf[Long] {
    override def read(data: Reader): Long               = readVarint(data)
    override def write(data: Writer, value: Long): Unit = writeVarint(data, value)
    override def sizeOf(value: Long): Int               = sizeOfVarint(value)
    override def wireType: Int                          = WireType.Varint
    override def toString: String                       = "Protobuf"+"."+"Int64"
  }

  private final class UInt32 extends Protobuf[Int] {
    override def read(data: Reader): Int               = readVarint(data).toInt
    override def write(data: Writer, value: Int): Unit = writeVarint(data, value.toLong & 0xFFFFFFFFL)
    override def sizeOf(value: Int): Int               = sizeOfVarint(value.toLong & 0xFFFFFFFFL)
    override def wireType: Int                         = WireType.Varint
    override def toString: String                      = "Protobuf"+"."+"UInt32"
  }

  private final class UInt64 extends Protobuf[Long] {
    override def read(data: Reader): Long               = readVarint(data)
    override def write(data: Writer, value: Long): Unit = writeVarint(data, value.toLong)
    override def sizeOf(value: Long): Int               = sizeOfVarint(value)
    override def wireType: Int                          = WireType.Varint
    override def toString: String                       = "Protobuf"+"."+"UInt64"
  }

  private final class SInt32 extends Protobuf[Int] {
    override def read(data: Reader): Int               = { val n = readVarint(data); ((n >>> 1) ^ (n << 63 >> 63)).toInt }
    override def write(data: Writer, value: Int): Unit = writeVarint(data, (value.toLong << 1) ^ (value.toLong >> 31))
    override def sizeOf(value: Int): Int               = sizeOfVarint((value.toLong << 1) ^ (value.toLong >> 31))
    override def wireType: Int                         = WireType.Varint
    override def toString: String                      = "Protobuf"+"."+"SInt32"
  }

  private final class SInt64 extends Protobuf[Long] {
    override def read(data: Reader): Long               = { val n = readVarint(data); (n >>> 1) ^ (n << 63 >> 63) }
    override def write(data: Writer, value: Long): Unit = writeVarint(data, (value << 1) ^ (value >> 63))
    override def sizeOf(value: Long): Int               = sizeOfVarint((value << 1) ^ (value >> 63))
    override def wireType: Int                          = WireType.Varint
    override def toString: String                       = "Protobuf"+"."+"SInt64"
  }

  private final class Fixed32 extends Protobuf[Int] {
    override def read(data: Reader): Int               = new ReaderOps(data).readIntLE()
    override def write(data: Writer, value: Int): Unit = new WriterOps(data).writeIntLE(value)
    override def sizeOf(value: Int): Int               = 4
    override def wireType: Int                         = WireType.Fixed32
    override def toString: String                      = "Protobuf"+"."+"Fixed32"
  }

  private final class Fixed64 extends Protobuf[Long] {
    override def read(data: Reader): Long               = new ReaderOps(data).readLongLE()
    override def write(data: Writer, value: Long): Unit = new WriterOps(data).writeLongLE(value)
    override def sizeOf(value: Long): Int               = 8
    override def wireType: Int                          = WireType.Fixed64
    override def toString: String                       = "Protobuf"+"."+"Fixed64"
  }

  private final class SFixed32 extends Protobuf[Int] {
    override def read(data: Reader): Int               = new ReaderOps(data).readIntLE()
    override def write(data: Writer, value: Int): Unit = new WriterOps(data).writeIntLE(value)
    override def sizeOf(value: Int): Int               = 4
    override def wireType: Int                         = WireType.Fixed32
    override def toString: String                      = "Protobuf"+"."+"SFixed32"
  }

  private final class SFixed64 extends Protobuf[Long] {
    override def read(data: Reader): Long               = new ReaderOps(data).readLongLE()
    override def write(data: Writer, value: Long): Unit = new WriterOps(data).writeLongLE(value)
    override def sizeOf(value: Long): Int               = 8
    override def wireType: Int                          = WireType.Fixed64
    override def toString: String                       = "Protobuf"+"."+"SFixed64"
  }

  private final class Float32 extends Protobuf[Float] {
    override def read(data: Reader): Float               = new ReaderOps(data).readFloatLE()
    override def write(data: Writer, value: Float): Unit = new WriterOps(data).writeFloatLE(value)
    override def sizeOf(value: Float): Int               = 4
    override def wireType: Int                           = WireType.Fixed32
    override def toString: String                        = "Protobuf"+"."+"Float"
  }

  private final class Float64 extends Protobuf[Double] {
    override def read(data: Reader): Double               = new ReaderOps(data).readDoubleLE()
    override def write(data: Writer, value: Double): Unit = new WriterOps(data).writeDoubleLE(value)
    override def sizeOf(value: Double): Int               = 8
    override def wireType: Int                            = WireType.Fixed64
    override def toString: String                         = "Protobuf"+"."+"Double"
  }

  private final class Bool extends Protobuf[Boolean] {
    override def read(data: Reader): Boolean               = readVarint(data) != 0L
    override def write(data: Writer, value: Boolean): Unit = data.writeByte(if (value) 1.toByte else 0.toByte)
    override def sizeOf(value: Boolean): Int               = 1
    override def wireType: Int                             = WireType.Varint
    override def toString: String                          = "Protobuf"+"."+"Bool"
  }

  private final class Blank extends Protobuf[Unit] {
    override def read(data: Reader): Unit               = { data.drop(readVarint(data)); () }
    override def write(data: Writer, value: Unit): Unit = data.writeByte(0.toByte)
    override def sizeOf(value: Unit): Int               = 1
    override def wireType: Int                          = WireType.Message
    override def toString: String                       = "Protobuf"+"."+"Unit"
  }

  private final class Text extends Protobuf[String] {
    override def read(data: Reader): String = {
      val s = UTF8.Decoder(basis.text.String.Builder)
      while (!data.isEOF) s.append(data.readByte & 0xFF)
      s.state
    }

    override def write(data: Writer, value: String): Unit = {
      val cs = new UString(value).utf8Iterator
      while (!cs.isEmpty) {
        data.writeByte(cs.head.toByte)
        cs.step()
      }
    }

    override def sizeOf(value: String): Int = new UString(value).utf8Length

    override def wireType: Int = WireType.Message

    override def toString: String = "Protobuf"+"."+"String"
  }

  private final class Bytes[Data <: Loader](implicit val Data: DataFactory[Data]) extends Protobuf[Data] {
    override def read(data: Reader): Data = {
      val framer = Data.Framer
      while (!data.isEOF) framer.writeByte(data.readByte)
      framer.state
    }

    override def write(data: Writer, value: Data): Unit = data.writeData(value)

    override def sizeOf(value: Data): Int = value.size.toInt

    override def wireType: Int = WireType.Message

    override def equals(other: Any): Boolean = other match {
      case that: Bytes[_] => Data.equals(that.Data)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(seed[Bytes[_]], Data.hashCode))
    }

    override def toString: String = "Protobuf"+"."+"Bytes"+"("+ Data +")"
  }

  private final class Perhaps[@specialized(Protobuf.Specialized) T](implicit private val T: Protobuf[T]) extends Protobuf[Maybe[T]] {
    override def read(data: Reader): Maybe[T] = if (!data.isEOF) Bind(T.read(data)) else Trap

    override def write(data: Writer, maybe: Maybe[T]): Unit = if (maybe.canBind) T.write(data, maybe.bind)

    override def sizeOf(maybe: Maybe[T]): Int = if (maybe.canBind) T.sizeOf(maybe.bind) else 0

    override def wireType: Int = T.wireType

    override def equals(other: Any): Boolean = other match {
      case that: Perhaps[_] => T.equals(that.T)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(seed[Perhaps[_]], T.hashCode))
    }

    override def toString: String = "Protobuf"+"."+"Perhaps"+"("+ T +")"
  }

  private final class RepeatedCollection[T, CC[X] <: Container[X]](implicit private val CC: generic.CollectionFactory[CC], private val T: Protobuf[T]) extends Protobuf[CC[T]] {
    if (T.wireType == WireType.Message) throw new ProtobufException("unsupported repeated length delimited values")

    override def read(data: Reader): CC[T] = {
      val builder = CC.Builder[T]
      while (!data.isEOF) builder.append(T.read(data))
      builder.state
    }

    override def write(data: Writer, value: CC[T]): Unit = {
      val xs = value.iterator
      while (!xs.isEmpty) {
        T.write(data, xs.head)
        xs.step()
      }
    }

    override def sizeOf(value: CC[T]): Int = {
      var size = 0
      val xs = value.iterator
      while (!xs.isEmpty) {
        size += T.sizeOf(xs.head)
        xs.step()
      }
      size
    }

    override def wireType: Int = WireType.Message

    override def equals(other: Any): Boolean = other match {
      case that: RepeatedCollection[_, CC forSome { type CC[_] }] @unchecked => CC.equals(that.CC) && T.equals(that.T)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[RepeatedCollection[_, CC forSome { type CC[_] }]], CC.hashCode), T.hashCode))
    }

    override def toString: String = "Protobuf"+"."+"Repeated"+"("+ CC +", "+ T +")"
  }

  private final class RepeatedArray[T, CC[X] <: Container[X]](implicit private val CC: generic.ArrayFactory[CC], private val T: Protobuf[T], private val TTag: ClassTag[T]) extends Protobuf[CC[T]] {
    if (T.wireType == WireType.Message) throw new ProtobufException("unsupported repeated length delimited values")

    override def read(data: Reader): CC[T] = {
      val builder = CC.Builder[T]
      while (!data.isEOF) builder.append(T.read(data))
      builder.state
    }

    override def write(data: Writer, value: CC[T]): Unit = {
      val xs = value.iterator
      while (!xs.isEmpty) {
        T.write(data, xs.head)
        xs.step()
      }
    }

    override def sizeOf(value: CC[T]): Int = {
      var size = 0
      val xs = value.iterator
      while (!xs.isEmpty) {
        size += T.sizeOf(xs.head)
        xs.step()
      }
      size
    }

    override def wireType: Int = WireType.Message

    override def equals(other: Any): Boolean = other match {
      case that: RepeatedArray[_, CC forSome { type CC[_] }] @unchecked => CC.equals(that.CC) && T.equals(that.T) && TTag.equals(that.TTag)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(mix(seed[RepeatedArray[_, CC forSome { type CC[_] }]], CC.hashCode), T.hashCode), TTag.hashCode))
    }

    override def toString: String = "Protobuf"+"."+"Repeated"+"("+ CC +", "+ T +", "+ TTag +")"
  }

  private final class Required[@specialized(Protobuf.Specialized) T](override val tag: Int)(implicit override val tpe: Protobuf[T]) extends Field[T] {
    override def equals(other: Any): Boolean = other match {
      case that: Required[_] => tag == that.tag && tpe.equals(that.tpe)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[Required[_]], tag.##), tpe.hashCode))
    }

    override def toString: String = "Protobuf"+"."+"Required"+"("+ tag +")"+"("+ tpe +")"
  }

  private final class Optional[@specialized(Protobuf.Specialized) T](override val tag: Int)(implicit private val T: Protobuf[T]) extends Field[Maybe[T]] {
    override val tpe: Protobuf[Maybe[T]] = new Perhaps()(T)

    override def read(data: Reader): Maybe[T] = if (!data.isEOF) super.read(data) else Trap

    override def write(data: Writer, maybe: Maybe[T]): Unit = {
      if (maybe.canBind) {
        val value = maybe.bind
        writeVarint(data, (tag << 3) | T.wireType & 0x7)
        if (T.wireType == WireType.Message) writeVarint(data, T.sizeOf(value))
        T.write(data, value)
      }
    }

    override def sizeOf(maybe: Maybe[T]): Int = {
      if (maybe.canBind) {
        val value = maybe.bind
        sizeOfVarint((tag << 3) | T.wireType)                                      +
        (if (T.wireType == WireType.Message) sizeOfVarint(T.sizeOf(value)) else 0) +
        T.sizeOf(value)
      }
      else 0
    }

    override def equals(other: Any): Boolean = other match {
      case that: Optional[_] => tag == that.tag && T.equals(that.T)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(seed[Optional[_]], tag.##), T.hashCode))
    }

    override def toString: String = "Protobuf"+"."+"Optional"+"("+ tag +")"+"("+ T +")"
  }

  private final class Default[@specialized(Protobuf.Specialized) T](override val tag: Int, private val default: T)(implicit override val tpe: Protobuf[T]) extends Field[T] {
    override def write(data: Writer, value: T): Unit = {
      if (value != default) {
        writeVarint(data, (tag << 3) | tpe.wireType & 0x7)
        if (tpe.wireType == WireType.Message) writeVarint(data, tpe.sizeOf(value))
        tpe.write(data, value)
      }
    }

    override def sizeOf(value: T): Int = {
      if (value != default)
        sizeOfVarint((tag << 3) | tpe.wireType)                                        +
        (if (tpe.wireType == WireType.Message) sizeOfVarint(tpe.sizeOf(value)) else 0) +
        tpe.sizeOf(value)
      else 0
    }

    override def equals(other: Any): Boolean = other match {
      case that: Default[_] => tag == that.tag && default.equals(that.default) && tpe.equals(that.tpe)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(mix(seed[Default[_]], tag.##), default.hashCode), tpe.hashCode))
    }

    override def toString: String = "Protobuf"+"."+"Default"+"("+ tag +","+ default +")"+"("+ tpe +")"
  }

  private final class Unknown[T](override val key: Long, val default: T)(implicit override val tpe: Protobuf[T]) extends Field[T] {
    def this(tag: Int, wireType: Int, default: T)(implicit tpe: Protobuf[T]) = this((tag.toLong << 3) | (wireType & 0x7).toLong, default)(tpe)

    override def tag: Int = (key >>> 3).toInt

    override def readValue(data: Reader): T = {
      (key.toInt & 0x7) match {
        case 0 => readVarint(data)
        case 1 => data.readLong()
        case 2 => data.drop(readVarint(data))
        case 5 => data.readInt()
        case _ => throw new ProtobufException("unknown wire type: " + wireType)
      }
      default
    }

    override def writeKey(data: Writer): Unit = {
      writeVarint(data, key)
    }

    override def writeValue(data: Writer, value: T): Unit = {
      (key.toInt & 0x7) match {
        case 0 => writeVarint(data, 0L)
        case 1 => data.writeLong(0L)
        case 2 => writeVarint(data, 0L)
        case 5 => data.writeInt(0)
        case _ => throw new ProtobufException("unknown wire type: " + wireType)
      }
    }

    override def read(data: Reader): T = {
      val key = readVarint(data)
      if ((key >>> 3).toInt != (this.key >>> 3).toInt) throw new ProtobufException(s"expected tag $tag, but found tag ${(key >>> 3).toInt}")
      if ((key.toInt & 0x7) != (this.key.toInt & 0x7)) throw new ProtobufException(s"expected wire type $wireType, but found wire type ${key.toInt & 0x7}")
      readValue(data)
    }

    override def write(data: Writer, value: T): Unit = {
      writeKey(data)
      writeValue(data, value)
    }

    override def sizeOf(value: T): Int = (key & 0x7) match {
      case 0 => 1
      case 1 => 8
      case 2 => 1
      case 5 => 4
      case _ => throw new ProtobufException("unknown wire type: " + wireType)
    }

    override def equals(other: Any): Boolean = other match {
      case that: Unknown[_] => key == that.key && default.equals(that.default) && tpe.equals(that.tpe)
      case _ => false
    }

    override def hashCode: Int = {
      import MurmurHash3._
      mash(mix(mix(mix(seed[Unknown[_]], key.##), default.hashCode), tpe.hashCode))
    }

    override def toString: String =
      (basis.text.String.Builder ~ "Protobuf" ~ '.' ~ "Unknown" ~ '(' ~
        "tag"      ~ " = " ~> (key >>> 3).toInt ~ ", " ~
        "wireType" ~ " = " ~> (key.toInt & 0x7) ~ ", " ~
        "default"  ~ " = " ~> default           ~ ')').state
  }

  private final class Union[T](fields: immutable.HashTrieMap[Long, Field[S] forSome { type S <: T }]) extends AbstractFunction1[Reader, Maybe[T]] {
    override def apply(data: Reader): Maybe[T] = {
      val key = readVarint(data)
      val maybeField = fields.get(key)
      if (maybeField.canBind) Bind(maybeField.bind.readValue(data))
      else {
        val wireType = key.toInt & 0x7
        wireType match {
          case 0 => readVarint(data)
          case 1 => data.readLong()
          case 2 => data.drop(readVarint(data))
          case 5 => data.readInt()
          case _ => throw new ProtobufException("unknown wire type: " + wireType)
        }
        Trap
      }
    }

    override def toString: String = {
      val s = basis.text.String.Builder ~ "Protobuf" ~ '.' ~ "Union" ~ '('
      var i = 0
      for (field <- fields.values) {
        if (i > 0) s.append(", ")
        s.append(field.toString)
        i += 1
      }
      s.append(')')
      s.state
    }
  }

  private final def sizeOfVarint(value: Long): Int = (63 - value.countLeadingZeros) / 7 + 1

  private final def readVarint(data: Reader): Long = readVariant(data, 0L, 0)
  @tailrec private final def readVariant(data: Reader, word: Long, shift: Int): Long = {
    val b = data.readByte().toInt
    val value = if (shift < 64) word | ((b & 0x7F).toLong << shift) else word
    if ((b & 0x80) == 0) value
    else readVariant(data, value, shift + 7)
  }

  @tailrec private final def writeVarint(data: Writer, value: Long): Unit = {
    val rest = value >>> 7
    if (rest == 0L) data.writeByte((value.toInt & 0x7F).toByte)
    else {
      data.writeByte(((value.toInt & 0x7F) | 0x80).toByte)
      writeVarint(data, rest)
    }
  }

  protected[proto] final val Specialized = new Specializable.Group((scala.Int, scala.Long, scala.Float, scala.Double))
}
