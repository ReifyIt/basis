//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis._
import basis.collections._
import basis.data._
import basis.text._
import basis.util._
import scala.annotation._

trait BsonVariant extends Variant { variant =>
  override type AnyForm    <: BsonValue
  override type ObjectForm <: BsonObject with AnyForm
  override type SeqForm    <: BsonSeq with AnyForm
  override type SetForm    <: BsonSet with AnyForm
  override type TextForm   <: BsonText with AnyForm
  override type DataForm   <: BsonData with AnyForm
  override type NumberForm <: BsonNumber with AnyForm
  override type DateForm   <: BsonDate with AnyForm
  override type BoolForm   <: BsonBool with AnyForm
  override type NullForm   <: BsonNull with AnyForm
  override type NoForm     <: BsonNo with AnyForm

  override val AnyForm: BsonValueFactory
  override val ObjectForm: BsonObjectFactory
  override val SeqForm: BsonSeqFactory
  override val SetForm: BsonSetFactory

  def BsonObjectValue(form: ObjectForm): AnyForm = form
  def BsonArrayValue(form: SeqForm): AnyForm     = form
  def BsonStringValue(form: TextForm): AnyForm   = form

  def BsonObjectBuilder: Builder[(String, AnyForm)] with State[ObjectForm] = ObjectForm.Builder
  def BsonStringBuilder: StringBuilder with State[TextForm]                = TextForm.Builder
  def BsonArrayBuilder: Builder[AnyForm] with State[SeqForm]               = SeqForm.Builder

  def BsonBinary(subtype: Byte, data: Array[Byte]): AnyForm = DataForm(data)
  def BsonBoolean(value: Boolean): AnyForm                  = BoolForm(value)
  def BsonDateTime(millis: Long): AnyForm                   = DateForm(millis)
  def BsonDBPointer(name: String, id: Array[Byte]): AnyForm = NoForm
  def BsonDouble(value: Double): AnyForm                    = NumberForm(value)
  def BsonInt32(value: Int): AnyForm                        = NumberForm(value)
  def BsonInt64(value: Long): AnyForm                       = NumberForm(value)
  def BsonJSCode(js: String): AnyForm                       = NoForm
  def BsonJSScope(js: String, scope: ObjectForm): AnyForm   = NoForm
  def BsonMaxKey: AnyForm                                   = NoForm
  def BsonMinKey: AnyForm                                   = NoForm
  def BsonNull: AnyForm                                     = NullForm
  def BsonObjectId(id: Array[Byte]): AnyForm                = NoForm
  def BsonRegex(pattern: String, options: String): AnyForm  = NoForm
  def BsonString(value: String): AnyForm                    = TextForm(value)
  def BsonSymbol(symbol: String): AnyForm                   = NoForm
  def BsonTimeStamp(value: Long): AnyForm                   = NoForm
  def BsonUndefined: AnyForm                                = NoForm

  protected[form] implicit def BsonReader(reader: Reader): BsonReader = new BsonReader(reader)
  protected[form] implicit def BsonWriter(writer: Writer): BsonWriter = new BsonWriter(writer)


  trait BsonValue extends BaseValue { this: AnyForm =>
    /** Returns the type code of this form's BSON representation. */
    def bsonType: Byte

    /** Returns the size in bytes of this form's BSON representation. */
    def bsonSize: Int

    /** Writes the serialized BSON representation of this form to `output`. */
    def writeBson(output: Writer): Unit

    /** Returns the serialized BSON representation of this form. */
    def toBson: Loader = {
      val output = ArrayDataLE.Framer.expect(bsonSize.toLong)
      writeBson(output)
      output.state
    }
  }

  trait BsonValueFactory extends BaseValueFactory {
    /** Reads a variant from BSON encoded `input`. */
    def readBson(input: Reader): AnyForm = BsonObjectValue(input.readBsonObject(BsonObjectBuilder))
  }


  trait BsonObject extends BsonValue with BaseObject { this: ObjectForm =>
    override def bsonType: Byte = 0x03

    private[this] var bsonLength: Int = -1
    override def bsonSize: Int = {
      if (bsonLength == -1) {
        var size = 4 // document length
        val fields = iterator
        while (!fields.isEmpty) {
          val field = fields.head
          if (field._2.isDefined) { // filter undefined fields
            size += 1 // type tag
            size += field._1.modifiedUTF8Length + 1 // key cstring
            size += field._2.bsonSize
          }
          fields.step()
        }
        size += 1 // document sentinel
        bsonLength = size
      }
      bsonLength
    }

    override def writeBson(output: Writer): Unit = {
      output.writeInt(bsonSize) // document length
      val fields = iterator
      while (!fields.isEmpty) {
        val field = fields.head
        if (field._2.isDefined) { // filter undefined fields
          output.writeByte(field._2.bsonType)
          output.writeCString(field._1)
          field._2.writeBson(output)
        }
        fields.step()
      }
      output.writeByte(0) // document sentinel
    }
  }

  trait BsonObjectFactory extends BaseObjectFactory {
    def readBson(input: Reader): ObjectForm = input.readBsonObject(Builder())
  }


  trait BsonSeq extends BsonValue with BaseSeq { this: SeqForm =>
    override def bsonType: Byte = 0x04

    private[this] var bsonLength: Int = -1
    override def bsonSize: Int = {
      if (bsonLength == -1) {
        var size = 4 // document length
        var i = 0
        val values = iterator
        while (!values.isEmpty) {
          val value = values.head
          if (value.isDefined) { // filter undefined values
            size += 1 // type tag
            size += (if (i == 0) 0 else i.toDouble.log10.toInt) + 2 // index cstring
            size += value.bsonSize
            i += 1
          }
          values.step()
        }
        size += 1 // document sentinel
        bsonLength = size
      }
      bsonLength
    }

    override def writeBson(output: Writer): Unit = {
      output.writeInt(bsonSize) // document length
      var i = 0
      val values = iterator
      while (!values.isEmpty) {
        val value = values.head
        if (value.isDefined) { // filter undefined values
          output.writeByte(value.bsonType)
          output.writeCString(i.toString) // worst array representation ever
          value.writeBson(output)
          i += 1
        }
        values.step()
      }
      output.writeByte(0) // document sentinel
    }
  }

  trait BsonSeqFactory extends BaseSeqFactory {
    def readBson(input: Reader): SeqForm = input.readBsonArray(Builder)
  }


  trait BsonSet extends BsonValue with BaseSet { this: SetForm =>
    override def bsonType: Byte = 0x04

    private[this] var bsonLength: Int = -1
    override def bsonSize: Int = {
      if (bsonLength == -1) {
        var size = 4 // document length
        var i = 0
        val values = iterator
        while (!values.isEmpty) {
          val value = values.head
          if (value.isDefined) { // filter undefined values
            size += 1 // type tag
            size += (if (i == 0) 0 else i.toDouble.log10.toInt) + 2 // index cstring
            size += value.bsonSize
            i += 1
          }
          values.step()
        }
        size += 1 // document sentinel
        bsonLength = size
      }
      bsonLength
    }

    override def writeBson(output: Writer): Unit = {
      output.writeInt(bsonSize) // document length
      var i = 0
      val values = iterator
      while (!values.isEmpty) {
        val value = values.head
        if (value.isDefined) { // filter undefined values
          output.writeByte(value.bsonType)
          output.writeCString(i.toString) // worst array representation ever
          value.writeBson(output)
          i += 1
        }
        values.step()
      }
      output.writeByte(0) // document sentinel
    }
  }

  trait BsonSetFactory extends BaseSetFactory {
    def readBson(input: Reader): SetForm = input.readBsonArray(Builder)
  }


  trait BsonText extends BsonValue with BaseText { this: TextForm =>
    override def bsonType: Byte = 0x02

    private[this] var bsonLength: Int = -1
    override def bsonSize: Int = {
      if (bsonLength == -1) bsonLength = 4 + utf8Length + 1
      bsonLength
    }

    def writeBson(output: Writer): Unit = {
      output.writeInt(bsonSize - 4)
      val cs = utf8Iterator
      while (!cs.isEmpty) {
        output.writeByte(cs.head.toByte)
        cs.step()
      }
      output.writeByte(0)
    }
  }


  trait BsonData extends BsonValue with BaseData { this: DataForm =>
    override def bsonType: Byte = 0x05

    override def bsonSize: Int = 4 + 1 + size.toInt

    override def writeBson(output: Writer): Unit = {
      output.writeInt(size.toInt)
      output.writeByte(0x00) // generic subtype
      output.writeData(this)
    }
  }


  trait BsonNumber extends BsonValue with BaseNumber { this: NumberForm =>
    override def bsonType: Byte = {
      if (isValidInt) 0x10 // int32
      else if (isValidLong) 0x12 // int64
      else 0x01 // double
    }

    override def bsonSize: Int = bsonType match {
      case 0x01 => 8 // double
      case 0x12 => 8 // int64
      case 0x10 => 4 // int32
    }

    override def writeBson(output: Writer): Unit = bsonType match {
      case 0x01 => output.writeDouble(toDouble) // double
      case 0x12 => output.writeLong(toLong) // int64
      case 0x10 => output.writeInt(toInt) // int32
    }
  }


  trait BsonDate extends BsonValue with BaseDate { this: DateForm =>
    override def bsonType: Byte                  = 0x09
    override def bsonSize: Int                   = 8
    override def writeBson(output: Writer): Unit = output.writeLong(millis)
  }


  trait BsonBool extends BsonValue with BaseBool { this: BoolForm =>
    override def bsonType: Byte                  = 0x08
    override def bsonSize: Int                   = 1
    override def writeBson(output: Writer): Unit = output.writeByte(if (toBoolean) 1 else 0)
  }


  trait BsonNull extends BsonValue with BaseNull { this: NullForm =>
    override def bsonType: Byte                  = 0x0A
    override def bsonSize: Int                   = 0
    override def writeBson(output: Writer): Unit = ()
  }


  trait BsonNo extends BsonValue with BaseNo { this: NoForm =>
    override def bsonType: Byte                  = 0x06
    override def bsonSize: Int                   = 0
    override def writeBson(output: Writer): Unit = ()
  }


  protected[form] class BsonReader(underlying: Reader) extends Reader {
    override def endian: Endianness = underlying.endian

    override def isEOF: Boolean = underlying.isEOF

    override def readByte(): Byte     = underlying.readByte()
    override def readShort(): Short   = underlying.readShort()
    override def readInt(): Int       = underlying.readInt()
    override def readLong(): Long     = underlying.readLong()
    override def readFloat(): Float   = underlying.readFloat()
    override def readDouble(): Double = underlying.readDouble()

    override def drop(lower: Long): this.type  = { underlying.drop(lower); this }
    override def take(upper: Long): BsonReader = new BsonReader(underlying.take(upper))

    def readBsonDouble(): AnyForm = BsonDouble(readDouble())

    def readBsonString(implicit builder: StringBuilder): builder.State = {
      val utf8Builder = UTF8.Decoder(builder)
      var i = 0
      val n = readInt() - 1
      while (i < n) {
        utf8Builder.append(readByte() & 0xFF)
        i += 1
      }
      if (readByte() != 0) throw new BsonException("unterminated string")
      utf8Builder.state
    }

    def readBsonObject(implicit builder: Builder[(String, AnyForm)]): builder.State = {
      readInt() // size
      var tag = readByte()
      while (tag != 0) {
        (tag: @switch) match {
          case 0x01 => builder.append(readCString() -> readBsonDouble())
          case 0x02 => builder.append(readCString() -> BsonStringValue(readBsonString(BsonStringBuilder)))
          case 0x03 => builder.append(readCString() -> BsonObjectValue(readBsonObject(BsonObjectBuilder)))
          case 0x04 => builder.append(readCString() -> BsonArrayValue(readBsonArray(BsonArrayBuilder)))
          case 0x05 => builder.append(readCString() -> readBsonBinary())
          case 0x06 => builder.append(readCString() -> readBsonUndefined())
          case 0x07 => builder.append(readCString() -> readBsonObjectId())
          case 0x08 => builder.append(readCString() -> readBsonBoolean())
          case 0x09 => builder.append(readCString() -> readBsonDateTime())
          case 0x0A => builder.append(readCString() -> readBsonNull())
          case 0x0B => builder.append(readCString() -> readBsonRegex())
          case 0x0C => builder.append(readCString() -> readBsonDBPointer())
          case 0x0D => builder.append(readCString() -> readBsonJSCode())
          case 0x0E => builder.append(readCString() -> readBsonSymbol())
          case 0x0F => builder.append(readCString() -> readBsonJSScope())
          case 0x10 => builder.append(readCString() -> readBsonInt32())
          case 0x11 => builder.append(readCString() -> readBsonTimeStamp())
          case 0x12 => builder.append(readCString() -> readBsonInt64())
          case 0x7F => builder.append(readCString() -> readBsonMinKey())
          case 0xFF => builder.append(readCString() -> readBsonMaxKey())
          case tag  => throw new BsonException("unknown bson type: "+ tag)
        }
        tag = readByte()
      }
      builder.state
    }

    def readBsonArray(implicit builder: Builder[AnyForm]): builder.State = {
      readInt() // size
      var tag = readByte()
      while (tag != 0) {
        (tag: @switch) match {
          case 0x01 => skipCString(); builder.append(readBsonDouble())
          case 0x02 => skipCString(); builder.append(BsonStringValue(readBsonString(BsonStringBuilder)))
          case 0x03 => skipCString(); builder.append(BsonObjectValue(readBsonObject(BsonObjectBuilder)))
          case 0x04 => skipCString(); builder.append(BsonArrayValue(readBsonArray(BsonArrayBuilder)))
          case 0x05 => skipCString(); builder.append(readBsonBinary())
          case 0x06 => skipCString(); builder.append(readBsonUndefined())
          case 0x07 => skipCString(); builder.append(readBsonObjectId())
          case 0x08 => skipCString(); builder.append(readBsonBoolean())
          case 0x09 => skipCString(); builder.append(readBsonDateTime())
          case 0x0A => skipCString(); builder.append(readBsonNull())
          case 0x0B => skipCString(); builder.append(readBsonRegex())
          case 0x0C => skipCString(); builder.append(readBsonDBPointer())
          case 0x0D => skipCString(); builder.append(readBsonJSCode())
          case 0x0E => skipCString(); builder.append(readBsonSymbol())
          case 0x0F => skipCString(); builder.append(readBsonJSScope())
          case 0x10 => skipCString(); builder.append(readBsonInt32())
          case 0x11 => skipCString(); builder.append(readBsonTimeStamp())
          case 0x12 => skipCString(); builder.append(readBsonInt64())
          case 0x7F => skipCString(); builder.append(readBsonMinKey())
          case 0xFF => skipCString(); builder.append(readBsonMaxKey())
          case tag  => throw new BsonException("unknown bson type: "+ tag)
        }
        tag = readByte()
      }
      builder.state
    }

    def readBsonBinary(): AnyForm = {
      val length = readInt()
      val subtype = readByte()
      val data = readByteArray(length)
      BsonBinary(subtype, data)
    }

    def readBsonUndefined(): AnyForm = BsonUndefined

    def readBsonObjectId(): AnyForm = BsonObjectId(readByteArray(12))

    def readBsonBoolean(): AnyForm = BsonBoolean(readByte() != 0)

    def readBsonDateTime(): AnyForm = BsonDateTime(readLong())

    def readBsonNull(): AnyForm = BsonNull

    def readBsonRegex(): AnyForm = {
      val pattern = readCString()
      val options = readCString()
      BsonRegex(pattern, options)
    }

    def readBsonDBPointer(): AnyForm = {
      val name = readUTF8String()
      val id = readByteArray(12)
      BsonDBPointer(name, id)
    }

    def readBsonJSCode(): AnyForm = {
      val js = readUTF8String()
      BsonJSCode(js)
    }

    def readBsonSymbol(): AnyForm = {
      val symbol = readUTF8String()
      BsonSymbol(symbol)
    }

    def readBsonJSScope(): AnyForm = {
      readInt() // length
      val js = readUTF8String()
      val scope = readBsonObject(ObjectForm.Builder())
      BsonJSScope(js, scope)
    }

    def readBsonInt32(): AnyForm = BsonInt32(readInt())

    def readBsonTimeStamp(): AnyForm = BsonTimeStamp(readLong())

    def readBsonInt64(): AnyForm = BsonInt64(readLong())

    def readBsonMinKey(): AnyForm = BsonMinKey

    def readBsonMaxKey(): AnyForm = BsonMaxKey

    def readUTF8String(): String = {
      var i = 0
      val n = readInt() - 1
      val builder = UTF8.Decoder(UString.Builder)
      while (i < n) {
        builder.append(readByte() & 0xFF)
        i += 1
      }
      if (readByte() != 0) throw new BsonException("unterminated string")
      builder.state.toString
    }

    def readCString(): String = {
      val builder = UTF8.Decoder(UString.Builder)
      var b = readByte()
      while (b != 0) {
        builder.append(b & 0xFF)
        b = readByte()
      }
      builder.state.toString
    }

    def skipCString(): Unit = while (readByte() != 0) ()

    def readByteArray(length: Int): Array[Byte] = {
      var i = 0
      val array = new Array[Byte](length)
      while (i < length) {
        array(i) = readByte()
        i += 1
      }
      array
    }
  }

  protected[form] class BsonWriter(protected val underlying: Writer) {
    def writeCString(string: UTF): Unit = {
      val cs = string.modifiedUTF8Iterator
      while (!cs.isEmpty) {
        underlying.writeByte(cs.head.toByte) // modified UTF-8 code units
        cs.step()
      }
      underlying.writeByte(0) // cstring sentinel
    }

    def writeCString(string: String): Unit = {
      val cs = new UString(string).modifiedUTF8Iterator
      while (!cs.isEmpty) {
        underlying.writeByte(cs.head.toByte) // modified UTF-8 code units
        cs.step()
      }
      underlying.writeByte(0) // cstring sentinel
    }
  }
}
