//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.data._
import basis.proto._
import basis.text._
import basis.util._
import scala.annotation._
import scala.reflect._

trait ProtoVariant extends Variant { variant =>
  override type AnyForm    <: ProtoValue
  override type ObjectForm <: ProtoObject with AnyForm
  override type SeqForm    <: ProtoSeq with AnyForm
  override type SetForm    <: ProtoSet with AnyForm
  override type TextForm   <: ProtoText with AnyForm
  override type DataForm   <: ProtoData with AnyForm
  override type NumberForm <: ProtoNumber with AnyForm
  override type DateForm   <: ProtoDate with AnyForm
  override type BoolForm   <: ProtoBool with AnyForm
  override type NullForm   <: ProtoNull with AnyForm
  override type NoForm     <: ProtoNo with AnyForm

  override val AnyForm: ProtoValueFactory

  /** A symmetrically encrypted variant form.
    * @template */
  type SecretForm <: ProtoSecret with AnyForm

  val SecretForm: ProtoSecretFactory

  implicit def SecretFormTag: ClassTag[SecretForm]

  implicit lazy val Proto: Proto = new Proto


  trait ProtoValue extends BaseValue { this: AnyForm =>
    def isSecretForm: Boolean = false
    def asSecretForm: SecretForm = throw new MatchError("not a SecretForm")

    def encrypt(secretKey: Loader, iv: Loader): AnyForm = try {
      val crypto = new ProtoVariantCrypto[variant.type](variant)
      crypto.encrypt(this, secretKey, iv)
    }
    catch { case _: NoClassDefFoundError => this }

    def decrypt(secretKey: Loader): AnyForm = this

    def protoField: Protobuf.Field[_ >: this.type]

    def writeProto(data: Writer): Unit = Proto.write(data, this)

    def toProto: Loader = {
      val data = ArrayDataLE.Framer.expect(Proto.sizeOf(this))
      writeProto(data)
      data.state
    }
  }

  trait ProtoValueFactory extends BaseValueFactory {
    def readProto(data: Reader): AnyForm = Proto.read(data)
  }


  trait ProtoSecret extends ProtoValue { this: SecretForm =>
    override def isSecretForm: Boolean = true
    override def asSecretForm: SecretForm = this

    private[form] var protoSize: Int = -1

    override def protoField: Protobuf.Field[SecretForm] = Proto.SecretFormField

    def data: Loader
    def iv: Loader
    def mac: Loader

    override def decrypt(secretKey: Loader): AnyForm = try {
      val crypto = new ProtoVariantCrypto[variant.type](variant)
      crypto.decrypt(this, secretKey)
    }
    catch { case _: NoClassDefFoundError => this }

    protected def toObjectForm: ObjectForm =
      ObjectForm(
        "data" -> DataForm.from(data),
        "iv"   -> DataForm.from(iv),
        "mac"  -> DataForm.from(mac))

    override def toString: String =
      (String.Builder~variant.toString~'.'~"SecretForm"~'('~
        "data"~" = "~>data~", "~
        "iv"~" = "~>iv~", "~
        "mac"~" = "~>mac~')').state
  }

  trait ProtoSecretFactory {
    def apply(data: Loader, iv: Loader, mac: Loader): SecretForm

    override def toString: String = (String.Builder~variant.toString~'.'~"SecretForm").state
  }


  trait ProtoObject extends ProtoValue with BaseObject { this: ObjectForm =>
    private[form] var protoSize: Int = -1

    override def protoField: Protobuf.Field[ObjectForm] = Proto.ObjectFormField
  }


  trait ProtoSeq extends ProtoValue with BaseSeq { this: SeqForm =>
    private[form] var protoSize: Int = -1

    override def protoField: Protobuf.Field[SeqForm] = Proto.SeqFormField
  }


  trait ProtoSet extends ProtoValue with BaseSet { this: SetForm =>
    private[form] var protoSize: Int = -1

    override def protoField: Protobuf.Field[SetForm] = Proto.SetFormField
  }


  trait ProtoText extends ProtoValue with BaseText { this: TextForm =>
    override def protoField: Protobuf.Field[TextForm] = Proto.TextFormField
  }


  trait ProtoData extends ProtoValue with BaseData { this: DataForm =>
    override def protoField: Protobuf.Field[DataForm] = Proto.DataFormField
  }


  trait ProtoNumber extends ProtoValue with BaseNumber { this: NumberForm =>
    override def protoField: Protobuf.Field[NumberForm] =
      if (isValidLong) Proto.LongFormField else Proto.DoubleFormField
  }


  trait ProtoDate extends ProtoValue with BaseDate { this: DateForm =>
    override def protoField: Protobuf.Field[DateForm] = Proto.DateFormField
  }


  trait ProtoBool extends ProtoValue with BaseBool { this: BoolForm =>
    override def protoField: Protobuf.Field[BoolForm] = Proto.BoolFormField
  }


  trait ProtoNull extends ProtoValue with BaseNull { this: NullForm =>
    override def protoField: Protobuf.Field[NullForm] = Proto.NullFormField
  }


  trait ProtoNo extends ProtoValue with BaseNo { this: NoForm =>
    override def protoField: Protobuf.Field[NoForm] = Proto.NoFormField
  }


  class Proto extends Protobuf[AnyForm] {
    implicit lazy val ObjectFieldProto: Protobuf[(String, AnyForm)] = new ObjectFieldProto

    implicit lazy val ObjectFormProto: Protobuf[ObjectForm] = new ObjectFormProto
    implicit lazy val SeqFormProto: Protobuf[SeqForm]       = new SeqFormProto
    implicit lazy val SetFormProto: Protobuf[SetForm]       = new SetFormProto
    implicit lazy val TextFormProto: Protobuf[TextForm]     = new TextFormProto
    implicit lazy val DataFormProto: Protobuf[DataForm]     = new DataFormProto
    implicit lazy val LongFormProto: Protobuf[NumberForm]   = new LongFormProto
    implicit lazy val DoubleFormProto: Protobuf[NumberForm] = new DoubleFormProto
    implicit lazy val DateFormProto: Protobuf[DateForm]     = new DateFormProto
    implicit lazy val BoolFormProto: Protobuf[BoolForm]     = new BoolFormProto
    implicit lazy val NullFormProto: Protobuf[NullForm]     = new NullFormProto
    implicit lazy val SecretFormProto: Protobuf[SecretForm] = new SecretFormProto
    implicit lazy val NoFormProto: Protobuf[NoForm]         = new NoFormProto

    lazy val ObjectFormField  = Protobuf.Required(1)(ObjectFormProto)
    lazy val SeqFormField     = Protobuf.Required(2)(SeqFormProto)
    lazy val SetFormField     = Protobuf.Required(3)(SetFormProto)
    lazy val TextFormField    = Protobuf.Required(5)(TextFormProto)
    lazy val DataFormField    = Protobuf.Required(6)(DataFormProto)
    lazy val LongFormField    = Protobuf.Required(7)(LongFormProto)
    lazy val DoubleFormField  = Protobuf.Required(8)(DoubleFormProto)
    lazy val IntegerFormField = Protobuf.Unknown[AnyForm](9, Protobuf.WireType.Message, NoForm)(this)
    lazy val DecimalFormField = Protobuf.Unknown[AnyForm](10, Protobuf.WireType.Message, NoForm)(this)
    lazy val DateFormField    = Protobuf.Required(11)(DateFormProto)
    lazy val BoolFormField    = Protobuf.Required(12)(BoolFormProto)
    lazy val NullFormField    = Protobuf.Required(13)(NullFormProto)
    lazy val SecretFormField  = Protobuf.Required(15)(SecretFormProto)
    lazy val NoFormField      = Protobuf.Default(0, NoForm)(NoFormProto)

    def field(key: Long): Protobuf.Field[_ <: AnyForm] = (key.toInt: @switch) match {
      case 0x0A => ObjectFormField
      case 0x12 => SeqFormField
      case 0x1A => SetFormField
      case 0x2A => TextFormField
      case 0x32 => DataFormField
      case 0x38 => LongFormField
      case 0x41 => DoubleFormField
      case 0x4A => IntegerFormField
      case 0x52 => DecimalFormField
      case 0x58 => DateFormField
      case 0x60 => BoolFormField
      case 0x68 => NullFormField
      case 0x7A => SecretFormField
      case _    => Protobuf.Unknown[AnyForm](key, NoForm)(this)
    }

    override def read(data: Reader): AnyForm = {
      var form = NoForm: AnyForm
      while (!data.isEOF) {
        val key = Protobuf.Varint.read(data)
        form = field(key).readValue(data)
      }
      form
    }

    override def write(data: Writer, form: AnyForm): Unit =
      if (form.isDefined) form.protoField.write(data, form)

    override def sizeOf(form: AnyForm): Int =
      if (form.isDefined) form.protoField.sizeOf(form) else 0

    override def wireType: Int = Protobuf.WireType.Message

    override def toString: String = variant.toString +"."+"Proto"
  }

  private[form] final class SecretFormProto extends Protobuf[SecretForm] {
    private[this] val DataField = Protobuf.Required(2)(Protobuf.Bytes)
    private[this] val IvField   = Protobuf.Required(3)(Protobuf.Bytes)
    private[this] val MacField  = Protobuf.Required(4)(Protobuf.Bytes)

    override def read(reader: Reader): SecretForm = {
      var data = null: Loader
      var iv   = null: Loader
      var mac  = null: Loader
      while (!reader.isEOF) {
        val key = Protobuf.Varint.read(reader)
        (key.toInt: @switch) match {
          case 0x12 => data = DataField.readValue(reader)
          case 0x1A => iv   = IvField.readValue(reader)
          case 0x22 => mac  = MacField.readValue(reader)
          case _    => Protobuf.Unknown(key).readValue(reader)
        }
      }
      if (data != null && iv != null && mac != null) SecretForm(data, iv, mac)
      else throw new ProtobufException {
        if (data != null && iv != null) "SecretForm has no mac"
        else if (data != null && mac != null) "SecretForm has no iv"
        else if (iv != null && mac != null) "SecretForm has no data"
        else if (data != null) "SecretForm has no iv and no mac"
        else if (iv != null) "SecretForm has no data and no mac"
        else if (mac != null) "SecretForm has no data and no iv"
        else "SecretForm has no data, no iv, and no mac"
      }
    }

    override def write(writer: Writer, form: SecretForm): Unit = {
      DataField.write(writer, form.data)
      IvField.write(writer, form.iv)
      MacField.write(writer, form.mac)
    }

    override def sizeOf(form: SecretForm): Int = {
      if (form.protoSize == -1)
        form.protoSize =
          DataField.sizeOf(form.data) +
          IvField.sizeOf(form.iv)     +
          MacField.sizeOf(form.mac)
      form.protoSize
    }

    override def wireType: Int = Protobuf.WireType.Message

    override def toString: String = variant.toString +"."+"Proto"+"."+"SecretFormProto"
  }

  private[form] final class ObjectFieldProto extends Protobuf[(String, AnyForm)] {
    private[this] val NameField  = Protobuf.Required(2)(Protobuf.String)
    private[this] val ValueField = Protobuf.Required(3)(Proto)

    override def read(data: Reader): (String, AnyForm) = {
      var k = null: String
      var v = null.asInstanceOf[AnyForm]
      while (!data.isEOF) {
        val key = Protobuf.Varint.read(data)
        (key.toInt: @switch) match {
          case 0x12 => k = NameField.readValue(data)
          case 0x1A => v = ValueField.readValue(data)
          case _    => Protobuf.Unknown(key).readValue(data)
        }
      }
      if (k != null && v != null) (k, v)
      else throw new ProtobufException {
        if (v != null) "ObjectForm field has no key"
        else if (k != null) "ObjectForm field has no value"
        else "ObjectForm field has no key or value"
      }
    }

    override def write(data: Writer, field: (String, AnyForm)): Unit = {
      NameField.write(data, field._1)
      ValueField.write(data, field._2)
    }

    override def sizeOf(field: (String, AnyForm)): Int = {
      NameField.sizeOf(field._1)  +
      ValueField.sizeOf(field._2)
    }

    override def wireType: Int = Protobuf.WireType.Message

    override def toString: String = variant.toString +"."+"Proto"+"."+"ObjectFieldProto"
  }

  private[form] final class ObjectFormProto extends Protobuf[ObjectForm] {
    private[this] val KeyValueField = Protobuf.Required(1)(Proto.ObjectFieldProto)

    override def read(data: Reader): ObjectForm = {
      val builder = ObjectFormBuilder
      while (!data.isEOF) {
        val key = Protobuf.Varint.read(data)
        (key.toInt: @switch) match {
          case 0x0A => builder.append(KeyValueField.readValue(data))
          case _    => Protobuf.Unknown(key).readValue(data)
        }
      }
      builder.state
    }

    override def write(data: Writer, form: ObjectForm): Unit = {
      val fields = form.iterator
      while (!fields.isEmpty) {
        val field = fields.head
        if (field._2.isDefined) KeyValueField.write(data, field)
        fields.step()
      }
    }

    override def sizeOf(form: ObjectForm): Int = {
      if (form.protoSize == -1) {
        var size = 0
        val fields = form.iterator
        while (!fields.isEmpty) {
          val field = fields.head
          if (field._2.isDefined) size += KeyValueField.sizeOf(field)
          fields.step()
        }
        form.protoSize = size
      }
      form.protoSize
    }

    override def wireType: Int = Protobuf.WireType.Message

    override def toString: String = variant.toString +"."+"Proto"+"."+"ObjectFormProto"
  }

  private[form] final class SeqFormProto extends Protobuf[SeqForm] {
    private[this] val ValueField = Protobuf.Required(1)(Proto)

    override def read(data: Reader): SeqForm = {
      val builder = SeqFormBuilder
      while (!data.isEOF) {
        val key = Protobuf.Varint.read(data)
        (key.toInt: @switch) match {
          case 0x0A => builder.append(ValueField.readValue(data))
          case _    => Protobuf.Unknown(key).readValue(data)
        }
      }
      builder.state
    }

    override def write(data: Writer, form: SeqForm): Unit = {
      val values = form.iterator
      while (!values.isEmpty) {
        val value = values.head
        if (value.isDefined) ValueField.write(data, value)
        values.step()
      }
    }

    override def sizeOf(form: SeqForm): Int = {
      if (form.protoSize == -1) {
        var size = 0
        val values = form.iterator
        while (!values.isEmpty) {
          val value = values.head
          if (value.isDefined) size += ValueField.sizeOf(value)
          values.step()
        }
        form.protoSize = size
      }
      form.protoSize
    }

    override def wireType: Int = Protobuf.WireType.Message

    override def toString: String = variant.toString +"."+"Proto"+"."+"SeqFormProto"
  }

  private[form] final class SetFormProto extends Protobuf[SetForm] {
    private[this] val ValueField = Protobuf.Required(1)(Proto)

    override def read(data: Reader): SetForm = {
      val builder = SetFormBuilder
      while (!data.isEOF) {
        val key = Protobuf.Varint.read(data)
        (key.toInt: @switch) match {
          case 0x0A => builder.append(ValueField.readValue(data))
          case _    => Protobuf.Unknown(key).readValue(data)
        }
      }
      builder.state
    }

    override def write(data: Writer, form: SetForm): Unit = {
      val values = form.iterator
      while (!values.isEmpty) {
        val value = values.head
        if (value.isDefined) ValueField.write(data, value)
        values.step()
      }
    }

    override def sizeOf(form: SetForm): Int = {
      if (form.protoSize == -1) {
        var size = 0
        val values = form.iterator
        while (!values.isEmpty) {
          val value = values.head
          if (value.isDefined) size += ValueField.sizeOf(value)
          values.step()
        }
        form.protoSize = size
      }
      form.protoSize
    }

    override def wireType: Int = Protobuf.WireType.Message

    override def toString: String = variant.toString +"."+"Proto"+"."+"SetFormProto"
  }

  private[form] final class TextFormProto extends Protobuf[TextForm] {
    override def read(data: Reader): TextForm = {
      val builder = UTF8.Decoder(TextFormBuilder)
      while (!data.isEOF) builder.append(data.readByte & 0xFF)
      builder.state
    }

    override def write(data: Writer, form: TextForm): Unit = {
      val cs = form.utf8Iterator
      while (!cs.isEmpty) {
        data.writeByte(cs.head.toByte)
        cs.step()
      }
    }

    override def sizeOf(form: TextForm): Int = form.utf8Length

    override def wireType: Int = Protobuf.WireType.Message
  }

  private[form] final class DataFormProto extends Protobuf[DataForm] {
    override def read(data: Reader): DataForm = {
      val framer = DataFormFramer
      while (!data.isEOF) framer.writeByte(data.readByte)
      framer.state
    }

    override def write(data: Writer, form: DataForm): Unit = data.writeData(form)

    override def sizeOf(form: DataForm): Int = form.size.toInt

    override def wireType: Int = Protobuf.WireType.Message

    override def toString: String = variant.toString +"."+"Proto"+"."+"DataFormProto"
  }

  private[form] final class LongFormProto extends Protobuf[NumberForm] {
    override def read(data: Reader): NumberForm              = NumberForm(Protobuf.Varint.read(data))
    override def write(data: Writer, form: NumberForm): Unit = Protobuf.Varint.write(data, form.toLong)
    override def sizeOf(form: NumberForm): Int               = Protobuf.Varint.sizeOf(form.toLong)
    override def wireType: Int                               = Protobuf.Varint.wireType
    override def toString: String                            = variant.toString +"."+"Proto"+"."+"LongFormProto"
  }

  private[form] final class DoubleFormProto extends Protobuf[NumberForm] {
    override def read(data: Reader): NumberForm              = NumberForm(Protobuf.Double.read(data))
    override def write(data: Writer, form: NumberForm): Unit = Protobuf.Double.write(data, form.toDouble)
    override def sizeOf(form: NumberForm): Int               = Protobuf.Double.sizeOf(form.toDouble)
    override def wireType: Int                               = Protobuf.Double.wireType
    override def toString: String                            = variant.toString +"."+"Proto"+"."+"DoubleFormProto"
  }

  private[form] final class DateFormProto extends Protobuf[DateForm] {
    override def read(data: Reader): DateForm              = DateForm(Protobuf.Varint.read(data))
    override def write(data: Writer, form: DateForm): Unit = Protobuf.Varint.write(data, form.millis)
    override def sizeOf(form: DateForm): Int               = Protobuf.Varint.sizeOf(form.millis)
    override def wireType: Int                             = Protobuf.Varint.wireType
    override def toString: String                          = variant.toString +"."+"Proto"+"."+"DateFormProto"
  }

  private[form] final class BoolFormProto extends Protobuf[BoolForm] {
    override def read(data: Reader): BoolForm              = BoolForm(Protobuf.Bool.read(data))
    override def write(data: Writer, form: BoolForm): Unit = Protobuf.Bool.write(data, form.toBoolean)
    override def sizeOf(form: BoolForm): Int               = Protobuf.Bool.sizeOf(form.toBoolean)
    override def wireType: Int                             = Protobuf.Bool.wireType
    override def toString: String                          = variant.toString +"."+"Proto"+"."+"BoolFormProto"
  }

  private[form] final class NullFormProto extends Protobuf[NullForm] {
    override def read(data: Reader): NullForm              = { Protobuf.Varint.read(data); NullForm }
    override def write(data: Writer, form: NullForm): Unit = Protobuf.Varint.write(data, 0L)
    override def sizeOf(form: NullForm): Int               = Protobuf.Varint.sizeOf(0L)
    override def wireType: Int                             = Protobuf.Varint.wireType
    override def toString: String                          = variant.toString +"."+"Proto"+"."+"NullFormProto"
  }

  private[form] final class NoFormProto extends Protobuf[NoForm] {
    override def read(data: Reader): NoForm              = { Protobuf.Varint.read(data); NoForm }
    override def write(data: Writer, form: NoForm): Unit = Protobuf.Varint.write(data, 0L)
    override def sizeOf(form: NoForm): Int               = Protobuf.Varint.sizeOf(0L)
    override def wireType: Int                           = Protobuf.Varint.wireType
    override def toString: String                        = variant.toString +"."+"Proto"+"."+"NoFormProto"
  }
}
