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
import scala.reflect._

object OmniVariant extends Variant with JsonVariant with BsonVariant with ProtoVariant {
  import Predef.classOf

  override type AnyForm    = OmniValue
  override type ObjectForm = OmniObject
  override type SeqForm    = OmniSeq
  override type SetForm    = OmniSet
  override type TextForm   = OmniText
  override type DataForm   = OmniData
  override type NumberForm = OmniNumber
  override type DateForm   = OmniDate
  override type BoolForm   = OmniBool
  override type NullForm   = OmniNull
  override type NoForm     = OmniNo

  override lazy val AnyForm    = new OmniValueFactory
  override lazy val ObjectForm = new OmniObjectFactory
  override lazy val SeqForm    = new OmniSeqFactory
  override lazy val SetForm    = new OmniSetFactory

  override lazy val TextForm   = new OmniTextFactory
  override lazy val DataForm   = new OmniDataFactory
  override lazy val NumberForm = new OmniNumberFactory
  override lazy val DateForm   = new OmniDateFactory
  override lazy val BoolForm   = new OmniBoolFactory

  override lazy val TrueForm: BoolForm  = new OmniBool(true)
  override lazy val FalseForm: BoolForm = new OmniBool(false)
  override lazy val NullForm: NullForm  = new OmniNull
  override lazy val NoForm: NoForm      = new OmniNo

  implicit override lazy val AnyFormTag: ClassTag[AnyForm]       = ClassTag(classOf[OmniValue])
  implicit override lazy val ObjectFormTag: ClassTag[ObjectForm] = ClassTag(classOf[OmniObject])
  implicit override lazy val SeqFormTag: ClassTag[SeqForm]       = ClassTag(classOf[OmniSeq])
  implicit override lazy val SetFormTag: ClassTag[SetForm]       = ClassTag(classOf[OmniSet])
  implicit override lazy val TextFormTag: ClassTag[TextForm]     = ClassTag(classOf[OmniText])
  implicit override lazy val DataFormTag: ClassTag[DataForm]     = ClassTag(classOf[OmniData])
  implicit override lazy val NumberFormTag: ClassTag[NumberForm] = ClassTag(classOf[OmniNumber])
  implicit override lazy val DateFormTag: ClassTag[DateForm]     = ClassTag(classOf[OmniDate])
  implicit override lazy val BoolFormTag: ClassTag[BoolForm]     = ClassTag(classOf[OmniBool])
  implicit override lazy val NullFormTag: ClassTag[NullForm]     = ClassTag(classOf[OmniNull])
  implicit override lazy val NoFormTag: ClassTag[NoForm]         = ClassTag(classOf[OmniNo])


  abstract class OmniValue extends BaseValue with JsonValue with BsonValue with ProtoValue

  protected class OmniValueFactory extends BaseValueFactory with JsonValueFactory with BsonValueFactory with ProtoValueFactory


  class OmniObject(protected val underlying: Seq[(String, AnyForm)]) extends OmniValue with BaseObject with JsonObject with BsonObject with ProtoObject {
    override def :+ (key: String, value: AnyForm): ObjectForm = underlying :+ (key -> value)
    override def +: (key: String, value: AnyForm): ObjectForm = (key -> value) +: underlying
    override def + (key: String, value: AnyForm): ObjectForm  = underlying :+ (key -> value)
    override def - (key: String): ObjectForm                  = underlying.filter(!_._1.equals(key))
    override def ++ (that: ObjectForm): ObjectForm            = underlying ++ that.underlying
    override def -- (that: ObjectForm): ObjectForm            = underlying.filter(field => !that.contains(field._1))
    override def iterator: Iterator[(String, AnyForm)]        = underlying.iterator
  }

  protected class OmniObjectFactory extends BaseObjectFactory with JsonObjectFactory with BsonObjectFactory {
    override val empty: ObjectForm                                                   = new OmniObject(immutable.FingerTrieSeq.empty)
    implicit override def Builder: Builder[(String, AnyForm)] with State[ObjectForm] = new OmniObjectBuilder(immutable.FingerTrieSeq.Builder[(String, AnyForm)])
  }

  protected final class OmniObjectBuilder(underlying: Builder[(String, AnyForm)] with State[Seq[(String, AnyForm)]]) extends Builder[(String, AnyForm)] with State[ObjectForm] {
    override def append(entry: (String, AnyForm)): Unit = underlying.append(entry)
    override def clear(): Unit                          = underlying.clear()
    override def expect(count: Int): this.type          = { underlying.expect(count); this }
    override def state: ObjectForm                      = new OmniObject(underlying.state)
    override def toString: String                       = "ObjectForm"+"."+"Builder"
  }


  class OmniSeq(protected val underlying: IndexedSeq[AnyForm]) extends OmniValue with BaseSeq with JsonSeq with BsonSeq with ProtoSeq {
    override def length: Int                  = underlying.length
    override def apply(index: Int): AnyForm   = underlying(index)
    override def :+ (value: AnyForm): SeqForm = underlying.:+(value)(SeqFormBuilder)
    override def +: (value: AnyForm): SeqForm = underlying.+:(value)(SeqFormBuilder)
    override def ++ (that: SeqForm): SeqForm  = underlying.++(that.underlying)(SeqFormBuilder)
    override def iterator: Iterator[AnyForm]  = underlying.iterator
  }

  protected class OmniSeqFactory extends BaseSeqFactory with JsonSeqFactory with BsonSeqFactory {
    override val empty: SeqForm                                         = new OmniSeq(immutable.FingerTrieSeq.empty)
    implicit override def Builder: Builder[AnyForm] with State[SeqForm] = new OmniSeqBuilder(immutable.FingerTrieSeq.Builder[AnyForm])
  }

  protected final class OmniSeqBuilder(underlying: Builder[AnyForm] with State[IndexedSeq[AnyForm]]) extends Builder[AnyForm] with State[SeqForm] {
    override def append(elem: AnyForm): Unit   = underlying.append(elem)
    override def clear(): Unit                 = underlying.clear()
    override def expect(count: Int): this.type = { underlying.expect(count); this }
    override def state: SeqForm                = new OmniSeq(underlying.state)
    override def toString: String              = "SeqForm"+"."+"Builder"
  }


  class OmniSet(protected val underlying: Seq[AnyForm]) extends OmniValue with BaseSet with JsonSet with BsonSet with ProtoSet {
    override def size: Int                   = underlying.length
    override def + (value: AnyForm): SetForm = underlying.:+(value)(SetFormBuilder)
    override def - (value: AnyForm): SetForm = underlying.filter(!_.equals(value))(SetFormBuilder)
    override def ++ (that: SetForm): SetForm = underlying.++(that.underlying)(SetFormBuilder)
    override def -- (that: SetForm): SetForm = underlying.filter(value => !that.contains(value))(SetFormBuilder)
    override def iterator: Iterator[AnyForm] = underlying.iterator
  }

  protected class OmniSetFactory extends BaseSetFactory with JsonSetFactory with BsonSetFactory {
    override val empty: SetForm                                         = new OmniSet(immutable.FingerTrieSeq.empty)
    implicit override def Builder: Builder[AnyForm] with State[SetForm] = new OmniSetBuilder(immutable.FingerTrieSeq.Builder[AnyForm])
  }

  protected final class OmniSetBuilder(underlying: Builder[AnyForm] with State[Seq[AnyForm]]) extends Builder[AnyForm] with State[SetForm] {
    override def append(elem: AnyForm): Unit   = underlying.append(elem)
    override def clear(): Unit                 = underlying.clear()
    override def expect(count: Int): this.type = { underlying.expect(count); this }
    override def state: SetForm                = new OmniSet(underlying.state)
    override def toString: String              = "SetForm"+"."+"Builder"
  }


  class OmniText(protected val underlying: UString) extends OmniValue with BaseText with JsonText with BsonText with ProtoText {
    private[this] var utf8Size: Int = -1
    override def utf8Length: Int = {
      if (utf8Size == -1) utf8Size = super.utf8Length
      utf8Size
    }
    override def iterator: Iterator[Int] = underlying.iterator
    override def toUString: UString      = underlying
  }

  protected class OmniTextFactory extends BaseTextFactory {
    override val empty: TextForm                                      = new OmniText(new UString(""))
    implicit override def Builder: StringBuilder with State[TextForm] = new OmniTextBuilder(UString.Builder)
  }

  protected final class OmniTextBuilder(underlying: StringBuilder with State[UString]) extends StringBuilder with State[TextForm] {
    override def append(c: Int): Unit           = underlying.append(c)
    override def append(cs: CharSequence): Unit = underlying.append(cs)
    override def clear(): Unit                  = underlying.clear()
    override def expect(count: Int): this.type  = { underlying.expect(count); this }
    override def state: TextForm                = new OmniText(underlying.state)
    override def toString: String               = "TextForm"+"."+"Builder"
  }


  class OmniData(underlying: Loader) extends OmniValue with BaseData with JsonData with BsonData with ProtoData {
    override def endian: Endianness                = underlying.endian
    override def size: Long                        = underlying.size
    override def as[E <: Endianness](endian: E): DataForm with basis.data.ByteOrder[E] = {
      if (underlying.endian.eq(endian)) this
      else new OmniData(underlying.as(endian))
    }.asInstanceOf[DataForm with basis.data.ByteOrder[E]]
    override def loadByte(address: Long): Byte     = underlying.loadByte(address)
    override def loadShort(address: Long): Short   = underlying.loadShort(address)
    override def loadInt(address: Long): Int       = underlying.loadInt(address)
    override def loadLong(address: Long): Long     = underlying.loadLong(address)
    override def loadFloat(address: Long): Float   = underlying.loadFloat(address)
    override def loadDouble(address: Long): Double = underlying.loadDouble(address)
    override def reader(address: Long): Reader     = underlying.reader(address)
    override def toArray: Array[Byte]              = underlying.toArray
  }

  protected class OmniDataFactory extends BaseDataFactory {
    override def endian: Endianness                  = NativeEndian
    override val empty: DataForm                     = new OmniData(ArrayData.empty)
    override def from(data: Loader): DataForm        = {
      if (data.isInstanceOf[OmniData]) data.asInstanceOf[OmniData]
      else new OmniData(data)
    }
    override def Framer: Framer with State[DataForm] = new OmniDataFramer(ArrayData.Framer)
  }

  protected final class OmniDataFramer(underlying: Framer with State[Loader]) extends Framer with State[DataForm] {
    override def endian: Endianness               = underlying.endian
    override def isEOF: Boolean                   = underlying.isEOF
    override def writeByte(value: Byte): Unit     = underlying.writeByte(value)
    override def writeShort(value: Short): Unit   = underlying.writeShort(value)
    override def writeInt(value: Int): Unit       = underlying.writeInt(value)
    override def writeLong(value: Long): Unit     = underlying.writeLong(value)
    override def writeFloat(value: Float): Unit   = underlying.writeFloat(value)
    override def writeDouble(value: Double): Unit = underlying.writeDouble(value)
    override def writeData(data: Loader): Unit    = underlying.writeData(data)
    override def expect(count: Long): this.type   = { underlying.expect(count); this }
    override def state: DataForm                  = new OmniData(underlying.state)
    override def clear(): Unit                    = underlying.clear()
  }


  abstract class OmniNumber extends OmniValue with BaseNumber with JsonNumber with BsonNumber with ProtoNumber

  protected class OmniInt(override val toInt: Int) extends OmniNumber with BaseInt

  protected class OmniLong(override val toLong: Long) extends OmniNumber with BaseLong

  protected class OmniFloat(override val toFloat: Float) extends OmniNumber with BaseFloat

  protected class OmniDouble(override val toDouble: Double) extends OmniNumber with BaseDouble

  protected class OmniNumberFactory extends BaseNumberFactory {
    override def apply(value: Int): NumberForm    = new OmniInt(value)
    override def apply(value: Long): NumberForm   = new OmniLong(value)
    override def apply(value: Float): NumberForm  = new OmniFloat(value)
    override def apply(value: Double): NumberForm = new OmniDouble(value)
  }


  class OmniDate(override val millis: Long) extends OmniValue with BaseDate with JsonDate with BsonDate with ProtoDate

  protected class OmniDateFactory extends BaseDateFactory {
    override def apply(millis: Long): DateForm = new OmniDate(millis)
  }


  class OmniBool(override val toBoolean: Boolean) extends OmniValue with BaseBool with JsonBool with BsonBool with ProtoBool

  protected class OmniBoolFactory extends BaseBoolFactory


  class OmniNull extends OmniValue with BaseNull with JsonNull with BsonNull with ProtoNull


  class OmniNo extends OmniValue with BaseNo with JsonNo with BsonNo with ProtoNo
}
