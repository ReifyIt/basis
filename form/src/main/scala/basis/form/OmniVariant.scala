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

object OmniVariant extends Variant with JsonVariant with BsonVariant with ProtoVariant { variant =>
  sealed abstract class AnyForm extends BaseValue with JsonValue with BsonValue with ProtoValue

  object AnyForm extends BaseValueFactory with JsonValueFactory with BsonValueFactory with ProtoValueFactory


  final class ObjectForm(protected val underlying: Seq[(String, AnyForm)]) extends AnyForm with BaseObject with JsonObject with BsonObject with ProtoObject {
    override def :+ (key: String, value: AnyForm): ObjectForm = underlying :+ (key -> value)
    override def +: (key: String, value: AnyForm): ObjectForm = (key -> value) +: underlying
    override def + (key: String, value: AnyForm): ObjectForm  = underlying :+ (key -> value)
    override def - (key: String): ObjectForm                  = underlying.filter(!_._1.equals(key))
    override def ++ (that: ObjectForm): ObjectForm            = underlying ++ that.underlying
    override def -- (that: ObjectForm): ObjectForm            = underlying.filter(field => !that.contains(field._1))
    override def iterator: Iterator[(String, AnyForm)]        = underlying.iterator
  }

  object ObjectForm extends BaseObjectFactory with JsonObjectFactory with BsonObjectFactory {
    override val empty: ObjectForm                                                   = new ObjectForm(immutable.FingerTrieSeq.empty)
    implicit override def Builder: Builder[(String, AnyForm)] with State[ObjectForm] = new ObjectFormBuilder(immutable.FingerTrieSeq.Builder[(String, AnyForm)])
  }

  private final class ObjectFormBuilder(underlying: Builder[(String, AnyForm)] with State[Seq[(String, AnyForm)]]) extends Builder[(String, AnyForm)] with State[ObjectForm] {
    override def append(entry: (String, AnyForm)): Unit = underlying.append(entry)
    override def clear(): Unit                          = underlying.clear()
    override def expect(count: Int): this.type          = { underlying.expect(count); this }
    override def state: ObjectForm                      = new ObjectForm(underlying.state)
    override def toString: String                       = variant.toString +"ObjectForm"+"."+"Builder"
  }


  final class SeqForm(protected val underlying: IndexedSeq[AnyForm]) extends AnyForm with BaseSeq with JsonSeq with BsonSeq with ProtoSeq {
    override def length: Int                  = underlying.length
    override def apply(index: Int): AnyForm   = underlying(index)
    override def :+ (value: AnyForm): SeqForm = underlying.:+(value)(SeqFormBuilder)
    override def +: (value: AnyForm): SeqForm = underlying.+:(value)(SeqFormBuilder)
    override def ++ (that: SeqForm): SeqForm  = underlying.++(that.underlying)(SeqFormBuilder)
    override def iterator: Iterator[AnyForm]  = underlying.iterator
  }

  object SeqForm extends BaseSeqFactory with JsonSeqFactory with BsonSeqFactory {
    override val empty: SeqForm                                         = new SeqForm(immutable.FingerTrieSeq.empty)
    implicit override def Builder: Builder[AnyForm] with State[SeqForm] = new SeqFormBuilder(immutable.FingerTrieSeq.Builder[AnyForm])
  }

  private final class SeqFormBuilder(underlying: Builder[AnyForm] with State[IndexedSeq[AnyForm]]) extends Builder[AnyForm] with State[SeqForm] {
    override def append(elem: AnyForm): Unit   = underlying.append(elem)
    override def clear(): Unit                 = underlying.clear()
    override def expect(count: Int): this.type = { underlying.expect(count); this }
    override def state: SeqForm                = new SeqForm(underlying.state)
    override def toString: String              = variant.toString +"SeqForm"+"."+"Builder"
  }


  final class SetForm(protected val underlying: Seq[AnyForm]) extends AnyForm with BaseSet with JsonSet with BsonSet with ProtoSet {
    override def size: Int                   = underlying.length
    override def + (value: AnyForm): SetForm = underlying.:+(value)(SetFormBuilder)
    override def - (value: AnyForm): SetForm = underlying.filter(!_.equals(value))(SetFormBuilder)
    override def ++ (that: SetForm): SetForm = underlying.++(that.underlying)(SetFormBuilder)
    override def -- (that: SetForm): SetForm = underlying.filter(value => !that.contains(value))(SetFormBuilder)
    override def iterator: Iterator[AnyForm] = underlying.iterator
  }

  object SetForm extends BaseSetFactory with JsonSetFactory with BsonSetFactory {
    override val empty: SetForm                                         = new SetForm(immutable.FingerTrieSeq.empty)
    implicit override def Builder: Builder[AnyForm] with State[SetForm] = new SetFormBuilder(immutable.FingerTrieSeq.Builder[AnyForm])
  }

  private final class SetFormBuilder(underlying: Builder[AnyForm] with State[Seq[AnyForm]]) extends Builder[AnyForm] with State[SetForm] {
    override def append(elem: AnyForm): Unit   = underlying.append(elem)
    override def clear(): Unit                 = underlying.clear()
    override def expect(count: Int): this.type = { underlying.expect(count); this }
    override def state: SetForm                = new SetForm(underlying.state)
    override def toString: String              = variant.toString +"SetForm"+"."+"Builder"
  }


  final class TextForm(protected val underlying: UString) extends AnyForm with BaseText with JsonText with BsonText with ProtoText {
    private[this] var utf8Size: Int = -1
    override def utf8Length: Int = {
      if (utf8Size == -1) utf8Size = super.utf8Length
      utf8Size
    }
    override def iterator: Iterator[Int] = underlying.iterator
    override def toUString: UString      = underlying
  }

  object TextForm extends BaseTextFactory {
    override val empty: TextForm                                      = new TextForm(new UString(""))
    implicit override def Builder: StringBuilder with State[TextForm] = new TextFormBuilder(UString.Builder)
  }

  private final class TextFormBuilder(underlying: StringBuilder with State[UString]) extends StringBuilder with State[TextForm] {
    override def append(c: Int): Unit           = underlying.append(c)
    override def append(cs: CharSequence): Unit = underlying.append(cs)
    override def clear(): Unit                  = underlying.clear()
    override def expect(count: Int): this.type  = { underlying.expect(count); this }
    override def state: TextForm                = new TextForm(underlying.state)
    override def toString: String               = variant.toString +"TextForm"+"."+"Builder"
  }


  final class DataForm(underlying: Loader) extends AnyForm with BaseData with JsonData with BsonData with ProtoData {
    override def endian: Endianness                = underlying.endian
    override def size: Long                        = underlying.size
    override def as[E <: Endianness](endian: E): DataForm with basis.data.ByteOrder[E] = {
      if (underlying.endian.eq(endian)) this
      else new DataForm(underlying.as(endian))
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

  object DataForm extends BaseDataFactory {
    override def endian: Endianness                  = NativeEndian
    override val empty: DataForm                     = new DataForm(ArrayData.empty)
    override def from(data: Loader): DataForm        = {
      if (data.isInstanceOf[DataForm]) data.asInstanceOf[DataForm]
      else new DataForm(data)
    }
    override def Framer: Framer with State[DataForm] = new DataFormFramer(ArrayData.Framer)
  }

  private final class DataFormFramer(underlying: Framer with State[Loader]) extends Framer with State[DataForm] {
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
    override def state: DataForm                  = new DataForm(underlying.state)
    override def clear(): Unit                    = underlying.clear()
    override def toString: String                 = variant.toString +"DataForm"+"."+"Framer"
  }


  sealed abstract class NumberForm extends AnyForm with BaseNumber with JsonNumber with BsonNumber with ProtoNumber

  private final class IntForm(override val toInt: Int) extends NumberForm with BaseInt

  private final class LongForm(override val toLong: Long) extends NumberForm with BaseLong

  private final class FloatForm(override val toFloat: Float) extends NumberForm with BaseFloat

  private final class DoubleForm(override val toDouble: Double) extends NumberForm with BaseDouble

  object NumberForm extends BaseNumberFactory {
    override def apply(value: Int): NumberForm    = new IntForm(value)
    override def apply(value: Long): NumberForm   = new LongForm(value)
    override def apply(value: Float): NumberForm  = new FloatForm(value)
    override def apply(value: Double): NumberForm = new DoubleForm(value)
  }


  final class DateForm(override val millis: Long) extends AnyForm with BaseDate with JsonDate with BsonDate with ProtoDate

  object DateForm extends BaseDateFactory {
    override def apply(millis: Long): DateForm = new DateForm(millis)
  }


  sealed abstract class BoolForm extends AnyForm with BaseBool with JsonBool with BsonBool with ProtoBool

  object TrueForm extends BoolForm {
    override def toBoolean: Boolean = true
  }

  object FalseForm extends BoolForm {
    override def toBoolean: Boolean = false
  }

  object BoolForm extends BaseBoolFactory


  sealed abstract class NullForm extends AnyForm with BaseNull with JsonNull with BsonNull with ProtoNull

  object NullForm extends NullForm


  sealed abstract class NoForm extends AnyForm with BaseNo with JsonNo with BsonNo with ProtoNo

  object NoForm extends NoForm


  implicit override lazy val AnyFormTag: ClassTag[AnyForm]       = ClassTag(Predef.classOf[AnyForm])
  implicit override lazy val ObjectFormTag: ClassTag[ObjectForm] = ClassTag(Predef.classOf[ObjectForm])
  implicit override lazy val SeqFormTag: ClassTag[SeqForm]       = ClassTag(Predef.classOf[SeqForm])
  implicit override lazy val SetFormTag: ClassTag[SetForm]       = ClassTag(Predef.classOf[SetForm])
  implicit override lazy val TextFormTag: ClassTag[TextForm]     = ClassTag(Predef.classOf[TextForm])
  implicit override lazy val DataFormTag: ClassTag[DataForm]     = ClassTag(Predef.classOf[DataForm])
  implicit override lazy val NumberFormTag: ClassTag[NumberForm] = ClassTag(Predef.classOf[NumberForm])
  implicit override lazy val DateFormTag: ClassTag[DateForm]     = ClassTag(Predef.classOf[DateForm])
  implicit override lazy val BoolFormTag: ClassTag[BoolForm]     = ClassTag(Predef.classOf[BoolForm])
  implicit override lazy val NullFormTag: ClassTag[NullForm]     = ClassTag(Predef.classOf[NullForm])
  implicit override lazy val NoFormTag: ClassTag[NoForm]         = ClassTag(Predef.classOf[NoForm])

  override def toString: String = "OmniVariant"
}
