//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis._
import basis.collections._
import basis.collections.immutable._
import basis.data._
import basis.text._
import basis.util._
import scala.reflect._

object OmniVariant extends Variant with DeltaVariant with JsonVariant with BsonVariant with ProtoVariant { variant =>
  final class ObjectState(override val state: ObjectForm, private[this] var _revert: ObjectState) extends StateObject {
    def this(state: ObjectForm) = { this(state, null); _revert = this }

    private[this] var _delta: ObjectDelta = _
    override def delta: ObjectDelta = {
      if (_delta == null) _delta =
        if (this eq revert) ObjectDelta.empty
        else revert.state.delta(state)
      _delta
    }

    override def commit: ObjectState = if (this eq revert) this else new ObjectState(state)
    override def revert: ObjectState = _revert

    override def update(state: ObjectForm): ObjectState                               = if (this.state eq state) this else new ObjectState(state, revert)
    implicit override def builder: Builder[(String, AnyForm)] with State[ObjectState] = new ObjectStateBuilder(ObjectFormBuilder, revert)

    def writeJson(builder: StringBuilder): Unit = state.writeJson(builder)
    def writeBson(writer: Writer): Unit         = state.writeBson(writer)
    def writeProto(writer: Writer): Unit        = state.writeProto(writer)

    def toJson: String  = state.toJson
    def toBson: Loader  = state.toBson
    def toProto: Loader = state.toProto
  }

  object ObjectState extends StateObjectFactory {
    override val empty: ObjectState                                                   = new ObjectState(ObjectForm.empty)
    override def apply(state: ObjectForm): ObjectState                                = new ObjectState(state)
    implicit override def Builder: Builder[(String, AnyForm)] with State[ObjectState] = new ObjectStateBuilder(ObjectFormBuilder, null)
  }

  private final class ObjectStateBuilder(underlying: Builder[(String, AnyForm)] with State[ObjectForm], revert: ObjectState) extends Builder[(String, AnyForm)] with State[ObjectState] {
    override def append(entry: (String, AnyForm)): Unit = underlying.append(entry)
    override def clear(): Unit                          = underlying.clear()
    override def expect(count: Int): this.type          = { underlying.expect(count); this }
    override def state: ObjectState                     = if (revert != null) new ObjectState(underlying.state, revert) else new ObjectState(underlying.state)
    override def toString: String                       = (String.Builder~variant.toString~'.'~"ObjectState"~'.'~"Builder").state
  }


  sealed abstract class AnyDelta extends DeltaValue with ProtoDelta

  object AnyDelta extends DeltaValueFactory with ProtoDeltaFactory


  final class ObjectDelta(protected val underlying: FingerTrieSeq[(String, AnyDelta)]) extends AnyDelta with DeltaObject with ProtoObjectDelta {
    private[this] var _index: HashTrieMap[String, AnyDelta] = null
    private[this] def index: HashTrieMap[String, AnyDelta] = {
      if (_index == null) _index = HashTrieMap.from(underlying)
      _index
    }

    override def isEmpty: Boolean = underlying.isEmpty

    override def size: Int = underlying.length

    override def contains(key: String): Boolean =
      if (size > 8) index.contains(key)
      else underlying.exists(_._1.equals(key))

    override def get(key: String): Maybe[AnyDelta] =
      if (size > 8) index.get(key)
      else underlying.find(_._1.equals(key)).map(_._2)

    override def apply(key: String): AnyDelta =
      if (size > 8) index(key)
      else {
        val these = underlying.iterator
        while (!these.isEmpty) {
          val field = these.head
          if (field._1.equals(key)) return field._2
          these.step()
        }
        throw new NoSuchElementException(key)
      }

    override def / (key: String): AnyDelta =
      if (size > 8) { if (index.contains(key)) index(key) else NoForm }
      else {
        val these = underlying.iterator
        while (!these.isEmpty) {
          val field = these.head
          if (field._1.equals(key)) return field._2
          these.step()
        }
        NoForm
      }

    override def iterator: Iterator[(String, AnyDelta)] = underlying.iterator

    override def traverse(f: ((String, AnyDelta)) => Unit): Unit = underlying.traverse(f)
  }

  object ObjectDelta extends DeltaObjectFactory {
    override val empty: ObjectDelta                                                    = new ObjectDelta(FingerTrieSeq.empty)
    implicit override def Builder: Builder[(String, AnyDelta)] with State[ObjectDelta] = new ObjectDeltaBuilder(FingerTrieSeq.Builder[(String, AnyDelta)])
  }

  private final class ObjectDeltaBuilder(underlying: Builder[(String, AnyDelta)] with State[FingerTrieSeq[(String, AnyDelta)]]) extends Builder[(String, AnyDelta)] with State[ObjectDelta] {
    override def append(entry: (String, AnyDelta)): Unit = underlying.append(entry)
    override def clear(): Unit                           = underlying.clear()
    override def expect(count: Int): this.type           = { underlying.expect(count); this }
    override def state: ObjectDelta                      = new ObjectDelta(underlying.state)
    override def toString: String                        = (String.Builder~variant.toString~'.'~"ObjectDelta"~'.'~"Builder").state
  }


  final class SetDelta(override val deletions: SetForm, override val additions: SetForm) extends AnyDelta with DeltaSet with ProtoSetDelta

  object SetDelta extends DeltaSetFactory {
    override def apply(deletions: SetForm, additions: SetForm): SetDelta = new SetDelta(deletions, additions)
  }


  sealed abstract class AnyForm extends AnyDelta with FormValue with BaseValue with JsonValue with BsonValue with ProtoValue

  object AnyForm extends BaseValueFactory with JsonValueFactory with BsonValueFactory with ProtoValueFactory


  final class SecretForm(override val data: Loader, override val iv: Loader, override val mac: Loader) extends AnyForm with ProtoSecret {
    override def writeJson(builder: StringBuilder): Unit = toObjectForm.writeJson(builder)

    override def bsonType: Byte = 0x03

    override def bsonSize: Int = toObjectForm.bsonSize

    override def writeBson(output: Writer): Unit = toObjectForm.writeBson(output)
  }

  object SecretForm extends ProtoSecretFactory {
    override def apply(data: Loader, iv: Loader, mac: Loader): SecretForm = new SecretForm(data, iv, mac)
  }


  final class ObjectForm(protected val underlying: FingerTrieSeq[(String, AnyForm)]) extends AnyForm with FormObject with BaseObject with JsonObject with BsonObject with ProtoObject {
    private[this] var _index: HashTrieMap[String, AnyForm] = null
    private[this] def index: HashTrieMap[String, AnyForm] = {
      if (_index == null) _index = HashTrieMap.from(underlying)
      _index
    }

    override def isEmpty: Boolean = underlying.isEmpty

    override def size: Int = underlying.length

    override def contains(key: String): Boolean =
      if (size > 8) index.contains(key)
      else underlying.exists(_._1.equals(key))

    override def get(key: String): Maybe[AnyForm] =
      if (size > 8) index.get(key)
      else underlying.find(_._1.equals(key)).map(_._2)

    override def apply(key: String): AnyForm =
      if (size > 8) index(key)
      else {
        val these = underlying.iterator
        while (!these.isEmpty) {
          val field = these.head
          if (field._1.equals(key)) return field._2
          these.step()
        }
        throw new NoSuchElementException(key)
      }

    override def / (key: String): AnyForm =
      if (size > 8) { if (index.contains(key)) index(key) else NoForm }
      else {
        val these = underlying.iterator
        while (!these.isEmpty) {
          val field = these.head
          if (field._1.equals(key)) return field._2
          these.step()
        }
        NoForm
      }

    override def :+ (field: (String, AnyForm)): ObjectForm = new ObjectForm(underlying :+ field)

    override def +: (field: (String, AnyForm)): ObjectForm = new ObjectForm(field +: underlying)

    override def + (key: String, value: AnyForm): ObjectForm =
      if (_index != null && !_index.contains(key)) new ObjectForm(underlying :+ (key -> value))
      else {
        var i = size - 1
        while (i >= 0 && !underlying(i)._1.equals(key)) i -= 1
        if (i >= 0) {
          if (underlying(i)._2.equals(value)) this
          else new ObjectForm(underlying.update(i, key -> value))
        }
        else new ObjectForm(underlying :+ (key -> value))
      }

    override def - (key: String): ObjectForm =
      if (_index != null && !_index.contains(key)) this
      else underlying.filter(!_._1.equals(key))(ObjectFormBuilder)

    override def ++ (that: ObjectForm): ObjectForm = underlying.++(that.underlying)(ObjectFormBuilder)

    override def -- (that: ObjectForm): ObjectForm = underlying.filter(field => !that.contains(field._1))(ObjectFormBuilder)

    override def iterator: Iterator[(String, AnyForm)] = underlying.iterator

    override def traverse(f: ((String, AnyForm)) => Unit): Unit = underlying.traverse(f)
  }

  object ObjectForm extends BaseObjectFactory with JsonObjectFactory with BsonObjectFactory {
    override val empty: ObjectForm                                                   = new ObjectForm(FingerTrieSeq.empty)
    implicit override def Builder: Builder[(String, AnyForm)] with State[ObjectForm] = new ObjectFormBuilder(FingerTrieSeq.Builder[(String, AnyForm)])
  }

  private final class ObjectFormBuilder(underlying: Builder[(String, AnyForm)] with State[FingerTrieSeq[(String, AnyForm)]]) extends Builder[(String, AnyForm)] with State[ObjectForm] {
    override def append(entry: (String, AnyForm)): Unit = underlying.append(entry)
    override def clear(): Unit                          = underlying.clear()
    override def expect(count: Int): this.type          = { underlying.expect(count); this }
    override def state: ObjectForm                      = new ObjectForm(underlying.state)
    override def toString: String                       = (String.Builder~variant.toString~'.'~"ObjectForm"~'.'~"Builder").state
  }


  final class SeqForm(protected val underlying: FingerTrieSeq[AnyForm]) extends AnyForm with BaseSeq with JsonSeq with BsonSeq with ProtoSeq {
    override def isEmpty: Boolean                   = underlying.isEmpty
    override def length: Int                        = underlying.length
    override def apply(index: Int): AnyForm         = underlying(index)
    override def :+ (value: AnyForm): SeqForm       = new SeqForm(underlying :+ value)
    override def +: (value: AnyForm): SeqForm       = new SeqForm(value +: underlying)
    override def ++ (that: SeqForm): SeqForm        = underlying.++(that.underlying)(SeqFormBuilder)
    override def iterator: Iterator[AnyForm]        = underlying.iterator
    override def traverse(f: AnyForm => Unit): Unit = underlying.traverse(f)
  }

  object SeqForm extends BaseSeqFactory with JsonSeqFactory with BsonSeqFactory {
    override val empty: SeqForm                                         = new SeqForm(FingerTrieSeq.empty)
    implicit override def Builder: Builder[AnyForm] with State[SeqForm] = new SeqFormBuilder(FingerTrieSeq.Builder[AnyForm])
  }

  private final class SeqFormBuilder(underlying: Builder[AnyForm] with State[FingerTrieSeq[AnyForm]]) extends Builder[AnyForm] with State[SeqForm] {
    override def append(elem: AnyForm): Unit   = underlying.append(elem)
    override def clear(): Unit                 = underlying.clear()
    override def expect(count: Int): this.type = { underlying.expect(count); this }
    override def state: SeqForm                = new SeqForm(underlying.state)
    override def toString: String              = (String.Builder~variant.toString~'.'~"SeqForm"~'.'~"Builder").state
  }


  final class SetForm(protected val underlying: HashTrieSet[AnyForm]) extends AnyForm with FormSet with BaseSet with JsonSet with BsonSet with ProtoSet {
    override def isEmpty: Boolean                   = underlying.isEmpty
    override def size: Int                          = underlying.size
    override def contains(value: AnyForm): Boolean  = underlying.contains(value)
    override def + (value: AnyForm): SetForm        = new SetForm(underlying + value)
    override def - (value: AnyForm): SetForm        = new SetForm(underlying - value)
    override def ++ (that: SetForm): SetForm        = underlying.++(that.underlying)(SetFormBuilder)
    override def -- (that: SetForm): SetForm        = underlying.filter(value => !that.contains(value))(SetFormBuilder)
    override def iterator: Iterator[AnyForm]        = underlying.iterator
    override def traverse(f: AnyForm => Unit): Unit = underlying.traverse(f)
  }

  object SetForm extends BaseSetFactory with JsonSetFactory with BsonSetFactory {
    override val empty: SetForm                                         = new SetForm(HashTrieSet.empty)
    implicit override def Builder: Builder[AnyForm] with State[SetForm] = new SetFormBuilder(HashTrieSet.Builder[AnyForm])
  }

  private final class SetFormBuilder(underlying: Builder[AnyForm] with State[HashTrieSet[AnyForm]]) extends Builder[AnyForm] with State[SetForm] {
    override def append(elem: AnyForm): Unit   = underlying.append(elem)
    override def clear(): Unit                 = underlying.clear()
    override def expect(count: Int): this.type = { underlying.expect(count); this }
    override def state: SetForm                = new SetForm(underlying.state)
    override def toString: String              = (String.Builder~variant.toString~'.'~"SetForm"~'.'~"Builder").state
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
    override def toString: String               = (String.Builder~variant.toString~'.'~"TextForm"~'.'~"Builder").state
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
    override def in(domain: Variant): domain.DataForm =
      if (variant eq domain) asInstanceOf[domain.DataForm]
      else domain.DataForm.from(underlying)
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
    override def toString: String                 = (String.Builder~variant.toString~'.'~"DataForm"~'.'~"Framer").state
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


  sealed abstract class NoForm extends AnyForm with BaseNo with JsonNo with BsonNo with ProtoNo {
    override def isDefined: Boolean = false
  }

  object NoForm extends NoForm


  implicit override lazy val ObjectStateTag: ClassTag[ObjectState] = ClassTag(Predef.classOf[ObjectState])
  implicit override lazy val AnyDeltaTag: ClassTag[AnyDelta]       = ClassTag(Predef.classOf[AnyDelta])
  implicit override lazy val ObjectDeltaTag: ClassTag[ObjectDelta] = ClassTag(Predef.classOf[ObjectDelta])
  implicit override lazy val SetDeltaTag: ClassTag[SetDelta]       = ClassTag(Predef.classOf[SetDelta])
  implicit override lazy val AnyFormTag: ClassTag[AnyForm]         = ClassTag(Predef.classOf[AnyForm])
  implicit override lazy val SecretFormTag: ClassTag[SecretForm]   = ClassTag(Predef.classOf[SecretForm])
  implicit override lazy val ObjectFormTag: ClassTag[ObjectForm]   = ClassTag(Predef.classOf[ObjectForm])
  implicit override lazy val SeqFormTag: ClassTag[SeqForm]         = ClassTag(Predef.classOf[SeqForm])
  implicit override lazy val SetFormTag: ClassTag[SetForm]         = ClassTag(Predef.classOf[SetForm])
  implicit override lazy val TextFormTag: ClassTag[TextForm]       = ClassTag(Predef.classOf[TextForm])
  implicit override lazy val DataFormTag: ClassTag[DataForm]       = ClassTag(Predef.classOf[DataForm])
  implicit override lazy val NumberFormTag: ClassTag[NumberForm]   = ClassTag(Predef.classOf[NumberForm])
  implicit override lazy val DateFormTag: ClassTag[DateForm]       = ClassTag(Predef.classOf[DateForm])
  implicit override lazy val BoolFormTag: ClassTag[BoolForm]       = ClassTag(Predef.classOf[BoolForm])
  implicit override lazy val NullFormTag: ClassTag[NullForm]       = ClassTag(Predef.classOf[NullForm])
  implicit override lazy val NoFormTag: ClassTag[NoForm]           = ClassTag(Predef.classOf[NoForm])

  override def toString: String = "OmniVariant"
}
