//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.collections._
import basis.data._
import basis.text._
import scala.reflect._

class OmniVariant extends Variant with JsonVariant with BsonVariant {
  import Predef.classOf

  override type AnyForm       = OmniValue
  override type ObjectForm    = OmniObject
  override type SeqForm       = OmniSeq
  override type SetForm       = OmniSet
  override type BinaryForm    = OmniBinary
  override type StringForm    = OmniString
  override type NumberForm    = OmniNumber
  override type DateForm      = OmniDate
  override type BooleanForm   = OmniBoolean
  override type NullForm      = OmniNull
  override type UndefinedForm = OmniUndefined

  override lazy val AnyForm: BaseValueFactory with JsonValueFactory with BsonValueFactory       = new OmniValueFactory
  override lazy val ObjectForm: BaseObjectFactory with JsonObjectFactory with BsonObjectFactory = new OmniObjectFactory
  override lazy val SeqForm: BaseSeqFactory with JsonSeqFactory with BsonSeqFactory             = new OmniSeqFactory
  override lazy val SetForm: BaseSetFactory with JsonSetFactory with BsonSetFactory             = new OmniSetFactory

  override lazy val BinaryForm: BaseBinaryFactory   = new OmniBinaryFactory
  override lazy val StringForm: BaseStringFactory   = new OmniStringFactory
  override lazy val NumberForm: BaseNumberFactory   = new OmniNumberFactory
  override lazy val DateForm: BaseDateFactory       = new OmniDateFactory
  override lazy val BooleanForm: BaseBooleanFactory = new OmniBooleanFactory

  override lazy val TrueForm: BooleanForm           = new OmniBoolean(true)
  override lazy val FalseForm: BooleanForm          = new OmniBoolean(false)
  override lazy val NullForm: NullForm              = new OmniNull
  override lazy val UndefinedForm: UndefinedForm    = new OmniUndefined

  implicit override lazy val AnyFormTag: ClassTag[AnyForm]             = ClassTag(classOf[OmniValue])
  implicit override lazy val ObjectFormTag: ClassTag[ObjectForm]       = ClassTag(classOf[OmniObject])
  implicit override lazy val SeqFormTag: ClassTag[SeqForm]             = ClassTag(classOf[OmniSeq])
  implicit override lazy val SetFormTag: ClassTag[SetForm]             = ClassTag(classOf[OmniSet])
  implicit override lazy val BinaryFormTag: ClassTag[BinaryForm]       = ClassTag(classOf[OmniBinary])
  implicit override lazy val StringFormTag: ClassTag[StringForm]       = ClassTag(classOf[OmniString])
  implicit override lazy val NumberFormTag: ClassTag[NumberForm]       = ClassTag(classOf[OmniNumber])
  implicit override lazy val DateFormTag: ClassTag[DateForm]           = ClassTag(classOf[OmniDate])
  implicit override lazy val BooleanFormTag: ClassTag[BooleanForm]     = ClassTag(classOf[OmniBoolean])
  implicit override lazy val NullFormTag: ClassTag[NullForm]           = ClassTag(classOf[OmniNull])
  implicit override lazy val UndefinedFormTag: ClassTag[UndefinedForm] = ClassTag(classOf[OmniUndefined])


  abstract class OmniValue extends BaseValue with JsonValue with BsonValue

  protected class OmniValueFactory extends BaseValueFactory with JsonValueFactory with BsonValueFactory


  class OmniObject(protected val underlying: Seq[(String, AnyForm)]) extends OmniValue with BaseObject with JsonObject with BsonObject {
    override def iterator: Iterator[(String, AnyForm)] = underlying.iterator
  }

  protected class OmniObjectFactory extends BaseObjectFactory with JsonObjectFactory with BsonObjectFactory {
    override val empty: ObjectForm                                                     = new OmniObject(immutable.Vector.empty)
    implicit override def Builder(): Builder[(String, AnyForm)] with State[ObjectForm] = new OmniObjectBuilder(immutable.Vector.Builder[(String, AnyForm)])
  }

  protected final class OmniObjectBuilder(underlying: Builder[(String, AnyForm)] with State[Seq[(String, AnyForm)]]) extends Builder[(String, AnyForm)] with State[ObjectForm] {
    override def append(entry: (String, AnyForm)): Unit = underlying.append(entry)
    override def clear(): Unit                          = underlying.clear()
    override def expect(count: Int): this.type          = { underlying.expect(count); this }
    override def state: ObjectForm                      = new OmniObject(underlying.state)
    override def toString: String                       = "ObjectForm"+"."+"Builder"+"()"
  }


  class OmniSeq(protected val underlying: IndexedSeq[AnyForm]) extends OmniValue with BaseSeq with JsonSeq with BsonSeq {
    override def length: Int                 = underlying.length
    override def apply(index: Int): AnyForm  = underlying(index)
    override def iterator: Iterator[AnyForm] = underlying.iterator
  }

  protected class OmniSeqFactory extends BaseSeqFactory with JsonSeqFactory with BsonSeqFactory {
    override val empty: SeqForm                                           = new OmniSeq(immutable.Vector.empty)
    implicit override def Builder(): Builder[AnyForm] with State[SeqForm] = new OmniSeqBuilder(immutable.Vector.Builder[AnyForm])
  }

  protected final class OmniSeqBuilder(underlying: Builder[AnyForm] with State[IndexedSeq[AnyForm]]) extends Builder[AnyForm] with State[SeqForm] {
    override def append(elem: AnyForm): Unit   = underlying.append(elem)
    override def clear(): Unit                 = underlying.clear()
    override def expect(count: Int): this.type = { underlying.expect(count); this }
    override def state: SeqForm                = new OmniSeq(underlying.state)
    override def toString: String              = "SeqForm"+"."+"Builder"+"()"
  }


  class OmniSet(protected val underlying: Seq[AnyForm]) extends OmniValue with BaseSet with JsonSet with BsonSet {
    override def size: Int                   = underlying.length
    override def iterator: Iterator[AnyForm] = underlying.iterator
  }

  protected class OmniSetFactory extends BaseSetFactory with JsonSetFactory with BsonSetFactory {
    override val empty: SetForm                                           = new OmniSet(immutable.Vector.empty)
    implicit override def Builder(): Builder[AnyForm] with State[SetForm] = new OmniSetBuilder(immutable.Vector.Builder[AnyForm])
  }

  protected final class OmniSetBuilder(underlying: Builder[AnyForm] with State[Seq[AnyForm]]) extends Builder[AnyForm] with State[SetForm] {
    override def append(elem: AnyForm): Unit   = underlying.append(elem)
    override def clear(): Unit                 = underlying.clear()
    override def expect(count: Int): this.type = { underlying.expect(count); this }
    override def state: SetForm                = new OmniSet(underlying.state)
    override def toString: String              = "SetForm"+"."+"Builder"+"()"
  }


  class OmniBinary(underlying: Loader) extends OmniValue with BaseBinary with JsonBinary with BsonBinary {
    override def endian: Endianness                = underlying.endian
    override def size: Long                        = underlying.size
    override def loadByte(address: Long): Byte     = underlying.loadByte(address)
    override def loadShort(address: Long): Short   = underlying.loadShort(address)
    override def loadInt(address: Long): Int       = underlying.loadInt(address)
    override def loadLong(address: Long): Long     = underlying.loadLong(address)
    override def loadFloat(address: Long): Float   = underlying.loadFloat(address)
    override def loadDouble(address: Long): Double = underlying.loadDouble(address)
    override def reader(address: Long): Reader     = underlying.reader(address)
  }

  protected class OmniBinaryFactory extends BaseBinaryFactory {
    override def endian: Endianness = NativeEndian
    override val empty: BinaryForm                       = new BinaryForm(ByteVector.empty)
    override def Framer(): Framer with State[BinaryForm] = new OmniBinaryFramer(ByteVector.Framer())
  }

  protected final class OmniBinaryFramer(underlying: Framer with State[Loader]) extends Framer with State[BinaryForm] {
    override def endian: Endianness               = underlying.endian
    override def writeByte(value: Byte): Unit     = underlying.writeByte(value)
    override def writeShort(value: Short): Unit   = underlying.writeShort(value)
    override def writeInt(value: Int): Unit       = underlying.writeInt(value)
    override def writeLong(value: Long): Unit     = underlying.writeLong(value)
    override def writeFloat(value: Float): Unit   = underlying.writeFloat(value)
    override def writeDouble(value: Double): Unit = underlying.writeDouble(value)
    override def writeData(data: Loader): Unit    = underlying.writeData(data)
    override def expect(count: Long): this.type   = { underlying.expect(count); this }
    override def state: BinaryForm                = new OmniBinary(underlying.state)
    override def clear(): Unit                    = underlying.clear()
  }


  class OmniString(protected val underlying: UString) extends OmniValue with BaseString with JsonString with BsonString {
    override def iterator: Iterator[Int] = underlying.iterator
    override def toUString: UString      = underlying
  }

  protected class OmniStringFactory extends BaseStringFactory {
    override val empty: StringForm                                        = new OmniString(new UString(""))
    implicit override def Builder(): StringBuilder with State[StringForm] = new OmniStringBuilder(UString.Builder())
  }

  protected final class OmniStringBuilder(underlying: StringBuilder with State[UString]) extends StringBuilder with State[StringForm] {
    override def append(c: Int): Unit           = underlying.append(c)
    override def append(cs: CharSequence): Unit = underlying.append(cs)
    override def clear(): Unit                  = underlying.clear()
    override def expect(count: Int): this.type  = { underlying.expect(count); this }
    override def state: StringForm              = new OmniString(underlying.state)
    override def toString: String               = "StringForm"+"."+"Builder"+"()"
  }


  abstract class OmniNumber extends OmniValue with BaseNumber with JsonNumber with BsonNumber

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


  class OmniDate(override val millis: Long) extends OmniValue with BaseDate with JsonDate with BsonDate

  protected class OmniDateFactory extends BaseDateFactory {
    override def apply(millis: Long): DateForm = new OmniDate(millis)
  }


  class OmniBoolean(override val toBoolean: Boolean) extends OmniValue with BaseBoolean with JsonBoolean with BsonBoolean

  protected class OmniBooleanFactory extends BaseBooleanFactory


  class OmniNull extends OmniValue with BaseNull with JsonNull with BsonNull


  class OmniUndefined extends OmniValue with BaseUndefined with JsonUndefined with BsonUndefined
}

object OmniVariant extends OmniVariant
