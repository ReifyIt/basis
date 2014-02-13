//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.collections._
import basis.memory._
import basis.text._
import scala.reflect._

class OmniVariant
  extends Variant
  with json.JsonVariant
  with bson.BsonVariant {

  override type AnyForm = OmniValue

  override lazy val AnyForm
    : BaseValueFactory with
      JsonValueFactory with
      BsonValueFactory =
    new OmniValueFactory

  implicit override lazy val AnyFormTag: ClassTag[AnyForm] =
    ClassTag(Predef.classOf[OmniValue])

  abstract class OmniValue
    extends BaseValue
    with JsonValue
    with BsonValue

  protected class OmniValueFactory
    extends BaseValueFactory
    with JsonValueFactory
    with BsonValueFactory


  override type ObjectForm = OmniObject

  override lazy val ObjectForm
    : BaseObjectFactory with
      JsonObjectFactory with
      BsonObjectFactory =
    new OmniObjectFactory

  implicit override lazy val ObjectFormTag: ClassTag[ObjectForm] =
    ClassTag(Predef.classOf[OmniObject])

  class OmniObject(protected val underlying: Seq[(String, AnyForm)])
    extends OmniValue
    with BaseObject
    with JsonObject
    with BsonObject {

    override def iterator: Iterator[(String, AnyForm)] = underlying.iterator
  }

  protected class OmniObjectFactory
    extends BaseObjectFactory
    with JsonObjectFactory
    with BsonObjectFactory {

    override val empty: ObjectForm = new OmniObject(immutable.Vector.empty)

    implicit override def Builder(): Builder[(String, AnyForm)] with State[ObjectForm] =
      new OmniObjectBuilder(immutable.Vector.Builder[(String, AnyForm)]())
  }

  protected final class OmniObjectBuilder(underlying: Builder[(String, AnyForm)] with State[Seq[(String, AnyForm)]])
    extends Builder[(String, AnyForm)] with State[ObjectForm] {

    override def expect(count: Int): this.type = { underlying.expect(count); this }

    override def append(entry: (String, AnyForm)): Unit = underlying.append(entry)

    override def state: ObjectForm = new OmniObject(underlying.state)

    override def clear(): Unit = underlying.clear()

    override def toString: String = "ObjectForm"+"."+"Builder"+"()"
  }


  override type SeqForm = OmniSeq

  override lazy val SeqForm
    : BaseSeqFactory with
      JsonSeqFactory with
      BsonSeqFactory =
    new OmniSeqFactory

  implicit override lazy val SeqFormTag: ClassTag[SeqForm] =
    ClassTag(Predef.classOf[OmniSeq])

  class OmniSeq(protected val underlying: IndexedSeq[AnyForm])
    extends OmniValue
    with BaseSeq
    with JsonSeq
    with BsonSeq {

    override def length: Int = underlying.length

    override def apply(index: Int): AnyForm = underlying(index)

    override def iterator: Iterator[AnyForm] = underlying.iterator
  }

  protected class OmniSeqFactory
    extends BaseSeqFactory
    with JsonSeqFactory
    with BsonSeqFactory {

    override val empty: SeqForm = new OmniSeq(immutable.Vector.empty)

    implicit override def Builder(): Builder[AnyForm] with State[SeqForm] =
      new OmniSeqBuilder(immutable.Vector.Builder[AnyForm]())
  }

  protected final class OmniSeqBuilder(underlying: Builder[AnyForm] with State[IndexedSeq[AnyForm]])
    extends Builder[AnyForm] with State[SeqForm] {

    override def expect(count: Int): this.type = { underlying.expect(count); this }

    override def append(elem: AnyForm): Unit = underlying.append(elem)

    override def state: SeqForm = new OmniSeq(underlying.state)

    override def clear(): Unit = underlying.clear()

    override def toString: String = "SeqForm"+"."+"Builder"+"()"
  }


  override type SetForm = OmniSet

  override lazy val SetForm
    : BaseSetFactory with
      JsonSetFactory with
      BsonSetFactory =
    new OmniSetFactory

  implicit override lazy val SetFormTag: ClassTag[SetForm] =
    ClassTag(Predef.classOf[OmniSet])

  class OmniSet(protected val underlying: Seq[AnyForm])
    extends OmniValue
    with BaseSet
    with JsonSet
    with BsonSet {

    override def iterator: Iterator[AnyForm] = underlying.iterator
  }

  protected class OmniSetFactory
    extends BaseSetFactory
    with JsonSetFactory
    with BsonSetFactory {

    override val empty: SetForm = new OmniSet(immutable.Vector.empty)

    implicit override def Builder(): Builder[AnyForm] with State[SetForm] =
      new OmniSetBuilder(immutable.Vector.Builder[AnyForm]())
  }

  protected final class OmniSetBuilder(underlying: Builder[AnyForm] with State[Seq[AnyForm]])
    extends Builder[AnyForm] with State[SetForm] {

    override def expect(count: Int): this.type = { underlying.expect(count); this }

    override def append(elem: AnyForm): Unit = underlying.append(elem)

    override def state: SetForm = new OmniSet(underlying.state)

    override def clear(): Unit = underlying.clear()

    override def toString: String = "SetForm"+"."+"Builder"+"()"
  }


  override type BinaryForm = OmniBinary

  override lazy val BinaryForm: BaseBinaryFactory =
    new OmniBinaryFactory

  implicit override lazy val BinaryFormTag: ClassTag[BinaryForm] =
    ClassTag(Predef.classOf[OmniBinary])

  class OmniBinary(protected override val underlying: Data)
    extends OmniValue
    with BaseBinary
    with JsonBinary
    with BsonBinary
    with ProxyLoader {

    override def size: Long = underlying.size
  }

  protected class OmniBinaryFactory extends BaseBinaryFactory {
    override val empty: BinaryForm = new BinaryForm(Data(0L))

    override def Framer(): Framer with State[BinaryForm] =
      new OmniBinaryFramer(Data.Framer())
  }

  protected final class OmniBinaryFramer(protected override val underlying: Framer with State[Data])
    extends ProxyFramer with State[BinaryForm] {

    override def state: BinaryForm = new OmniBinary(underlying.state)
  }


  override type StringForm = OmniString

  override lazy val StringForm: BaseStringFactory =
    new OmniStringFactory

  implicit override lazy val StringFormTag: ClassTag[StringForm] =
    ClassTag(Predef.classOf[OmniString])

  class OmniString(protected val underlying: UString)
    extends OmniValue
    with BaseString
    with JsonString
    with BsonString {

    override def iterator: Iterator[Int] = underlying.iterator

    override def toUString: UString = underlying
  }

  protected class OmniStringFactory extends BaseStringFactory {
    override val empty: StringForm = new OmniString(new UString(""))

    override def apply(value: CharSequence): StringForm =
      new OmniString(new UString(value.toString))

    implicit override def Builder(): StringBuilder with State[StringForm] =
      new OmniStringBuilder(UString.Builder())
  }

  protected final class OmniStringBuilder(underlying: StringBuilder with State[UString])
    extends StringBuilder with State[StringForm] {

    override def expect(count: Int): this.type = { underlying.expect(count); this }

    override def append(c: Int): Unit = underlying.append(c)

    override def append(cs: CharSequence): Unit = underlying.append(cs)

    override def state: StringForm = new OmniString(underlying.state)

    override def clear(): Unit = underlying.clear()

    override def toString: String = "StringForm"+"."+"Builder"+"()"
  }


  override type NumberForm = OmniNumber

  override lazy val NumberForm: BaseNumberFactory =
    new OmniNumberFactory

  implicit override lazy val NumberFormTag: ClassTag[NumberForm] =
    ClassTag(Predef.classOf[OmniNumber])

  abstract class OmniNumber
    extends OmniValue
    with BaseNumber
    with JsonNumber
    with BsonNumber

  protected class OmniInt(override val toInt: Int)
    extends OmniNumber with BaseInt

  protected class OmniLong(override val toLong: Long)
    extends OmniNumber with BaseLong

  protected class OmniFloat(override val toFloat: Float)
    extends OmniNumber with BaseFloat

  protected class OmniDouble(override val toDouble: Double)
    extends OmniNumber with BaseDouble

  protected class OmniDecimalString(override val toDecimalString: String)
    extends OmniNumber with BaseDecimalString

  protected class OmniNumberFactory extends BaseNumberFactory {
    override def apply(value: Byte): NumberForm = new OmniInt(value.toInt)

    override def apply(value: Short): NumberForm = new OmniInt(value.toInt)

    override def apply(value: Int): NumberForm = new OmniInt(value)

    override def apply(value: Long): NumberForm = new OmniLong(value)

    override def apply(value: Float): NumberForm = new OmniFloat(value)

    override def apply(value: Double): NumberForm = new OmniDouble(value)

    override def apply(value: String): NumberForm = new OmniDecimalString(value)
  }


  override type DateForm = OmniDate

  override lazy val DateForm: BaseDateFactory =
    new OmniDateFactory

  implicit override lazy val DateFormTag: ClassTag[DateForm] =
    ClassTag(Predef.classOf[OmniDate])

  class OmniDate(override val millis: Long)
    extends OmniValue
    with BaseDate
    with JsonDate
    with BsonDate

  protected class OmniDateFactory extends BaseDateFactory {
    override def apply(millis: Long): DateForm =
      new OmniDate(millis)
  }


  override type BooleanForm = OmniBoolean

  override lazy val BooleanForm: BaseBooleanFactory =
    new OmniBooleanFactory

  override lazy val TrueForm: BooleanForm = new OmniBoolean(true)

  override lazy val FalseForm: BooleanForm = new OmniBoolean(false)

  implicit override lazy val BooleanFormTag: ClassTag[BooleanForm] =
    ClassTag(Predef.classOf[OmniBoolean])

  class OmniBoolean(override val toBoolean: Boolean)
    extends OmniValue
    with BaseBoolean
    with JsonBoolean
    with BsonBoolean

  protected class OmniBooleanFactory extends BaseBooleanFactory


  override type NullForm = OmniNull

  override lazy val NullForm: NullForm = new OmniNull

  implicit override lazy val NullFormTag: ClassTag[NullForm] =
    ClassTag(Predef.classOf[OmniNull])

  class OmniNull
    extends OmniValue
    with BaseNull
    with JsonNull
    with BsonNull


  override type UndefinedForm = OmniUndefined

  override lazy val UndefinedForm: UndefinedForm = new OmniUndefined

  implicit override lazy val UndefinedFormTag: ClassTag[UndefinedForm] =
    ClassTag(Predef.classOf[OmniUndefined])

  class OmniUndefined
    extends OmniValue
    with BaseUndefined
    with JsonUndefined
    with BsonUndefined
}

object OmniVariant extends OmniVariant
