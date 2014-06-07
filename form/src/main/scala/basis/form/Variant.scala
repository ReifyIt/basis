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
import scala.reflect._
import scala.runtime._

/** A loosely structured abstract data model.
  *
  * @contentDiagram hideNodes "basis.form.Variant.BaseValue" "basis.form.Variant.BaseObject" "basis.form.Variant.BaseSeq" "basis.form.Variant.BaseSet" "basis.form.Variant.BaseData" "basis.form.Variant.BaseText" "basis.form.Variant.BaseNumber" "basis.form.Variant.BaseDate" "basis.form.Variant.BaseBool" "basis.form.Variant.BaseNull" "basis.form.Variant.BaseNo"
  */
trait Variant { variant =>
  /** The variant top type.
    * @template */
  type AnyForm <: BaseValue

  /** An association from string keys to variant forms.
    * @template */
  type ObjectForm <: BaseObject with AnyForm

  /** A sequence of variant forms.
    * @template */
  type SeqForm <: BaseSeq with AnyForm

  /** A set of variant forms.
    * @template */
  type SetForm <: BaseSet with AnyForm

  /** A Unicode® character sequence variant form.
    * @template */
  type TextForm <: BaseText with AnyForm

  /** A binary data variant form.
    * @template */
  type DataForm <: BaseData with AnyForm

  /** A numeric variant form.
    * @template */
  type NumberForm <: BaseNumber with AnyForm

  /** A date variant form.
    * @template */
  type DateForm <: BaseDate with AnyForm

  /** A boolean variant form.
    * @template */
  type BoolForm <: BaseBool with AnyForm

  /** A null variant form.
    * @template */
  type NullForm <: BaseNull with AnyForm

  /** An undefined variant form.
    * @template */
  type NoForm <: BaseNo with AnyForm

  val AnyForm: BaseValueFactory
  val ObjectForm: BaseObjectFactory
  val SeqForm: BaseSeqFactory
  val SetForm: BaseSetFactory
  val TextForm: BaseTextFactory
  val DataForm: BaseDataFactory
  val NumberForm: BaseNumberFactory
  val DateForm: BaseDateFactory
  val BoolForm: BaseBoolFactory

  def TrueForm: BoolForm
  def FalseForm: BoolForm
  def NullForm: NullForm
  def NoForm: NoForm

  implicit def ObjectFormBuilder: Builder[(String, AnyForm)] with From[ObjectForm] with State[ObjectForm] = ObjectForm.Builder
  implicit def SeqFormBuilder: Builder[AnyForm] with From[SeqForm] with State[SeqForm]                    = SeqForm.Builder
  implicit def SetFormBuilder: Builder[AnyForm] with From[SetForm] with State[SetForm]                    = SetForm.Builder
  implicit def TextFormBuilder: StringBuilder with From[TextForm] with State[TextForm]                    = TextForm.Builder
  implicit def DataFormFramer: Framer with From[DataForm] with State[DataForm]                            = DataForm.Framer

  implicit lazy val StringToForm: String => TextForm   = new StringToForm
  implicit lazy val IntToForm: Int => NumberForm       = new IntToForm
  implicit lazy val LongToForm: Long => NumberForm     = new LongToForm
  implicit lazy val FloatToForm: Float => NumberForm   = new FloatToForm
  implicit lazy val DoubleToForm: Double => NumberForm = new DoubleToForm
  implicit lazy val BooleanToForm: Boolean => BoolForm = new BooleanToForm

  implicit def AnyFormTag: ClassTag[AnyForm]
  implicit def ObjectFormTag: ClassTag[ObjectForm]
  implicit def SeqFormTag: ClassTag[SeqForm]
  implicit def SetFormTag: ClassTag[SetForm]
  implicit def TextFormTag: ClassTag[TextForm]
  implicit def DataFormTag: ClassTag[DataForm]
  implicit def NumberFormTag: ClassTag[NumberForm]
  implicit def DateFormTag: ClassTag[DateForm]
  implicit def BoolFormTag: ClassTag[BoolForm]
  implicit def NullFormTag: ClassTag[NullForm]
  implicit def NoFormTag: ClassTag[NoForm]


  trait BaseValue { this: AnyForm =>
    def isDefined: Boolean = true

    def isObjectForm: Boolean = false
    def isSeqForm: Boolean    = false
    def isSetForm: Boolean    = false
    def isTextForm: Boolean   = false
    def isDataForm: Boolean   = false
    def isNumberForm: Boolean = false
    def isDateForm: Boolean   = false
    def isBoolForm: Boolean   = false
    def isNullForm: Boolean   = false

    def asObjectForm: ObjectForm = throw new MatchError("not an ObjectForm")
    def asSeqForm: SeqForm       = throw new MatchError("not a SeqForm")
    def asSetForm: SetForm       = throw new MatchError("not a SetForm")
    def asTextForm: TextForm     = throw new MatchError("not a TextForm")
    def asDataForm: DataForm     = throw new MatchError("not a DataForm")
    def asNumberForm: NumberForm = throw new MatchError("not a NumberForm")
    def asDateForm: DateForm     = throw new MatchError("not a DateForm")
    def asBoolForm: BoolForm     = throw new MatchError("not a BoolForm")
    def asNullForm: NullForm     = throw new MatchError("not a NullForm")

    def / (key: String): AnyForm = NoForm
    def / (index: Int): AnyForm  = NoForm

    def cast[T](implicit T: Mold[T]): Maybe[T]                           = T.cast(variant)(this)
    def coerce[@specialized(Mold.Specialized) T](implicit T: Mold[T]): T = T.cast(variant)(this).bindOrElse(T.identity)
  }

  trait BaseValueFactory {
    /** Encodes a typed value as a variant form using an implicit `Mold`. */
    def apply[@specialized(Mold.Specialized) T](value: T)(implicit T: Mold[T]): AnyForm = T.form(variant)(value)
    override def toString: String = "AnyForm"
  }


  trait BaseObject extends Equals with Immutable with Family[ObjectForm] with Map[String, AnyForm] with BaseValue { this: ObjectForm =>
    override def isObjectForm: Boolean          = true
    override def asObjectForm: ObjectForm       = this
    override def / (key: String): AnyForm       = get(key).bindOrElse(NoForm)
    protected override def stringPrefix: String = "ObjectForm"
  }

  trait BaseObjectFactory extends special.MapSource[ObjectForm, String, AnyForm] {
    override def toString: String = "ObjectForm"
  }


  trait BaseSeq extends Equals with Immutable with Family[SeqForm] with IndexedSeq[AnyForm] with BaseValue { this: SeqForm =>
    override def isSeqForm: Boolean             = true
    override def asSeqForm: SeqForm             = this
    override def / (index: Int): AnyForm        = if (0 <= index && index < length) this(index) else NoForm
    protected override def stringPrefix: String = "SeqForm"
  }

  trait BaseSeqFactory extends special.SeqSource[SeqForm, AnyForm] {
    override def toString: String = "SeqForm"
  }


  trait BaseSet extends Equals with Immutable with Family[SetForm] with Set[AnyForm] with BaseValue { this: SetForm =>
    override def isSetForm: Boolean             = true
    override def asSetForm: SetForm             = this
    protected override def stringPrefix: String = "SetForm"
  }

  trait BaseSetFactory extends special.SetSource[SetForm, AnyForm] {
    override def toString: String = "SetForm"
  }


  trait BaseText extends Equals with Family[TextForm] with UTF with BaseValue { this: TextForm =>
    override def isTextForm: Boolean            = true
    override def asTextForm: TextForm           = this
    protected override def stringPrefix: String = "TextForm"
  }

  trait BaseTextFactory extends StringFactory[TextForm] {
    override def toString: String = "TextForm"
  }


  trait BaseData extends Equals with Family[DataForm] with Loader with BaseValue { this: DataForm =>
    override def isDataForm: Boolean            = true
    override def asDataForm: DataForm           = this
    protected override def stringPrefix: String = "DataForm"
  }

  trait BaseDataFactory extends DataFactory[DataForm] {
    override def toString: String = "DataForm"
  }


  trait BaseNumber extends Equals with BaseValue { this: NumberForm =>
    override def isNumberForm: Boolean    = true
    override def asNumberForm: NumberForm = this

    def isNaN: Boolean
    def isInfinite: Boolean

    def isValidByte: Boolean
    def isValidShort: Boolean
    def isValidInt: Boolean
    def isValidLong: Boolean
    def isValidFloat: Boolean
    def isValidDouble: Boolean

    def toByte: Byte
    def toShort: Short
    def toInt: Int
    def toLong: Long
    def toFloat: Float
    def toDouble: Double

    def toDecimalString: String

    override def canEqual(other: Any): Boolean = other.isInstanceOf[BaseNumber]

    override def equals(other: Any): Boolean = eq(other.asInstanceOf[AnyRef]) || (other match {
      case that: BaseNumber =>
        that.canEqual(this) &&
        isValidByte && that.isValidByte && toByte == that.toByte ||
        isValidShort && that.isValidShort && toShort == that.toShort ||
        isValidInt && that.isValidInt && toInt == that.toInt ||
        isValidLong && that.isValidLong && toLong == that.toLong ||
        isValidFloat && that.isValidFloat && toFloat == that.toFloat ||
        isValidDouble && that.isValidDouble && toDouble == that.toDouble ||
        toDecimalString.equals(that.toDecimalString)
      case _ => false
    })

    override def hashCode: Int = {
      import basis.util.MurmurHash3._
      val h =
        if (isValidByte) hash(toByte)
        else if (isValidShort) hash(toShort)
        else if (isValidInt) hash(toInt)
        else if (isValidLong) hash(toLong)
        else if (isValidFloat) hash(toFloat)
        else if (isValidDouble) hash(toDouble)
        else toDecimalString.hashCode
      mash(mix(seed[NumberForm], h))
    }

    override def toString: String = {
      val s = UString.Builder
      s.append("NumberForm")
      s.append('(')
      s.append(toDecimalString)
      s.append(')')
      s.state.toString
    }
  }

  protected trait BaseInt extends BaseNumber { this: NumberForm =>
    override def isNaN: Boolean          = false
    override def isInfinite: Boolean     = false
    override def isValidByte: Boolean    = toByte == toInt
    override def isValidShort: Boolean   = toShort == toInt
    override def isValidInt: Boolean     = true
    override def isValidLong: Boolean    = true
    override def isValidFloat: Boolean   = true
    override def isValidDouble: Boolean  = true
    override def toByte: Byte            = toInt.toByte
    override def toShort: Short          = toInt.toShort
    override def toLong: Long            = toInt.toLong
    override def toFloat: Float          = toInt.toFloat
    override def toDouble: Double        = toInt.toDouble
    override def toDecimalString: String = java.lang.Integer.toString(toInt)
  }

  protected trait BaseLong extends BaseNumber { this: NumberForm =>
    override def isNaN: Boolean          = false
    override def isInfinite: Boolean     = false
    override def isValidByte: Boolean    = toByte == toLong
    override def isValidShort: Boolean   = toShort == toLong
    override def isValidInt: Boolean     = toInt == toLong
    override def isValidLong: Boolean    = true
    override def isValidFloat: Boolean   = true
    override def isValidDouble: Boolean  = true
    override def toByte: Byte            = toLong.toByte
    override def toShort: Short          = toLong.toShort
    override def toInt: Int              = toLong.toInt
    override def toFloat: Float          = toLong.toFloat
    override def toDouble: Double        = toLong.toDouble
    override def toDecimalString: String = java.lang.Long.toString(toLong)
  }

  protected trait BaseFloat extends BaseNumber { this: NumberForm =>
    override def isNaN: Boolean          = java.lang.Float.isNaN(toFloat)
    override def isInfinite: Boolean     = java.lang.Float.isInfinite(toFloat)
    override def isValidByte: Boolean    = toByte == toFloat
    override def isValidShort: Boolean   = toShort == toFloat
    override def isValidInt: Boolean     = toInt == toFloat
    override def isValidLong: Boolean    = toLong == toFloat
    override def isValidFloat: Boolean   = true
    override def isValidDouble: Boolean  = true
    override def toByte: Byte            = toFloat.toByte
    override def toShort: Short          = toFloat.toShort
    override def toInt: Int              = toFloat.toInt
    override def toLong: Long            = toFloat.toLong
    override def toDouble: Double        = toFloat.toDouble
    override def toDecimalString: String = java.lang.Float.toString(toFloat)
  }

  protected trait BaseDouble extends BaseNumber { this: NumberForm =>
    override def isNaN: Boolean          = java.lang.Double.isNaN(toDouble)
    override def isInfinite: Boolean     = java.lang.Double.isInfinite(toDouble)
    override def isValidByte: Boolean    = toByte == toDouble
    override def isValidShort: Boolean   = toShort == toDouble
    override def isValidInt: Boolean     = toInt == toDouble
    override def isValidLong: Boolean    = toLong == toDouble
    override def isValidFloat: Boolean   = toFloat == toDouble
    override def isValidDouble: Boolean  = true
    override def toByte: Byte            = toDouble.toByte
    override def toShort: Short          = toDouble.toShort
    override def toInt: Int              = toDouble.toInt
    override def toLong: Long            = toDouble.toLong
    override def toFloat: Float          = toDouble.toFloat
    override def toDecimalString: String = java.lang.Double.toString(toDouble)
  }

  trait BaseNumberFactory {
    def apply(value: Int): NumberForm
    def apply(value: Long): NumberForm
    def apply(value: Float): NumberForm
    def apply(value: Double): NumberForm
    def apply(value: String): NumberForm =
      try apply(java.lang.Integer.parseInt(value))
      catch {
        case _: NumberFormatException =>
          try apply(java.lang.Long.parseLong(value))
          catch {
            case _: NumberFormatException =>
              apply(java.lang.Double.parseDouble(value))
          }
      }
    override def toString: String = "NumberForm"
  }


  trait BaseDate extends Equals with BaseValue { this: DateForm =>
    override def isDateForm: Boolean  = true
    override def asDateForm: DateForm = this

    def millis: Long

    override def canEqual(other: Any): Boolean = other.isInstanceOf[BaseDate]

    override def equals(other: Any): Boolean = eq(other.asInstanceOf[AnyRef]) || (other match {
      case that: BaseDate => that.canEqual(this) && millis == that.millis
      case _ => false
    })

    override def hashCode: Int = {
      import basis.util.MurmurHash3._
      mash(mix(seed[DateForm], hash(millis)))
    }

    override def toString: String = {
      val s = UString.Builder
      s.append("DateForm")
      s.append('(')
      s.append(java.lang.Long.toString(millis))
      s.append('L')
      s.append(')')
      s.state.toString()
    }
  }

  trait BaseDateFactory {
    def apply(millis: Long): DateForm
    def now: DateForm             = apply(System.currentTimeMillis)
    override def toString: String = "DateForm"
  }


  trait BaseBool extends Equals with BaseValue { this: BoolForm =>
    override def isBoolForm: Boolean  = true
    override def asBoolForm: BoolForm = this

    def toBoolean: Boolean

    override def canEqual(other: Any): Boolean = other.isInstanceOf[BaseBool]

    override def equals(other: Any): Boolean = eq(other.asInstanceOf[AnyRef]) || (other match {
      case that: BaseBool => that.canEqual(this) && toBoolean == that.toBoolean
      case _ => false
    })

    override def hashCode: Int = {
      import basis.util.MurmurHash3._
      mash(mix(seed[BoolForm], hash(toBoolean)))
    }

    override def toString: String = if (toBoolean) "TrueForm" else "FalseForm"
  }

  trait BaseBoolFactory {
    def apply(value: Boolean): BoolForm = if (value) TrueForm else FalseForm
    override def toString: String       = "BoolForm"
  }


  trait BaseNull extends BaseValue { this: NullForm =>
    override def isNullForm: Boolean  = true
    override def asNullForm: NullForm = this
    override def toString: String     = "NullForm"
  }


  trait BaseNo extends BaseValue { this: NoForm =>
    override def isDefined: Boolean = false
    override def toString: String   = "NoForm"
  }


  private final class StringToForm extends AbstractFunction1[String, TextForm] {
    override def apply(value: String): TextForm = TextForm(value)
    override def toString: String = variant.toString +"."+"StringToForm"
  }

  private final class IntToForm extends AbstractFunction1[Int, NumberForm] {
    override def apply(value: Int): NumberForm = NumberForm(value)
    override def toString: String = variant.toString +"."+"IntToForm"
  }

  private final class LongToForm extends AbstractFunction1[Long, NumberForm] {
    override def apply(value: Long): NumberForm = NumberForm(value)
    override def toString: String = variant.toString +"."+"LongToForm"
  }

  private final class FloatToForm extends AbstractFunction1[Float, NumberForm] {
    override def apply(value: Float): NumberForm = NumberForm(value)
    override def toString: String = variant.toString +"."+"FloatToForm"
  }

  private final class DoubleToForm extends AbstractFunction1[Double, NumberForm] {
    override def apply(value: Double): NumberForm = NumberForm(value)
    override def toString: String = variant.toString +"."+"DoubleToForm"
  }

  private final class BooleanToForm extends AbstractFunction1[Boolean, BoolForm] {
    override def apply(value: Boolean): BoolForm = BoolForm(value)
    override def toString: String = variant.toString +"."+"BoolToForm"
  }
}
