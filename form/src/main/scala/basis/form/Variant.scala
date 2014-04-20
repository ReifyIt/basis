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

/** A loosely structured abstract data model.
  *
  * @contentDiagram hideNodes "basis.form.Variant.BaseValue" "basis.form.Variant.BaseObject" "basis.form.Variant.BaseSeq" "basis.form.Variant.BaseSet" "basis.form.Variant.BaseBinary" "basis.form.Variant.BaseString" "basis.form.Variant.BaseNumber" "basis.form.Variant.BaseDate" "basis.form.Variant.BaseBoolean" "basis.form.Variant.BaseNull" "basis.form.Variant.BaseUndefined"
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

  /** A binary data variant forms.
    * @template */
  type BinaryForm <: BaseBinary with AnyForm

  /** A Unicode® character sequence variant form.
    * @template */
  type StringForm <: BaseString with AnyForm

  /** A numeric variant form.
    * @template */
  type NumberForm <: BaseNumber with AnyForm

  /** A date variant form.
    * @template */
  type DateForm <: BaseDate with AnyForm

  /** A boolean variant form.
    * @template */
  type BooleanForm <: BaseBoolean with AnyForm

  /** A null variant form.
    * @template */
  type NullForm <: BaseNull with AnyForm

  /** An undefined variant form.
    * @template */
  type UndefinedForm <: BaseUndefined with AnyForm

  val AnyForm: BaseValueFactory
  val ObjectForm: BaseObjectFactory
  val SeqForm: BaseSeqFactory
  val SetForm: BaseSetFactory
  val BinaryForm: BaseBinaryFactory
  val StringForm: BaseStringFactory
  val NumberForm: BaseNumberFactory
  val DateForm: BaseDateFactory
  val BooleanForm: BaseBooleanFactory

  def TrueForm: BooleanForm
  def FalseForm: BooleanForm
  def UndefinedForm: UndefinedForm
  def NullForm: NullForm

  implicit def ObjectFormBuilder: Builder[(String, AnyForm)] with From[ObjectForm] with State[ObjectForm] = ObjectForm.Builder
  implicit def SeqFormBuilder: Builder[AnyForm] with From[SeqForm] with State[SeqForm]                    = SeqForm.Builder
  implicit def SetFormBuilder: Builder[SetForm] with From[SetForm] with State[SetForm]                    = SetForm.Builder
  implicit def StringFormBuilder: StringBuilder with From[StringForm] with State[StringForm]              = StringForm.Builder

  implicit def StringToForm(value: String): StringForm    = StringForm(value)
  implicit def IntToForm(value: Int): NumberForm          = NumberForm(value)
  implicit def LongToForm(value: Long): NumberForm        = NumberForm(value)
  implicit def FloatToForm(value: Float): NumberForm      = NumberForm(value)
  implicit def DoubleToForm(value: Double): NumberForm    = NumberForm(value)
  implicit def BooleanToForm(value: Boolean): BooleanForm = if (value) TrueForm else FalseForm

  implicit def AnyFormTag: ClassTag[AnyForm]
  implicit def ObjectFormTag: ClassTag[ObjectForm]
  implicit def SeqFormTag: ClassTag[SeqForm]
  implicit def SetFormTag: ClassTag[SetForm]
  implicit def BinaryFormTag: ClassTag[BinaryForm]
  implicit def StringFormTag: ClassTag[StringForm]
  implicit def NumberFormTag: ClassTag[NumberForm]
  implicit def DateFormTag: ClassTag[DateForm]
  implicit def BooleanFormTag: ClassTag[BooleanForm]
  implicit def NullFormTag: ClassTag[NullForm]
  implicit def UndefinedFormTag: ClassTag[UndefinedForm]


  trait BaseValue { this: AnyForm =>
    def isDefined: Boolean = true

    def isObjectForm: Boolean    = false
    def isSeqForm: Boolean       = false
    def isSetForm: Boolean       = false
    def isBinaryForm: Boolean    = false
    def isStringForm: Boolean    = false
    def isNumberForm: Boolean    = false
    def isDateForm: Boolean      = false
    def isBooleanForm: Boolean   = false
    def isNullForm: Boolean      = false
    def isUndefinedForm: Boolean = false

    def asObjectForm: ObjectForm       = throw new MatchError("not an ObjectForm")
    def asSeqForm: SeqForm             = throw new MatchError("not a SeqForm")
    def asSetForm: SetForm             = throw new MatchError("not a SetForm")
    def asBinaryForm: BinaryForm       = throw new MatchError("not a BinaryForm")
    def asStringForm: StringForm       = throw new MatchError("not a StringForm")
    def asNumberForm: NumberForm       = throw new MatchError("not a NumberForm")
    def asDateForm: DateForm           = throw new MatchError("not a DateForm")
    def asBooleanForm: BooleanForm     = throw new MatchError("not a BooleanForm")
    def asNullForm: NullForm           = throw new MatchError("not a NullForm")
    def asUndefinedForm: UndefinedForm = throw new MatchError("not an UndefinedForm")

    def / (key: String): AnyForm = UndefinedForm
    def / (index: Int): AnyForm  = UndefinedForm

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
    override def / (key: String): AnyForm       = get(key).bindOrElse(UndefinedForm)
    protected override def stringPrefix: String = "ObjectForm"
  }

  trait BaseObjectFactory extends special.MapSource[ObjectForm, String, AnyForm] {
    override def toString: String = "ObjectForm"
  }


  trait BaseSeq extends Equals with Immutable with Family[SeqForm] with IndexedSeq[AnyForm] with BaseValue { this: SeqForm =>
    override def isSeqForm: Boolean             = true
    override def asSeqForm: SeqForm             = this
    override def / (index: Int): AnyForm        = if (0 <= index && index < length) this(index) else UndefinedForm
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


  trait BaseBinary extends Equals with Loader with BaseValue { this: BinaryForm =>
    override def isBinaryForm: Boolean          = true
    override def asBinaryForm: BinaryForm       = this
    protected override def stringPrefix: String = "BinaryForm"
  }

  trait BaseBinaryFactory extends ByteFactory[BinaryForm] {
    override def toString: String = "BinaryForm"
  }


  trait BaseString extends Equals with Family[StringForm] with UTF with BaseValue { this: StringForm =>
    override def isStringForm: Boolean          = true
    override def asStringForm: StringForm       = this
    protected override def stringPrefix: String = "StringForm"
  }

  trait BaseStringFactory extends StringFactory[StringForm] {
    override def toString: String = "StringForm"
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
      try java.lang.Integer.parseInt(value)
      catch {
        case _: NumberFormatException =>
          try java.lang.Long.parseLong(value)
          catch {
            case _: NumberFormatException =>
              java.lang.Double.parseDouble(value)
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


  trait BaseBoolean extends Equals with BaseValue { this: BooleanForm =>
    override def isBooleanForm: Boolean     = true
    override def asBooleanForm: BooleanForm = this

    def toBoolean: Boolean

    override def canEqual(other: Any): Boolean = other.isInstanceOf[BaseBoolean]

    override def equals(other: Any): Boolean = eq(other.asInstanceOf[AnyRef]) || (other match {
      case that: BaseBoolean => that.canEqual(this) && toBoolean == that.toBoolean
      case _ => false
    })

    override def hashCode: Int = {
      import basis.util.MurmurHash3._
      mash(mix(seed[BooleanForm], hash(toBoolean)))
    }

    override def toString: String = if (toBoolean) "TrueForm" else "FalseForm"
  }

  trait BaseBooleanFactory {
    def apply(value: Boolean): BooleanForm = if (value) TrueForm else FalseForm
    override def toString: String          = "BooleanForm"
  }


  trait BaseNull extends BaseValue { this: NullForm =>
    override def isNullForm: Boolean  = true
    override def asNullForm: NullForm = this
    override def toString: String     = "NullForm"
  }


  trait BaseUndefined extends BaseValue { this: UndefinedForm =>
    override def isDefined: Boolean             = false
    override def isUndefinedForm: Boolean       = true
    override def asUndefinedForm: UndefinedForm = this
    override def toString: String               = "UndefinedForm"
  }
}
