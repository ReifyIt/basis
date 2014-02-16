//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.collections._
import basis.text._
import basis.util._

trait Mold[@specialized(Mold.Specialized) T] {
  /** Returns a fallback typed value. */
  def identity: T

  /** Encodes a typed value as a variant form.
    *
    * @param  variant   the variant domain in which to encode the value.
    * @param  value     the typed value to encode.
    * @return the encoded form in the `variant` domain.
    */
  def form(variant: Variant)(value: T): variant.AnyForm

  /** Decodes a variant form as a typed value, if possible.
    *
    * @param  variant   the variant domain of the form to decode.
    * @param  form      the variant form to decode.
    * @return the decoded value, or `Trap` if the deconstruction failed.
    */
  def cast(variant: Variant)(form: variant.AnyForm): Maybe[T]

  /** Normalizes encoded values into a preferred form, if one exists. */
  def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = form
}

object Mold extends CollectionMolds {
  import java.util.Date

  def apply[T](implicit T: Mold[T]): T.type = T

  def Byte(identity: Byte): Mold[Byte]          = new ByteMold(identity)
  def Short(identity: Short): Mold[Short]       = new ShortMold(identity)
  def Int(identity: Int): Mold[Int]             = new IntMold(identity)
  def Long(identity: Long): Mold[Long]          = new LongMold(identity)
  def Float(identity: Float): Mold[Float]       = new FloatMold(identity)
  def Double(identity: Double): Mold[Double]    = new DoubleMold(identity)
  def Boolean(identity: Boolean): Mold[Boolean] = new BooleanMold(identity)
  def String(identity: String): Mold[String]    = new StringMold(identity)
  def Date(identity: Date): Mold[Date]          = new DateMold(identity)

  implicit lazy val Byte: Mold[Byte]       = new ByteMold(0)
  implicit lazy val Short: Mold[Short]     = new ShortMold(0)
  implicit lazy val Int: Mold[Int]         = new IntMold(0)
  implicit lazy val Long: Mold[Long]       = new LongMold(0L)
  implicit lazy val Float: Mold[Float]     = new FloatMold(0.0F)
  implicit lazy val Double: Mold[Double]   = new DoubleMold(0.0)
  implicit lazy val Boolean: Mold[Boolean] = new BooleanMold(false)
  implicit lazy val String: Mold[String]   = new StringMold("")
  implicit lazy val Date: Mold[Date]       = new DateMold(new Date(0L))

  implicit def Set[CC[X] <: Set[X], A](implicit CC: generic.SetFactory[CC], A: Mold[A]): Mold[CC[A]] = new SetMold[CC, A]

  private final class ByteMold(override val identity: Byte) extends Mold[Byte] {
    override def form(variant: Variant)(value: Byte): variant.AnyForm = variant.NumberForm(value.toInt)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Byte] = {
      if (form.isNumberForm) Bind(form.asNumberForm.toByte)
      else if (form.isStringForm)
        try Bind(java.lang.Byte.parseByte(form.asStringForm.toUString.toString))
        catch { case _: NumberFormatException => Trap }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isNumberForm) form
      else if (form.isStringForm)
        try variant.NumberForm(java.lang.Byte.parseByte(form.asStringForm.toUString.toString).toInt)
        catch { case _: NumberFormatException => form }
      else form
    }

    override def toString: String = "Mold"+"."+"Byte"+"("+ identity +")"
  }

  private final class ShortMold(override val identity: Short) extends Mold[Short] {
    override def form(variant: Variant)(value: Short): variant.AnyForm = variant.NumberForm(value.toInt)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Short] = {
      if (form.isNumberForm) Bind(form.asNumberForm.toShort)
      else if (form.isStringForm)
        try Bind(java.lang.Short.parseShort(form.asStringForm.toUString.toString))
        catch { case _: NumberFormatException => Trap }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isNumberForm) form
      else if (form.isStringForm)
        try variant.NumberForm(java.lang.Short.parseShort(form.asStringForm.toUString.toString).toInt)
        catch { case _: NumberFormatException => form }
      else form
    }

    override def toString: String = "Mold"+"."+"Short"+"("+ identity +")"
  }

  private final class IntMold(override val identity: Int) extends Mold[Int] {
    override def form(variant: Variant)(value: Int): variant.AnyForm = variant.NumberForm(value)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Int] = {
      if (form.isNumberForm) Bind(form.asNumberForm.toInt)
      else if (form.isStringForm)
        try Bind(java.lang.Integer.parseInt(form.asStringForm.toUString.toString))
        catch { case _: NumberFormatException => Trap }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isNumberForm) form
      else if (form.isStringForm)
        try variant.NumberForm(java.lang.Integer.parseInt(form.asStringForm.toUString.toString))
        catch { case _: NumberFormatException => form }
      else form
    }

    override def toString: String = "Mold"+"."+"Int"+"("+ identity +")"
  }

  private final class LongMold(override val identity: Long) extends Mold[Long] {
    override def form(variant: Variant)(value: Long): variant.AnyForm = variant.NumberForm(value)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Long] = {
      if (form.isNumberForm) Bind(form.asNumberForm.toLong)
      else if (form.isStringForm)
        try Bind(java.lang.Long.parseLong(form.asStringForm.toUString.toString))
        catch { case _: NumberFormatException => Trap }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isNumberForm) form
      else if (form.isStringForm)
        try variant.NumberForm(java.lang.Long.parseLong(form.asStringForm.toUString.toString))
        catch { case _: NumberFormatException => form }
      else form
    }

    override def toString: String = "Mold"+"."+"Long"+"("+ identity +"L"+")"
  }

  private final class FloatMold(override val identity: Float) extends Mold[Float] {
    override def form(variant: Variant)(value: Float): variant.AnyForm = variant.NumberForm(value)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Float] = {
      if (form.isNumberForm) Bind(form.asNumberForm.toFloat)
      else if (form.isStringForm)
        try Bind(java.lang.Float.parseFloat(form.asStringForm.toUString.toString))
        catch { case _: NumberFormatException => Trap }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isNumberForm) form
      else if (form.isStringForm)
        try variant.NumberForm(java.lang.Float.parseFloat(form.asStringForm.toUString.toString))
        catch { case _: NumberFormatException => form }
      else form
    }

    override def toString: String = "Mold"+"."+"Float"+"("+ identity +"F"+")"
  }

  private final class DoubleMold(override val identity: Double) extends Mold[Double] {
    override def form(variant: Variant)(value: Double): variant.AnyForm = variant.NumberForm(value)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Double] = {
      if (form.isNumberForm) Bind(form.asNumberForm.toDouble)
      else if (form.isStringForm)
        try Bind(java.lang.Double.parseDouble(form.asStringForm.toUString.toString))
        catch { case _: NumberFormatException => Trap }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isNumberForm) form
      else if (form.isStringForm)
        try variant.NumberForm(java.lang.Double.parseDouble(form.asStringForm.toUString.toString))
        catch { case _: NumberFormatException => form }
      else form
    }

    override def toString: String = "Mold"+"."+"Double"+"("+ identity +")"
  }

  private final class BooleanMold(override val identity: Boolean) extends Mold[Boolean] {
    override def form(variant: Variant)(value: Boolean): variant.AnyForm = variant.BooleanForm(value)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Boolean] = {
      if (form.isBooleanForm) Bind(form.asBooleanForm.toBoolean)
      else if (form.isStringForm) form.asStringForm.toUString.toString match {
        case "true" => True
        case "false" => False
        case _ => Trap
      }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isBooleanForm) form
      else if (form.isStringForm) form.asStringForm.toUString.toString match {
        case "true" => variant.TrueForm
        case "false" => variant.FalseForm
        case _ => form
      }
      else form
    }

    override def toString: String = "Mold"+"."+"Boolean"+"("+ identity +")"
  }

  private final class StringMold(override val identity: String) extends Mold[String] {
    override def form(variant: Variant)(value: String): variant.AnyForm = variant.StringForm(value)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[String] = {
      if (form.isStringForm) Bind(form.asStringForm.toUString.toString)
      else if (form.isNumberForm) Bind(form.asNumberForm.toDecimalString)
      else if (form.isBooleanForm) Bind(if (form.asBooleanForm.toBoolean) "true" else "false")
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isStringForm) form
      else if (form.isNumberForm) variant.StringForm(form.asNumberForm.toDecimalString)
      else if (form.isBooleanForm) variant.StringForm(if (form.asBooleanForm.toBoolean) "true" else "false")
      else form
    }

    override def toString: String = "Mold"+"."+"String"+"("+ new UString(identity).show +")"
  }

  private final class DateMold(override val identity: Date) extends Mold[Date] {
    override def form(variant: Variant)(date: Date): variant.AnyForm = variant.DateForm(date.getTime)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Date] = {
      if (form.isDateForm) Bind(new Date(form.asDateForm.millis))
      else if (form.isNumberForm) Bind(new Date(form.asNumberForm.toLong))
      // TODO: accept ISO 8601 dates
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isDateForm) form
      else if (form.isNumberForm) variant.DateForm(form.asNumberForm.toLong)
      // TODO: normalize ISO 8601 dates
      else form
    }

    override def toString: String = "Mold"+"."+"Date"+"("+ identity +")"
  }

  private[form] final class ContainerMold[CC[X] <: Container[X], A](implicit CC: generic.CollectionFactory[CC], A: Mold[A]) extends Mold[CC[A]] {
    override def identity: CC[A] = CC.empty[A]

    override def form(variant: Variant)(elems: CC[A]): variant.AnyForm =
      elems.map(A.form(variant)(_))(variant.SeqForm.Builder())

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[CC[A]] = {
      if (form.isSeqForm) Bind(form.asSeqForm.flatMap(A.cast(variant)(_))(CC.Builder[A]()))
      else if (form.isSetForm) Bind(form.asSetForm.flatMap(A.cast(variant)(_))(CC.Builder[A]))
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isSeqForm) form.asSeqForm.map(A.norm(variant)(_))(variant.SeqForm.Builder())
      else if (form.isSetForm) form.asSetForm.map(A.norm(variant)(_))(variant.SeqForm.Builder())
      else variant.SeqForm.empty
    }

    override def toString: String = "Mold"+"."+"Container"+"("+ CC +", "+ A +")"
  }

  private[form] final class SetMold[CC[X] <: Set[X], A](implicit CC: generic.SetFactory[CC], A: Mold[A]) extends Mold[CC[A]] {
    override def identity: CC[A] = CC.empty[A]

    override def form(variant: Variant)(elems: CC[A]): variant.AnyForm =
      elems.map(A.form(variant)(_))(variant.SetForm.Builder())

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[CC[A]] = {
      if (form.isSetForm) Bind(form.asSetForm.flatMap(A.cast(variant)(_))(CC.Builder[A]()))
      else if (form.isSeqForm) Bind(form.asSeqForm.flatMap(A.cast(variant)(_))(CC.Builder[A]()))
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isSetForm) form.asSetForm.map(A.norm(variant)(_))(variant.SetForm.Builder())
      else if (form.isSeqForm) form.asSeqForm.map(A.norm(variant)(_))(variant.SetForm.Builder())
      else variant.SetForm.empty
    }

    override def toString: String = "Mold"+"."+"Set"+"("+ CC +", "+ A +")"
  }
  /*
  private[form] final class MapMold[CC[X, Y] <: Map[X, Y], T](implicit CC: generic.MapFactory[CC], T: Mold[T]) extends Mold[CC[String, T]] {
    override def identity: CC[String, T] = CC.empty[String, T]

    override def form(variant: Variant)(fields: CC[String, T]): variant.AnyForm =
      fields.map(field => field._1 -> T.form(variant)(field._2))(variant.ObjectForm.Builder())

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[CC[String, T]] = {
      if (form.isObjectForm) Bind(form.asObjectForm.flatMap(field => T.cast(variant)(field._2).map(field._1 -> _))(CC.Builder[String, T]()))
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isObjectForm) form.asObjectForm.map(field => field._1 -> T.norm(variant)(field._2))(variant.ObjectForm.Builder())
      else variant.ObjectForm.empty
    }

    override def toString: String = "Mold"+"."+"Map"+"("+ CC +", "+ T +")"
  }
  */
  protected[form] final val Specialized = new Specializable.Group((scala.Byte, scala.Short, scala.Int, scala.Long, scala.Float, scala.Double, scala.Boolean))
}

private[form] class CollectionMolds {
  import Mold._
  implicit def Container[CC[X] <: Container[X], A](implicit CC: generic.CollectionFactory[CC], A: Mold[A]): Mold[CC[A]] = new ContainerMold[CC, A]
//implicit def Map[CC[X, Y] <: Map[X, Y], T](implicit CC: generic.MapFactory[CC], T: Mold[T]): Mold[CC[String, T]] = new MapMold[CC, T]
}
