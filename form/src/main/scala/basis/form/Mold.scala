//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis._
import basis.collections._
import basis.text._
import basis.util._
import scala.reflect._

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
  implicit lazy val Unit: Mold[Unit]       = new UnitMold

  implicit def Tuple2[T1, T2](implicit T1: Mold[T1], T2: Mold[T2]): Mold[(T1, T2)] = new Tuple2Mold[T1, T2]()(T1, T2)
  implicit def Tuple3[T1, T2, T3](implicit T1: Mold[T1], T2: Mold[T2], T3: Mold[T3]): Mold[(T1, T2, T3)] = new Tuple3Mold[T1, T2, T3]()(T1, T2, T3)
  implicit def Tuple4[T1, T2, T3, T4](implicit T1: Mold[T1], T2: Mold[T2], T3: Mold[T3], T4: Mold[T4]): Mold[(T1, T2, T3, T4)] = new Tuple4Mold[T1, T2, T3, T4]()(T1, T2, T3, T4)

  implicit def Set[CC[X] <: Set[X], A](implicit CC: generic.SetFactory[CC], A: Mold[A]): Mold[CC[A]] = new SetMold[CC, A]()(CC, A)

  private final class ByteMold(override val identity: Byte) extends Mold[Byte] {
    override def form(variant: Variant)(value: Byte): variant.AnyForm = variant.NumberForm(value.toInt)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Byte] = {
      if (form.isNumberForm) Bind(form.asNumberForm.toByte)
      else if (form.isTextForm)
        try Bind(java.lang.Byte.parseByte(form.asTextForm.toUString.toString))
        catch { case _: NumberFormatException => Trap }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isNumberForm) form
      else if (form.isTextForm)
        try variant.NumberForm(java.lang.Byte.parseByte(form.asTextForm.toUString.toString).toInt)
        catch { case _: NumberFormatException => form }
      else form
    }

    override def toString: String = (basis.text.String.Builder~"Mold"~'.'~"Byte"~'('~>identity~')').state
  }

  private final class ShortMold(override val identity: Short) extends Mold[Short] {
    override def form(variant: Variant)(value: Short): variant.AnyForm = variant.NumberForm(value.toInt)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Short] = {
      if (form.isNumberForm) Bind(form.asNumberForm.toShort)
      else if (form.isTextForm)
        try Bind(java.lang.Short.parseShort(form.asTextForm.toUString.toString))
        catch { case _: NumberFormatException => Trap }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isNumberForm) form
      else if (form.isTextForm)
        try variant.NumberForm(java.lang.Short.parseShort(form.asTextForm.toUString.toString).toInt)
        catch { case _: NumberFormatException => form }
      else form
    }

    override def toString: String = (basis.text.String.Builder~"Mold"~'.'~"Short"~'('~>identity~')').state
  }

  private final class IntMold(override val identity: Int) extends Mold[Int] {
    override def form(variant: Variant)(value: Int): variant.AnyForm = variant.NumberForm(value)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Int] = {
      if (form.isNumberForm) Bind(form.asNumberForm.toInt)
      else if (form.isTextForm)
        try Bind(java.lang.Integer.parseInt(form.asTextForm.toUString.toString))
        catch { case _: NumberFormatException => Trap }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isNumberForm) form
      else if (form.isTextForm)
        try variant.NumberForm(java.lang.Integer.parseInt(form.asTextForm.toUString.toString))
        catch { case _: NumberFormatException => form }
      else form
    }

    override def toString: String = (basis.text.String.Builder~"Mold"~'.'~"Int"~'('~>identity~')').state
  }

  private final class LongMold(override val identity: Long) extends Mold[Long] {
    override def form(variant: Variant)(value: Long): variant.AnyForm = variant.NumberForm(value)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Long] = {
      if (form.isNumberForm) Bind(form.asNumberForm.toLong)
      else if (form.isTextForm)
        try Bind(java.lang.Long.parseLong(form.asTextForm.toUString.toString))
        catch { case _: NumberFormatException => Trap }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isNumberForm) form
      else if (form.isTextForm)
        try variant.NumberForm(java.lang.Long.parseLong(form.asTextForm.toUString.toString))
        catch { case _: NumberFormatException => form }
      else form
    }

    override def toString: String = (basis.text.String.Builder~"Mold"~'.'~"Long"~'('~>identity~')').state
  }

  private final class FloatMold(override val identity: Float) extends Mold[Float] {
    override def form(variant: Variant)(value: Float): variant.AnyForm = variant.NumberForm(value)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Float] = {
      if (form.isNumberForm) Bind(form.asNumberForm.toFloat)
      else if (form.isTextForm)
        try Bind(java.lang.Float.parseFloat(form.asTextForm.toUString.toString))
        catch { case _: NumberFormatException => Trap }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isNumberForm) form
      else if (form.isTextForm)
        try variant.NumberForm(java.lang.Float.parseFloat(form.asTextForm.toUString.toString))
        catch { case _: NumberFormatException => form }
      else form
    }

    override def toString: String = (basis.text.String.Builder~"Mold"~'.'~"Float"~'('~>identity~')').state
  }

  private final class DoubleMold(override val identity: Double) extends Mold[Double] {
    override def form(variant: Variant)(value: Double): variant.AnyForm = variant.NumberForm(value)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Double] = {
      if (form.isNumberForm) Bind(form.asNumberForm.toDouble)
      else if (form.isTextForm)
        try Bind(java.lang.Double.parseDouble(form.asTextForm.toUString.toString))
        catch { case _: NumberFormatException => Trap }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isNumberForm) form
      else if (form.isTextForm)
        try variant.NumberForm(java.lang.Double.parseDouble(form.asTextForm.toUString.toString))
        catch { case _: NumberFormatException => form }
      else form
    }

    override def toString: String = (basis.text.String.Builder~"Mold"~'.'~"Double"~'('~>identity~')').state
  }

  private final class BooleanMold(override val identity: Boolean) extends Mold[Boolean] {
    override def form(variant: Variant)(value: Boolean): variant.AnyForm = variant.BoolForm(value)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Boolean] = {
      if (form.isBoolForm) Bind(form.asBoolForm.toBoolean)
      else if (form.isTextForm) form.asTextForm.toUString.toString match {
        case "true" => True
        case "false" => False
        case _ => Trap
      }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isBoolForm) form
      else if (form.isTextForm) form.asTextForm.toUString.toString match {
        case "true" => variant.TrueForm
        case "false" => variant.FalseForm
        case _ => form
      }
      else form
    }

    override def toString: String = (basis.text.String.Builder~"Mold"~'.'~"Boolean"~'('~>identity~')').state
  }

  private final class StringMold(override val identity: String) extends Mold[String] {
    override def form(variant: Variant)(value: String): variant.AnyForm = variant.TextForm(value)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[String] = {
      if (form.isTextForm) Bind(form.asTextForm.toUString.toString)
      else if (form.isNumberForm) Bind(form.asNumberForm.toDecimalString)
      else if (form.isBoolForm) Bind(if (form.asBoolForm.toBoolean) "true" else "false")
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isTextForm) form
      else if (form.isNumberForm) variant.TextForm(form.asNumberForm.toDecimalString)
      else if (form.isBoolForm) variant.TextForm(if (form.asBoolForm.toBoolean) "true" else "false")
      else form
    }

    override def toString: String = {
      val s = basis.text.String.Builder~"Mold"~'.'~"String"~'('
      new UString(identity).show(s)
      (s~')').state
    }
  }

  private final class DateMold(override val identity: Date) extends Mold[Date] {
    override def form(variant: Variant)(date: Date): variant.AnyForm = variant.DateForm(date.getTime)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Date] = {
      if (form.isDateForm) Bind(new Date(form.asDateForm.millis))
      else if (form.isTextForm) variant.DateForm.parse(form.asTextForm.toUString.toString).map(form => new Date(form.millis))
      else if (form.isNumberForm) Bind(new Date(form.asNumberForm.toLong))
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isDateForm) form
      else if (form.isTextForm) variant.DateForm.parse(form.asTextForm.toUString.toString).bindOrElse(form)
      else if (form.isNumberForm) variant.DateForm(form.asNumberForm.toLong)
      else form
    }

    override def toString: String = (basis.text.String.Builder~"Mold"~'.'~"Date"~'('~>identity~')').state
  }

  private final class UnitMold extends Mold[Unit] {
    override def identity: Unit = ()

    override def form(variant: Variant)(unit: Unit): variant.AnyForm = variant.NoForm

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Unit] = Trap

    override def toString: String = (basis.text.String.Builder~"Mold"~'.'~"Unit").state
  }

  private final class Tuple2Mold[T1, T2](implicit T1: Mold[T1], T2: Mold[T2]) extends Mold[(T1, T2)] {
    override def identity: (T1, T2) = (T1.identity, T2.identity)

    override def form(variant: Variant)(tuple: (T1, T2)): variant.AnyForm = {
      val _1 = T1.form(variant)(tuple._1)
      val _2 = T2.form(variant)(tuple._2)
      variant.SeqForm(_1, _2)
    }

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[(T1, T2)] = {
      if (form.isSeqForm && form.asSeqForm.length >= 2) {
        val xs = form.asSeqForm
        val _1 = T1.cast(variant)(xs(0))
        val _2 = T2.cast(variant)(xs(1))
        if (_1.isDefined && _2.isDefined) Bind((_1.bind, _2.bind))
        else Trap
      }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isSeqForm && form.asSeqForm.length == 2) {
        val xs = form.asSeqForm
        val _1 = T1.norm(variant)(xs(0))
        val _2 = T2.norm(variant)(xs(1))
        variant.SeqForm(_1, _2)
      }
      else form
    }

    override def toString: String = (basis.text.String.Builder~"Mold"~'.'~"Tuple2"~'('~>T1~", "~>T2~')').state
  }

  private final class Tuple3Mold[T1, T2, T3](implicit T1: Mold[T1], T2: Mold[T2], T3: Mold[T3]) extends Mold[(T1, T2, T3)] {
    override def identity: (T1, T2, T3) = (T1.identity, T2.identity, T3.identity)

    override def form(variant: Variant)(tuple: (T1, T2, T3)): variant.AnyForm = {
      val _1 = T1.form(variant)(tuple._1)
      val _2 = T2.form(variant)(tuple._2)
      val _3 = T3.form(variant)(tuple._3)
      variant.SeqForm(_1, _2, _3)
    }

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[(T1, T2, T3)] = {
      if (form.isSeqForm && form.asSeqForm.length >= 3) {
        val xs = form.asSeqForm
        val _1 = T1.cast(variant)(xs(0))
        val _2 = T2.cast(variant)(xs(1))
        val _3 = T3.cast(variant)(xs(2))
        if (_1.isDefined && _2.isDefined && _3.isDefined) Bind((_1.bind, _2.bind, _3.bind))
        else Trap
      }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isSeqForm && form.asSeqForm.length == 3) {
        val xs = form.asSeqForm
        val _1 = T1.norm(variant)(xs(0))
        val _2 = T2.norm(variant)(xs(1))
        val _3 = T3.norm(variant)(xs(2))
        variant.SeqForm(_1, _2, _3)
      }
      else form
    }

    override def toString: String = (basis.text.String.Builder~"Mold"~'.'~"Tuple3"~'('~>T1~", "~>T2~", "~>T3~')').state
  }

  private final class Tuple4Mold[T1, T2, T3, T4](implicit T1: Mold[T1], T2: Mold[T2], T3: Mold[T3], T4: Mold[T4]) extends Mold[(T1, T2, T3, T4)] {
    override def identity: (T1, T2, T3, T4) = (T1.identity, T2.identity, T3.identity, T4.identity)

    override def form(variant: Variant)(tuple: (T1, T2, T3, T4)): variant.AnyForm = {
      val _1 = T1.form(variant)(tuple._1)
      val _2 = T2.form(variant)(tuple._2)
      val _3 = T3.form(variant)(tuple._3)
      val _4 = T4.form(variant)(tuple._4)
      variant.SeqForm(_1, _2, _3, _4)
    }

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[(T1, T2, T3, T4)] = {
      if (form.isSeqForm && form.asSeqForm.length >= 4) {
        val xs = form.asSeqForm
        val _1 = T1.cast(variant)(xs(0))
        val _2 = T2.cast(variant)(xs(1))
        val _3 = T3.cast(variant)(xs(2))
        val _4 = T4.cast(variant)(xs(3))
        if (_1.isDefined && _2.isDefined && _3.isDefined && _4.isDefined) Bind((_1.bind, _2.bind, _3.bind, _4.bind))
        else Trap
      }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isSeqForm && form.asSeqForm.length == 4) {
        val xs = form.asSeqForm
        val _1 = T1.norm(variant)(xs(0))
        val _2 = T2.norm(variant)(xs(1))
        val _3 = T3.norm(variant)(xs(2))
        val _4 = T4.norm(variant)(xs(3))
        variant.SeqForm(_1, _2, _3, _4)
      }
      else form
    }

    override def toString: String = (basis.text.String.Builder~"Mold"~'.'~"Tuple4"~'('~>T1~", "~>T2~", "~>T3~", "~>T4~')').state
  }

  private[form] final class ArrayMold[A](implicit A: Mold[A], ATag: ClassTag[A]) extends Mold[Array[A]] {
    override def identity: Array[A] = ATag.newArray(0)

    override def form(variant: Variant)(array: Array[A]): variant.AnyForm = {
      var i = 0
      val n = array.length
      val b = variant.SeqFormBuilder.expect(n)
      while (i < n) {
        b.append(A.form(variant)(array(i)))
        i += 1
      }
      b.state
    }

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[Array[A]] = {
      var iter = null: Iterator[variant.AnyForm]
      var n = -1
      if (form.isSeqForm) {
        iter = form.asSeqForm.iterator
        n = form.asSeqForm.length
      }
      else if (form.isSetForm) {
        iter = form.asSetForm.iterator
        n = form.asSetForm.size
      }
      if (n >= 0) {
        var i = 0
        val xs = ATag.newArray(n)
        while (!iter.isEmpty) {
          A.cast(variant)(iter.head).foreach { value =>
            xs(i) = value
            i += 1
          }
          iter.step()
        }
        if (i == n) Bind(xs)
        else {
          val ys = ATag.newArray(i)
          java.lang.System.arraycopy(xs, 0, ys, 0, i)
          Bind(ys)
        }
      }
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isSeqForm) form.asSeqForm.map(A.norm(variant)(_))(variant.SeqForm.Builder)
      else if (form.isSetForm) form.asSetForm.map(A.norm(variant)(_))(variant.SeqForm.Builder)
      else variant.SeqForm.empty
    }

    override def toString: String = (basis.text.String.Builder~"Mold"~'.'~"Array"~'('~>A~", "~>ATag~')').state
  }

  private[form] final class ContainerMold[CC[X] <: Container[X], A](implicit CC: generic.CollectionFactory[CC], A: Mold[A]) extends Mold[CC[A]] {
    override def identity: CC[A] = CC.empty[A]

    override def form(variant: Variant)(elems: CC[A]): variant.AnyForm =
      elems.map(A.form(variant)(_))(variant.SeqForm.Builder)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[CC[A]] = {
      if (form.isSeqForm) Bind(form.asSeqForm.flatMap(A.cast(variant)(_))(CC.Builder[A]))
      else if (form.isSetForm) Bind(form.asSetForm.flatMap(A.cast(variant)(_))(CC.Builder[A]))
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isSeqForm) form.asSeqForm.map(A.norm(variant)(_))(variant.SeqForm.Builder)
      else if (form.isSetForm) form.asSetForm.map(A.norm(variant)(_))(variant.SeqForm.Builder)
      else variant.SeqForm.empty
    }

    override def toString: String = (basis.text.String.Builder~"Mold"~'.'~"Container"~'('~>CC~", "~>A~')').state
  }

  private[form] final class SetMold[CC[X] <: Set[X], A](implicit CC: generic.SetFactory[CC], A: Mold[A]) extends Mold[CC[A]] {
    override def identity: CC[A] = CC.empty[A]

    override def form(variant: Variant)(elems: CC[A]): variant.AnyForm =
      elems.map(A.form(variant)(_))(variant.SetForm.Builder)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[CC[A]] = {
      if (form.isSetForm) Bind(form.asSetForm.flatMap(A.cast(variant)(_))(CC.Builder[A]))
      else if (form.isSeqForm) Bind(form.asSeqForm.flatMap(A.cast(variant)(_))(CC.Builder[A]))
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isSetForm) form.asSetForm.map(A.norm(variant)(_))(variant.SetForm.Builder)
      else if (form.isSeqForm) form.asSeqForm.map(A.norm(variant)(_))(variant.SetForm.Builder)
      else variant.SetForm.empty
    }

    override def toString: String = (basis.text.String.Builder~"Mold"~'.'~"Set"~'('~>CC~", "~>A~')').state
  }

  private[form] final class MapMold[CC[X, Y] <: Map[X, Y], T](implicit CC: generic.MapFactory[CC], T: Mold[T]) extends Mold[CC[String, T]] {
    override def identity: CC[String, T] = CC.empty[String, T]

    override def form(variant: Variant)(fields: CC[String, T]): variant.AnyForm =
      fields.map(field => field._1 -> T.form(variant)(field._2))(variant.ObjectForm.Builder)

    override def cast(variant: Variant)(form: variant.AnyForm): Maybe[CC[String, T]] = {
      def castField(field: (String, variant.AnyForm)): Maybe[(String, T)] = T.cast(variant)(field._2).map(field._1 -> _)
      if (form.isObjectForm) Bind(form.asObjectForm.flatMap(castField)(CC.Builder[String, T]))
      else Trap
    }

    override def norm(variant: Variant)(form: variant.AnyForm): variant.AnyForm = {
      if (form.isObjectForm) form.asObjectForm.map(field => field._1 -> T.norm(variant)(field._2))(variant.ObjectForm.Builder)
      else variant.ObjectForm.empty
    }

    override def toString: String = (basis.text.String.Builder~"Mold"~'.'~"Map"~'('~>CC~", "~>T~')').state
  }

  protected[form] final val Specialized = new Specializable.Group((scala.Byte, scala.Short, scala.Int, scala.Long, scala.Float, scala.Double, scala.Boolean))
}

private[form] class CollectionMolds extends ArrayMolds {
  import Mold._
  implicit def Container[CC[X] <: Container[X], A](implicit CC: generic.CollectionFactory[CC], A: Mold[A]): Mold[CC[A]] = new ContainerMold[CC, A]()(CC, A)
  implicit def Map[CC[X, Y] <: Map[X, Y], T](implicit CC: generic.MapFactory[CC], T: Mold[T]): Mold[CC[String, T]] = new MapMold[CC, T]()(CC, T)
}

private[form] class ArrayMolds {
  import Mold._
  implicit def Array[A](implicit A: Mold[A], ATag: ClassTag[A]): Mold[Array[A]] = new ArrayMold[A]()(A, ATag)
}
