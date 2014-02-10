//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.collections._
import basis.text._

trait Variant
  extends AnyForm
  with ObjectForm
  with SeqForm
  with SetForm
  with BinaryForm
  with StringForm
  with NumberForm
  with DateForm
  with BooleanForm
  with NullForm
  with UndefinedForm { variant =>

  import scala.runtime._

  implicit def ObjectFormBuilder(): Builder[(String, AnyForm)] with From[ObjectForm] with State[ObjectForm] =
    ObjectForm.Builder()

  implicit def SeqFormBuilder(): Builder[AnyForm] with From[SeqForm] with State[SeqForm] =
    SeqForm.Builder()

  implicit def SetFormBuilder(): Builder[SetForm] with From[SetForm] with State[SetForm] =
    SetForm.Builder()

  implicit def StringFormBuilder(): StringBuilder with From[StringForm] with State[StringForm] =
    StringForm.Builder()

  implicit lazy val StringToForm: String => StringForm = new StringToForm

  implicit lazy val IntToForm: Int => NumberForm = new IntToForm

  implicit lazy val LongToForm: Long => NumberForm = new LongToForm

  implicit lazy val FloatToForm: Float => NumberForm = new FloatToForm

  implicit lazy val DoubleToForm: Double => NumberForm = new DoubleToForm

  implicit lazy val BooleanToForm: Boolean => BooleanForm = new BooleanToForm

  private final class StringToForm extends AbstractFunction1[String, StringForm] {
    override def apply(value: String): StringForm = StringForm(value)
    override def toString: String = "StringToForm"
  }

  private final class IntToForm extends AbstractFunction1[Int, NumberForm] {
    override def apply(value: Int): NumberForm = NumberForm(value)
    override def toString: String = "IntToForm"
  }

  private final class LongToForm extends AbstractFunction1[Long, NumberForm] {
    override def apply(value: Long): NumberForm = NumberForm(value)
    override def toString: String = "LongToForm"
  }

  private final class FloatToForm extends AbstractFunction1[Float, NumberForm] {
    override def apply(value: Float): NumberForm = NumberForm(value)
    override def toString: String = "FloatToForm"
  }

  private final class DoubleToForm extends AbstractFunction1[Double, NumberForm] {
    override def apply(value: Double): NumberForm = NumberForm(value)
    override def toString: String = "DoubleToForm"
  }

  private final class BooleanToForm extends AbstractFunction1[Boolean, BooleanForm] {
    override def apply(value: Boolean): BooleanForm = if (value) TrueForm else FalseForm
    override def toString: String = "BooleanToForm"
  }
}
