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
  with UndefinedForm {

  implicit def ObjectFormBuilder(): Builder[(String, AnyForm)] with From[ObjectForm] with State[ObjectForm] = ObjectForm.Builder()
  implicit def SeqFormBuilder(): Builder[AnyForm] with From[SeqForm] with State[SeqForm]                    = SeqForm.Builder()
  implicit def SetFormBuilder(): Builder[SetForm] with From[SetForm] with State[SetForm]                    = SetForm.Builder()
  implicit def StringFormBuilder(): StringBuilder with From[StringForm] with State[StringForm]              = StringForm.Builder()

  implicit def StringToForm(value: String): StringForm    = StringForm(value)
  implicit def IntToForm(value: Int): NumberForm          = NumberForm(value)
  implicit def LongToForm(value: Long): NumberForm        = NumberForm(value)
  implicit def FloatToForm(value: Float): NumberForm      = NumberForm(value)
  implicit def DoubleToForm(value: Double): NumberForm    = NumberForm(value)
  implicit def BooleanToForm(value: Boolean): BooleanForm = if (value) TrueForm else FalseForm
}
