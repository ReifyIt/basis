//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package json

import basis.collections._
import basis.text._

trait JsonVariant
  extends Variant
  with JsonValue
  with JsonObject
  with JsonSeq
  with JsonSet
  with JsonBinary
  with JsonString
  with JsonNumber
  with JsonDate
  with JsonBoolean
  with JsonNull
  with JsonUndefined { variant =>

  implicit def JsonStringContext(stringContext: StringContext): JsonStringContext[variant.type] =
    macro json.JsonStringContext.JsonStringContext[variant.type]

  def JsonObjectValue(form: ObjectForm): AnyForm = {
    if (form.size == 1) {
      val field = form.iterator.head
      val value = field._2
      field._1 match {
        case "$base64" if value.isStringForm =>
          try BinaryForm(value.asStringForm.toUString.toString)
          catch { case _: IllegalArgumentException => form }
        case "$date" if value.isNumberForm =>
          DateForm(value.asNumberForm.toLong)
        case _ => form
      }
    }
    else form
  }

  def JsonArrayValue(form: SeqForm): AnyForm = form

  def JsonStringValue(form: StringForm): AnyForm = form

  def JsonObjectBuilder(): Builder[(String, AnyForm)] with State[ObjectForm] = ObjectForm.Builder()

  def JsonArrayBuilder(): Builder[AnyForm] with State[SeqForm] = SeqForm.Builder()

  def JsonString(value: String): StringForm = StringForm(value)

  def JsonStringBuilder(): StringBuilder with State[StringForm] = StringForm.Builder()

  def JsonNumber(value: Int): NumberForm = NumberForm(value)

  def JsonNumber(value: Long): NumberForm = NumberForm(value)

  def JsonNumber(value: Float): NumberForm = NumberForm(value)

  def JsonNumber(value: Double): NumberForm = NumberForm(value)

  def JsonNumber(value: String): NumberForm = NumberForm(value)

  def JsonTrue: BooleanForm = TrueForm

  def JsonFalse: BooleanForm = FalseForm

  def JsonNull: NullForm = NullForm

  def JsonUndefined: UndefinedForm = UndefinedForm

  def JsonNew(identifier: String, arguments: SeqForm): AnyForm = UndefinedForm
}
