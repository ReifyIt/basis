//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package bson

import basis.collections._
import basis.memory._
import basis.text._

trait BsonVariant
  extends Variant
  with BsonValue
  with BsonObject
  with BsonSeq
  with BsonSet
  with BsonBinary
  with BsonString
  with BsonNumber
  with BsonDate
  with BsonBoolean
  with BsonNull
  with BsonUndefined
  with BsonReader {

  def BsonDouble(value: Double): AnyForm = NumberForm(value)

  def BsonString(value: String): AnyForm = StringForm(value)

  def BsonStringValue(form: StringForm): AnyForm = form

  def BsonStringBuilder(): StringBuilder with State[StringForm] = StringForm.Builder()

  def BsonObjectValue(form: ObjectForm): AnyForm = form

  def BsonObjectBuilder(): Builder[(String, AnyForm)] with State[ObjectForm] = ObjectForm.Builder()

  def BsonArrayValue(form: SeqForm): AnyForm = form

  def BsonArrayBuilder(): Builder[AnyForm] with State[SeqForm] = SeqForm.Builder()

  def BsonBinary(subtype: Byte, data: Array[Byte]): AnyForm = BinaryForm(data)

  def BsonUndefined: AnyForm = UndefinedForm

  def BsonObjectId(id: Array[Byte]): AnyForm = UndefinedForm

  def BsonBoolean(value: Boolean): AnyForm = BooleanForm(value)

  def BsonDateTime(millis: Long): AnyForm = DateForm(millis)

  def BsonNull: AnyForm = NullForm

  def BsonRegex(pattern: String, options: String): AnyForm = UndefinedForm

  def BsonDBPointer(name: String, id: Array[Byte]): AnyForm = UndefinedForm

  def BsonJSCode(js: String): AnyForm = UndefinedForm

  def BsonSymbol(symbol: String): AnyForm = UndefinedForm

  def BsonJSScope(js: String, scope: ObjectForm): AnyForm = UndefinedForm

  def BsonInt32(value: Int): AnyForm = NumberForm(value)

  def BsonTimeStamp(value: Long): AnyForm = UndefinedForm

  def BsonInt64(value: Long): AnyForm = NumberForm(value)

  def BsonMinKey: AnyForm = UndefinedForm

  def BsonMaxKey: AnyForm = UndefinedForm

  private[bson] implicit def BsonReader(reader: Reader): BsonReader = new BsonReader(reader)

  private[bson] implicit def BsonWriter(writer: Writer): BsonWriter = new BsonWriter(writer)
}
