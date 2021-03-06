//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis._
import basis.collections._
import basis.text._

private[form] final class JsonVariantFactory[V <: JsonVariant](val variant: V) extends JsonFactory {
  import variant._

  override type JsonValue     = AnyForm
  override type JsonObject    = ObjectForm
  override type JsonArray     = SeqForm
  override type JsonString    = TextForm
  override type JsonNumber    = NumberForm
  override type JsonBoolean   = BoolForm
  override type JsonNull      = NullForm
  override type JsonUndefined = NoForm

  override def JsonObjectValue(json: JsonObject): JsonValue = variant.JsonObjectValue(json)
  override def JsonArrayValue(json: JsonArray): JsonValue   = variant.JsonArrayValue(json)
  override def JsonStringValue(json: JsonString): JsonValue = variant.JsonStringValue(json)

  override def JsonObjectBuilder: Builder[(String, JsonValue)] with State[JsonObject] = variant.JsonObjectBuilder
  override def JsonArrayBuilder: Builder[JsonValue] with State[JsonArray]             = variant.JsonArrayBuilder
  override def JsonStringBuilder: StringBuilder with State[JsonString]                = variant.JsonStringBuilder

  override def JsonString(value: String): JsonString  = variant.JsonString(value)
  override def JsonNumber(value: String): JsonNumber  = variant.JsonNumber(value)
  override def JsonInteger(value: String): JsonNumber = variant.JsonNumber(value)
  override def JsonTrue: JsonBoolean                  = variant.JsonTrue
  override def JsonFalse: JsonBoolean                 = variant.JsonFalse
  override def JsonNull: JsonNull                     = variant.JsonNull
  override def JsonUndefined: JsonUndefined           = variant.JsonUndefined

  override def JsonNew(identifier: String, arguments: JsonArray): JsonValue = variant.JsonNew(identifier, arguments)
}
