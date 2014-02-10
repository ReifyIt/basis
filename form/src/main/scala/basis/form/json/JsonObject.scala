//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package json

import basis.text._

trait JsonObject extends ObjectForm { variant: JsonVariant =>
  override type ObjectForm <: JsonObject with AnyForm

  override val ObjectForm: JsonObjectFactory

  trait JsonObject extends JsonValue with BaseObject { this: ObjectForm =>
    override def writeJson(builder: StringBuilder): Unit = {
      builder.append('{')
      val fields = iterator
      var field = null: (String, AnyForm)
      while (!fields.isEmpty && { field = fields.head; !field._2.isDefined })
        fields.step() // filter leading undefined fields
      if (!fields.isEmpty) {
        StringForm(field._1).writeJson(builder)
        builder.append(':')
        field._2.writeJson(builder)
        fields.step()
        while (!fields.isEmpty) {
          field = fields.head
          if (field._2.isDefined) { // filter undefined fields
            builder.append(',')
            StringForm(field._1).writeJson(builder)
            builder.append(':')
            field._2.writeJson(builder)
          }
          fields.step()
        }
      }
      builder.append('}')
    }

    override def toJson: String = {
      val builder = UString.Builder()
      writeJson(builder)
      builder.state.toString
    }
  }

  trait JsonObjectFactory extends BaseObjectFactory {
    def parseJson(json: String): ObjectForm = {
      val factory = new JsonVariantFactory[variant.type](variant)
      val parser = new JsonStringParser(json)
      parser.skipWhitespace()
      val value = parser.parseObject(factory)(Builder())
      parser.skipWhitespace()
      parser.parseEOF()
      value
    }
  }
}
