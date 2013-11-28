//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package json

import basis.text._

trait JsonValue extends AnyForm { variant: JsonVariant =>
  override type AnyForm <: JsonValue

  override val AnyForm: JsonValueFactory

  trait JsonValue extends BaseValue { this: AnyForm =>
    def writeJson(builder: StringBuilder): Unit

    def toJson: String
  }

  trait JsonValueFactory extends BaseValueFactory {
    def parseJson(json: String): AnyForm = {
      val factory = new JsonVariantFactory[variant.type](variant)
      val parser = new JsonStringParser(json)
      parser.skipWhitespace()
      val value = parser.parseValue(factory)
      parser.skipWhitespace()
      parser.parseEOF()
      value
    }
  }
}
