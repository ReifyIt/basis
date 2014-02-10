//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package json

import basis.text._

trait JsonNumber extends NumberForm { variant: JsonVariant =>
  override type NumberForm <: JsonNumber with AnyForm

  trait JsonNumber extends JsonValue with BaseNumber { this: NumberForm =>
    override def writeJson(builder: StringBuilder): Unit = builder.append(toDecimalString)

    override def toJson: String = toDecimalString
  }
}
