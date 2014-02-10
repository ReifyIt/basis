//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package json

import basis.text._

trait JsonBoolean extends BooleanForm { variant: JsonVariant =>
  override type BooleanForm <: JsonBoolean with AnyForm

  trait JsonBoolean extends JsonValue with BaseBoolean { this: BooleanForm =>
    override def writeJson(builder: StringBuilder): Unit =
      builder.append(if (toBoolean) "true" else "false")

    override def toJson: String = if (toBoolean) "true" else "false"
  }
}
