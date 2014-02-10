//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package json

import basis.text._

trait JsonNull extends NullForm { variant: JsonVariant =>
  override type NullForm <: JsonNull with AnyForm

  trait JsonNull extends JsonValue with BaseNull { this: NullForm =>
    override def writeJson(builder: StringBuilder): Unit = builder.append("null")

    override def toJson: String = "null"
  }
}
