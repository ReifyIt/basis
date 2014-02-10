//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package json

import basis.text._

trait JsonUndefined extends UndefinedForm { variant: JsonVariant =>
  override type UndefinedForm <: JsonUndefined with AnyForm

  trait JsonUndefined extends JsonValue with BaseUndefined { this: UndefinedForm =>
    override def writeJson(builder: StringBuilder): Unit = builder.append("undefined")

    override def toJson: String = "undefined"
  }
}
