//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package json

import basis.text._

trait JsonDate extends DateForm { variant: JsonVariant =>
  override type DateForm <: JsonDate with AnyForm

  trait JsonDate extends JsonValue with BaseDate { this: DateForm =>
    override def writeJson(builder: StringBuilder): Unit = {
      builder.append('{')
      builder.append("\"$date\"")
      builder.append(':')
      builder.append(java.lang.Long.toString(millis))
      builder.append('}')
    }

    override def toJson: String = {
      val s = UString.Builder()
      writeJson(s)
      s.state.toString
    }
  }
}
