//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package json

import basis.text._

trait JsonBinary extends BinaryForm { variant: JsonVariant =>
  override type BinaryForm <: JsonBinary with AnyForm

  trait JsonBinary extends JsonValue with BaseBinary { this: BinaryForm =>
    override def writeJson(builder: StringBuilder): Unit = {
      builder.append('{')
      builder.append("\"$base64\"")
      builder.append(':')
      builder.append('"')
      writeBase64(builder)
      builder.append('"')
      builder.append('}')
    }

    override def toJson: String = {
      val s = UString.Builder()
      writeJson(s)
      s.state.toString
    }
  }
}
