//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package json

import basis.text._

trait JsonSeq extends SeqForm { variant: JsonVariant =>
  override type SeqForm <: JsonSeq with AnyForm

  override val SeqForm: JsonSeqFactory

  trait JsonSeq extends JsonValue with BaseSeq { this: SeqForm =>
    override def writeJson(builder: StringBuilder): Unit = {
      builder.append('[')
      val values = iterator
      var value = null.asInstanceOf[AnyForm]
      while (!values.isEmpty && { value = values.head; !value.isDefined })
        values.step() // filter leading undefined values
      if (!values.isEmpty) {
        value.writeJson(builder)
        values.step()
        while (!values.isEmpty) {
          value = values.head
          if (value.isDefined) {
            builder.append(',')
            value.writeJson(builder)
          }
          values.step()
        }
      }
      builder.append(']')
    }

    override def toJson: String = {
      val builder = UString.Builder()
      writeJson(builder)
      builder.state.toString
    }
  }

  trait JsonSeqFactory extends BaseSeqFactory {
    def parseJson(json: String): SeqForm = {
      val factory = new JsonVariantFactory[variant.type](variant)
      val parser = new JsonStringParser(json)
      parser.skipWhitespace()
      val value = parser.parseArray(factory)(Builder())
      parser.skipWhitespace()
      parser.parseEOF()
      value
    }
  }
}
