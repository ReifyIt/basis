//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package bson

import basis.memory._
import basis.util._

trait BsonSet extends SetForm { variant: BsonVariant =>
  override type SetForm <: BsonSet with AnyForm

  override val SetForm: BsonSetFactory

  trait BsonSet extends BsonValue with BaseSet { this: SetForm =>
    override def bsonType: Byte = 0x04

    private[this] var bsonLength: Int = -1
    override def bsonSize: Int = {
      if (bsonLength == -1) {
        var size = 4 // document length
        var i = 0
        val values = iterator
        while (!values.isEmpty) {
          val value = values.head
          if (value.isDefined) { // filter undefined values
            size += 1 // type tag
            size += (if (i == 0) 0 else i.toDouble.log10.toInt) + 2 // index cstring
            size += value.bsonSize
            i += 1
          }
          values.step()
        }
        size += 1 // document sentinel
        bsonLength = size
      }
      bsonLength
    }

    override def writeBson(output: Writer): Unit = {
      output.writeUnalignedInt(bsonSize) // document length
      var i = 0
      val values = iterator
      while (!values.isEmpty) {
        val value = values.head
        if (value.isDefined) { // filter undefined values
          output.writeByte(value.bsonType)
          output.writeCString(i.toString) // worst array representation ever
          value.writeBson(output)
          i += 1
        }
        values.step()
      }
      output.writeByte(0) // document sentinel
    }
  }

  trait BsonSetFactory extends BaseSetFactory {
    def readBson(input: Reader): SetForm = input.readBsonArray(Builder())
  }
}
