//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package bson

import basis.memory._

trait BsonObject extends ObjectForm { variant: BsonVariant =>
  override type ObjectForm <: BsonObject with AnyForm

  override val ObjectForm: BsonObjectFactory

  trait BsonObject extends BsonValue with BaseObject { this: ObjectForm =>
    override def bsonType: Byte = 0x03

    private[this] var bsonLength: Int = -1
    override def bsonSize: Int = {
      if (bsonLength == -1) {
        var size = 4 // document length
        val fields = iterator
        while (!fields.isEmpty) {
          val field = fields.head
          if (field._2.isDefined) { // filter undefined fields
            size += 1 // type tag
            size += field._1.modifiedUTF8Length + 1 // key cstring
            size += field._2.bsonSize
          }
          fields.step()
        }
        size += 1 // document sentinel
        bsonLength = size
      }
      bsonLength
    }

    override def writeBson(output: Writer): Unit = {
      output.writeUnalignedInt(bsonSize) // document length
      val fields = iterator
      while (!fields.isEmpty) {
        val field = fields.head
        if (field._2.isDefined) { // filter undefined fields
          output.writeByte(field._2.bsonType)
          output.writeCString(field._1)
          field._2.writeBson(output)
        }
        fields.step()
      }
      output.writeByte(0) // document sentinel
    }
  }

  trait BsonObjectFactory extends BaseObjectFactory {
    def readBson(input: Reader): ObjectForm = input.readBsonObject(Builder())
  }
}
