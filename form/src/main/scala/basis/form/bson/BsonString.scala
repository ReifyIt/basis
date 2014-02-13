//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package bson

import basis.memory._

trait BsonString extends StringForm { variant: BsonVariant =>
  override type StringForm <: BsonString with AnyForm

  trait BsonString extends BsonValue with BaseString { this: StringForm =>
    override def bsonType: Byte = 0x02

    private[this] var bsonLength: Int = -1
    override def bsonSize: Int = {
      if (bsonLength == -1) bsonLength = 4 + utf8Length + 1
      bsonLength
    }

    def writeBson(output: Writer): Unit = {
      output.writeUnalignedInt(bsonSize - 4)
      val cs = utf8Iterator
      while (!cs.isEmpty) {
        output.writeByte(cs.head.toByte)
        cs.step()
      }
      output.writeByte(0)
    }
  }
}
