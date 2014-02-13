//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package bson

import basis.memory._

trait BsonBinary extends BinaryForm { variant: BsonVariant =>
  override type BinaryForm <: BsonBinary with AnyForm

  trait BsonBinary extends BsonValue with BaseBinary { this: BinaryForm =>
    override def bsonType: Byte = 0x05

    override def bsonSize: Int = 4 + 1 + size.toInt

    override def writeBson(output: Writer): Unit = {
      output.writeUnalignedInt(size.toInt)
      output.writeByte(0x00) // generic subtype
      var i = 0L
      val n = size
      while (i < n) {
        output.writeByte(loadByte(i))
        i += 1L
      }
    }
  }
}
