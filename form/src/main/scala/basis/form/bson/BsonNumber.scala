//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package bson

import basis.memory._

trait BsonNumber extends NumberForm { variant: BsonVariant =>
  override type NumberForm <: BsonNumber with AnyForm

  trait BsonNumber extends BsonValue with BaseNumber { this: NumberForm =>
    override def bsonType: Byte = {
      if (isValidInt) 0x10 // int32
      else if (isValidLong) 0x12 // int64
      else 0x01 // double
    }

    def bsonSize: Int = bsonType match {
      case 0x01 => 8 // double
      case 0x12 => 8 // int64
      case 0x10 => 4 // int32
    }

    def writeBson(output: Writer): Unit = bsonType match {
      case 0x01 => output.writeUnalignedDouble(toDouble) // double
      case 0x12 => output.writeUnalignedLong(toLong) // int64
      case 0x10 => output.writeUnalignedInt(toInt) // int32
    }
  }
}
