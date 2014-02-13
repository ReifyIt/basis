//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package bson

import basis.memory._

trait BsonDate extends DateForm { variant: BsonVariant =>
  override type DateForm <: BsonDate with AnyForm

  trait BsonDate extends BsonValue with BaseDate { this: DateForm =>
    override def bsonType: Byte = 0x09

    override def bsonSize: Int = 8

    def writeBson(output: Writer): Unit = output.writeUnalignedLong(millis)
  }
}
