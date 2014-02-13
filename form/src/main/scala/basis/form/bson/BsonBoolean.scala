//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package bson

import basis.memory._

trait BsonBoolean extends BooleanForm { variant: BsonVariant =>
  override type BooleanForm <: BsonBoolean with AnyForm

  trait BsonBoolean extends BsonValue with BaseBoolean { this: BooleanForm =>
    override def bsonType: Byte = 0x08

    override def bsonSize: Int = 1

    def writeBson(output: Writer): Unit =
      output.writeByte(if (toBoolean) 1 else 0)
  }
}
