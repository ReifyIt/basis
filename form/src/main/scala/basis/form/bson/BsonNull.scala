//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package bson

import basis.memory._

trait BsonNull extends NullForm { variant: BsonVariant =>
  override type NullForm <: BsonNull with AnyForm

  trait BsonNull extends BsonValue with BaseNull { this: NullForm =>
    override def bsonType: Byte = 0x0A

    override def bsonSize: Int = 0

    def writeBson(output: Writer): Unit = ()
  }
}
