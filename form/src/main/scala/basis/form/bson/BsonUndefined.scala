//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package bson

import basis.memory._

trait BsonUndefined extends UndefinedForm { variant: BsonVariant =>
  override type UndefinedForm <: BsonUndefined with AnyForm

  trait BsonUndefined extends BsonValue with BaseUndefined { this: UndefinedForm =>
    override def bsonType: Byte = 0x06

    override def bsonSize: Int = 0

    def writeBson(output: Writer): Unit = ()
  }
}
