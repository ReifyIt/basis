//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package bson

import basis.memory._

trait BsonValue extends AnyForm { variant: BsonVariant =>
  override type AnyForm <: BsonValue

  override val AnyForm: BsonValueFactory

  trait BsonValue extends BaseValue { this: AnyForm =>
    /** Returns the type code of this form's BSON representation. */
    def bsonType: Byte

    /** Returns the size in bytes of this form's BSON representation. */
    def bsonSize: Int

    /** Writes the serialized BSON representation of this form to `output`. */
    def writeBson(output: Writer): Unit

    /** Returns the serialized BSON representation of this form. */
    def toBson: Data1 = {
      val output = Data1.LE.Framer().expect(bsonSize.toLong)
      writeBson(output)
      output.state
    }
  }

  trait BsonValueFactory extends BaseValueFactory {
    /** Reads a variant from BSON encoded `input`. */
    def readBson(input: Reader): AnyForm = BsonObjectValue(input.readBsonObject(BsonObjectBuilder()))
  }
}
