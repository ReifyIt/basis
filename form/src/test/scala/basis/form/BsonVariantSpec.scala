//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import org.scalatest._
import org.scalatest.matchers._

class BsonVariantSpec
  extends FunSpec
  with VariantTranscoding {

  override val variant: BsonVariant = OmniVariant
  import variant._

  override def suiteName = "BSON variant specification"

  it should behave like Transcodes()

  protected object transcode extends Matcher[AnyForm] {
    private def transcoded(x: AnyForm): AnyForm = {
      val data = x.toBson
      val input = data.pointer()
      x.bsonType match {
        case 0x01 => input.readBsonDouble()
        case 0x02 => input.readBsonString(BsonStringBuilder())
        case 0x03 => input.readBsonObject(BsonObjectBuilder())
        case 0x04 => input.readBsonArray(BsonArrayBuilder())
        case 0x05 => input.readBsonBinary()
        case 0x06 => input.readBsonUndefined()
        case 0x07 => input.readBsonObjectId()
        case 0x08 => input.readBsonBoolean()
        case 0x09 => input.readBsonDateTime()
        case 0x0A => input.readBsonNull()
        case 0x10 => input.readBsonInt32()
        case 0x12 => input.readBsonInt64()
        case tag  => throw new BsonException("unknown bson type: "+ tag)
      }
    }

    def apply(x: AnyForm): MatchResult = {
      val y = transcoded(x)
      MatchResult(
        x == y,
        s"$x improperly transcoded to $y",
        s"$x properly transcoded")
    }
  }
}
