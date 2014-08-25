//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.data._
import org.scalatest._
import org.scalatest.matchers._

trait ProtoVariantBehaviors extends Matchers with VariantTranscoding { this: FlatSpec =>
  def ProtoVariantImplementation(variant: ProtoVariant): Unit = {
    it should behave like TranscodesProtobufs(variant)
  }

  def TranscodesProtobufs(variant: ProtoVariant): Unit = {
    import variant._

    object ProtobufTranscoder extends Matcher[AnyForm] {
      override def apply(x: AnyForm): MatchResult = {
        val framer = FingerTrieDataLE.Framer
        Proto.write(framer, x)
        val data = framer.state
        val y = Proto.read(data.reader(0L))
        val same = x == y
        val xSize = Proto.sizeOf(x)
        val ySize = data.size
        MatchResult(
          same && xSize == ySize,
          if (!same) s"$x improperly transcoded to $y" else s"wrote $ySize bytes, but Proto.sizeOf($x) = $xSize bytes",
          s"$x properly transcoded")
      }
    }

    "Protobuf transcoding" should behave like Transcodes(variant)(ProtobufTranscoder)
  }
}
