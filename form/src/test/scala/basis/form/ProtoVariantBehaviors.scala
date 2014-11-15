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
    it should behave like TranscodesEncryptedProtobufs(variant)
    it should behave like TranscodesDeltaProtobufs(variant)
  }

  def TranscodesProtobufs(variant: ProtoVariant): Unit = {
    import variant._

    object ProtobufTranscoder extends Matcher[AnyForm] {
      override def apply(x: AnyForm): MatchResult = {
        val framer = FingerTrieDataLE.Framer
        FormProto.write(framer, x)
        val data = framer.state
        val y = FormProto.read(data.reader(0L))
        val same = x == y
        val xSize = FormProto.sizeOf(x)
        val ySize = data.size
        MatchResult(
          same && xSize == ySize,
          if (!same) s"$x improperly transcoded to $y" else s"wrote $ySize bytes, but FormProto.sizeOf($x) = $xSize bytes",
          s"$x properly transcoded")
      }
    }

    s"$variant protobuf transcoding" should behave like TranscodesForms(variant)(ProtobufTranscoder)
  }

  def TranscodesDeltaProtobufs(variant: ProtoVariant): Unit = {
    import variant._

    object ProtobufTranscoder extends Matcher[AnyForm] {
      override def apply(x: AnyForm): MatchResult = {
        val framer = FingerTrieDataLE.Framer
        DeltaProto.write(framer, x)
        val data = framer.state
        val y = DeltaProto.read(data.reader(0L))
        val same = x == y
        val xSize = DeltaProto.sizeOf(x)
        val ySize = data.size
        MatchResult(
          same && xSize == ySize,
          if (!same) s"$x improperly transcoded to $y" else s"wrote $ySize bytes, but DeltaProto.sizeOf($x) = $xSize bytes",
          s"$x properly transcoded")
      }
    }

    object DeltaProtobufTranscoder extends Matcher[AnyDelta] {
      override def apply(x: AnyDelta): MatchResult = {
        val framer = FingerTrieDataLE.Framer
        DeltaProto.write(framer, x)
        val data = framer.state
        val y = DeltaProto.read(data.reader(0L))
        val same = x == y
        val xSize = DeltaProto.sizeOf(x)
        val ySize = data.size
        MatchResult(
          same && xSize == ySize,
          if (!same) s"$x improperly transcoded to $y" else s"wrote $ySize bytes, but DeltaProto.sizeOf($x) = $xSize bytes",
          s"$x properly transcoded")
      }
    }

    s"$variant delta protobuf transcoding" should behave like TranscodesForms(variant)(ProtobufTranscoder)
    it should behave like TranscodesDeltas(variant)(DeltaProtobufTranscoder)
  }

  def TranscodesEncryptedProtobufs(variant: ProtoVariant): Unit = {
    import variant._

    object EncryptedProtobufTranscoder extends Matcher[AnyForm] {
      private val secretKey = ArrayData.fromBase16("FFEEDDCCBBAA99887766554433221100")
      private val iv = ArrayData.fromBase16("0123456789ABCDEFFEDCBA9876543210")

      override def apply(x: AnyForm): MatchResult = {
        val p = x.encrypt(secretKey, iv)
        val framer = FingerTrieDataLE.Framer
        FormProto.write(framer, p)
        val data = framer.state
        val q = FormProto.read(data.reader(0L))
        val y = q.decrypt(secretKey)
        val same = x == y
        val pSize = FormProto.sizeOf(p)
        val qSize = data.size
        MatchResult(
          same && pSize == qSize,
          if (!same) s"$x improperly transcoded to $y" else s"wrote $qSize bytes, but FormProto.sizeOf($p) = $pSize bytes",
          s"$x properly transcoded")
      }
    }

    s"$variant encrypted protobuf transcoding" should behave like TranscodesForms(variant)(EncryptedProtobufTranscoder)
  }
}
