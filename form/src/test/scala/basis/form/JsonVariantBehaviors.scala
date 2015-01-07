//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import org.scalatest._
import org.scalatest.matchers._

trait JsonVariantBehaviors
  extends Matchers
  with VariantTranscoding
  with JsonParserBehaviors
  with JsonInterpolatorBehaviors { this: FlatSpec =>

  def JsonVariantImplementation(variant: JsonVariant): Unit = {
    it should behave like TranscodesJson(variant)

    it should behave like ParsesJsonComments(variant)
    it should behave like ParsesJsonLiterals(variant)
    it should behave like RejectsInvalidJson(variant)

    it should behave like InterpolatesJsonComments(variant)
    it should behave like InterpolatesJsonLiterals(variant)
    it should behave like InterpolatesJsonArguments(variant)
  }

  def TranscodesJson(variant: JsonVariant): Unit = {
    import variant._

    object JsonTranscoder extends Matcher[AnyForm] {
      private def transcoded(x: AnyForm): AnyForm = AnyForm.parseJson(x.toJson)
      override def apply(x: AnyForm): MatchResult = {
        val y = transcoded(x)
        MatchResult(
          x == y,
          s"$x improperly transcoded to $y",
          s"$x properly transcoded")
      }
    }

    s"$variant JSON transcoding" should behave like TranscodesForms(variant)(JsonTranscoder)
  }
}
