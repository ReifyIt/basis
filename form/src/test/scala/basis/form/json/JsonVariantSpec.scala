//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form
package json

import org.scalatest._
import org.scalatest.matchers._

class JsonVariantSpec
  extends FunSpec
  with VariantTranscoding
  with JsonParserBehaviors
  with JsonInterpolatorBehaviors {

  import ShouldMatchers._

  override val variant: JsonVariant = OmniVariant
  import variant._

  override def suiteName = "JSON variant specification"

  it should behave like Transcodes()

  it should behave like ParsesJsonComments()
  it should behave like ParsesJsonLiterals()
  it should behave like RejectsInvalidJson()

  it should behave like InterpolatesJsonComments()
  it should behave like InterpolatesJsonLiterals()
  it should behave like InterpolatesJsonArguments()

  protected object transcode extends Matcher[AnyForm] {
    private def transcoded(x: AnyForm): AnyForm = AnyForm.parseJson(x.toJson)
    override def apply(x: AnyForm): MatchResult = {
      val y = transcoded(x)
      MatchResult(
        x == y,
        s"$x improperly transcoded to $y",
        s"$x properly transcoded")
    }
  }
}
