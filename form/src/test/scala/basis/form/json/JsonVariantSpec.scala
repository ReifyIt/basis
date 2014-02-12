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
  with JsonVariantBehaviors
  with JsonParserBehaviors
  with JsonInterpolatorBehaviors {

  import ShouldMatchers._

  override val variant: JsonVariant = OmniVariant

  override def suiteName = "JSON variant specification"

  it should behave like TranscodesJson()

  it should behave like ParsesJsonComments()
  it should behave like ParsesJsonLiterals()
  it should behave like RejectsInvalidJson()

  it should behave like InterpolatesJsonComments()
  it should behave like InterpolatesJsonLiterals()
  it should behave like InterpolatesJsonArguments()
}
