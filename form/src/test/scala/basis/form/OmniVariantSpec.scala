//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import org.scalatest._

class OmniVariantSpec
  extends FlatSpec
  with JsonVariantBehaviors
  with BsonVariantBehaviors
  with ProtoVariantBehaviors
  with DeltaVariantBehaviors {

  override def suiteName = "OmniVariant specification"

  it should behave like JsonVariantImplementation(OmniVariant)
  it should behave like BsonVariantImplementation(OmniVariant)
  it should behave like ProtoVariantImplementation(OmniVariant)
  it should behave like DeltaVariantImplementation(OmniVariant)
}
